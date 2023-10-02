

library(tidyverse)
library(resample) # para bootstrap
library(lubridate)
library(here)
#library(magrittr)

onerun = function(dado, n) {
  experiment = sample(dado$sucesso, n, replace=TRUE)
  b = bootstrap(experiment, mean, R = 2000)
  ci.from.bca = CI.bca(b, probs = c(.025, .975))
  ci.from.percentile = CI.percentile(b, probs = c(.025, .975))
  return(tibble(type = c("bca", "percentile"),
                lower = c(ci.from.bca[1], ci.from.percentile[1]), 
                upper = c(ci.from.bca[2], ci.from.percentile[2])))
}
cobertura = function(dado, sample_size, experiments = 2000){
  cis = tibble(dado) %>% 
    group_by(honestidade, tem_validador) %>% 
    do(onerun(dado, sample_size))
}

ecomony <-
  list.files(path = "../economy_simulation/output/", pattern = "data*.csv", full.names = TRUE) %>% 
  lapply(function(x) read_csv(x)) %>%                              # Store all files in list
  bind_rows 

colnames(ecomony) <- c("sucesso","memoria","tem_validador","typeAgnostic","securityDeposit","hasFeedback",
                  "feedbackPercent","honestidade","stepCount","totalTransactions","transactionFail",
                  "AvoidedFailTransactions","uniqueValidators","terminate")

as.data.frame(ecomony)

ecomony <- cobertura(ecomony, 
                     sample_size = 200, 
                     experiments = 1000)

ecomony_2 = ecomony %>% 
  mutate(profile = case_when(
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ "C",
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ "D",
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ "G",
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ "H",
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "I",
  ) ,
  profile_count = case_when(
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ 1.4,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ 1.2,
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ 1.0,
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ 0.8,
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ 0.6,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ 0.4,
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ 0.2,
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ 0.0,
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ -0.2,
  ) 
  )%>% 
  filter(honestidade <= 0.9) %>%
  group_by(honestidade, profile,profile_count) %>% 
  summarise(acertou = (sum(sucesso)/n()),profile_count = first(profile_count), total = n()) 

ecomony_3 = ecomony %>% 
  mutate(profile = case_when(
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
  ) ,
  profile_count = case_when(
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ 1.65,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ 0.95,
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ 0.25,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ -0.45,
  ) 
  )%>% 
  filter(honestidade <= 0.9, !is.na(profile)) %>%
  group_by(honestidade, profile, profile_count) %>% 
  summarise(uniq = (mean(uniqueValidators)),total = n()) 

ecomony_3
ggplot(data=ecomony_3, aes(y=uniq,x=honestidade*100,ymax = lower, ymin = upper, fill=profile))  +
  geom_bar(stat="identity", position=position_dodge(),color="white")+
  geom_text(aes(label=format(round(uniq, 1), nsmall = 1),hjust=profile_count    ), vjust=-0.2, color="black", size=3.5)+
  geom_errorbar(position=position_dodge(8.6))  +
  labs(x='Honesty rate (%)',  
       y="Diversity of arbitrators (%)", 
       #title="Comparativo de Correlação entre Honestidade e Sucesso ", 
       #subtitle="(Sucesso da População, Taxa de Honestidade na População)",
       fill="Models") +
  theme_minimal()+
  theme(plot.title = element_text(face="bold",size = "15"),
        plot.subtitle = element_text(size = "10"),
        plot.caption = element_text(size="10"),
        axis.title.y = element_text(size="12"),
        axis.text.x = element_text(size="10"),
        axis.text.y = element_text(size="12"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #panel.background = element_blank(),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits=c(0, 100)) +
  scale_fill_grey()


ecomony_2
ggplot(data=ecomony_2, aes(y=acertou*100,x=honestidade*100,ymax = lower, ymin = upper,  fill=profile))  +
  geom_bar(stat="identity", position=position_dodge(),color="white")+
  geom_text(aes(label=format(round(acertou*100, 1), nsmall = 1) ),  vjust=-0.1, color="black", size=3.5)+
  geom_errorbar(position=position_dodge(8.6))  +
  labs(x='Honesty Rate (%)',  
       y="Population Success (%)", 
       #title="Comparativo de Correlação entre Honestidade e Sucesso ", 
       #subtitle="(Sucesso da População, Taxa de Honestidade na População)",
       fill="Models") +
  theme_minimal()+
  theme(plot.title = element_text(face="bold",size = "15"),
        plot.subtitle = element_text(size = "10"),
        plot.caption = element_text(size="10"),
        axis.title.y = element_text(size="12"),
        axis.text.x = element_text(size="10"),
        axis.text.y = element_text(size="12"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits=c(0, 100)) +
  scale_fill_grey()
