

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

ecomony = read_csv("../Traders/data.csv",
                   col_names = TRUE, col_types = cols(honestidade = col_number(),
                                                      sucesso = col_integer(),
                                                      memoria = col_integer(),
                                                      tem_validador = col_character(),
                                                      validador_n_eh_conhecido = col_integer()))

# Remove jobs idênticos no mesmo build
ecomony_2 = ecomony %>% 
  filter(honestidade <= 0.9) %>%
  group_by(honestidade, tem_validador,validador_n_eh_conhecido) %>% 
  summarise(acertou = (sum(sucesso)/n()),total = n()) 

ecomony_2
ggplot(data=ecomony_2, aes(y=acertou*100,x=honestidade*100,ymax = (acertou*97.5), ymin = if_else(acertou*102.5+2.5<100,acertou*102.5+2.5,100),  fill=if_else(tem_validador==0,"Não","Sim")))  +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=paste0(round(acertou*100, 1), "")    ), vjust=-0.1, color="black", size=3.5)+
  geom_errorbar() +
  labs(x='Taxa de honestidade (%)',  
       y="Percentual de sucesso da população (%)", 
       #title="Comparativo de Correlação entre Honestidade e Sucesso ", 
       #subtitle="(Sucesso da População, Taxa de Honestidade na População)",
       fill="Com validação") +
  theme_minimal()+
  theme(plot.title = element_text(face="bold",size = "15"),
        plot.subtitle = element_text(size = "10"),
        plot.caption = element_text(size="10"),
        axis.title.y = element_text(size="12"),
        axis.text.x = element_text(size="10"),
        axis.text.y = element_text(size="12"),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_fill_grey()


#data = ecomony %>% 
#  filter(honestidade <= 0.9)
#data %>% tibble() %>%
#  group_by(honestidade, tem_validador) %>% 
#  summarise(acertou = (sum(sucesso)/n())*100,total = n()) 

#experimento_cobertura = cobertura(data, 
#                                  sample_size = 10, 
#                                  experiments = 10)


#experimento_cobertura

#cis_com_cobertura = experimento_cobertura %>% 
#  mutate(acertou = mean(data) <= upper & mean(data) >= lower)
#cis_com_cobertura %>% 
#  ggplot(aes(x = honestidade, ymax = upper, ymin = lower, color = tem_validador)) + 
#  geom_hline(yintercept = mean(data)) + 
#  geom_errorbar() + 
#  facet_grid(. ~ type)