

library(tidyverse)
library(resample) # para bootstrap
library(lubridate)
library(here)
library(scales)
library(latex2exp)
library(ggcorrplot)

library(reshape2)


df <-
  list.files(path = "../economy_simulation/output/", pattern = "timeline*.csv", full.names = TRUE) %>% 
  lapply(function(x) read_csv(x)) %>%                              # Store all files in list
  bind_rows
  
colnames(df) <- c("run","hash","type","notMatch","memory","hasValidator","typeAgnostic",
                  "securityDeposit","hasFeedback","feedbackPercent","honestPercent","stepCount",
                  "totalTransactions","transactionFail","AvoidedFailTransactions")

as.data.frame(df)
#count(df)
df<- df %>% filter(totalTransactions >= 100)
df<-df[sample(nrow(df), 400), ]
df %>% 
  mutate(full_fail_transactions = transactionFail - notMatch,
         sucessTransactions = totalTransactions - (transactionFail - notMatch),
         honestidade = sapply(honestPercent*100, as.factor),
         count=n(),
         profile = case_when(
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ "C",
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ "D",
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ "G",
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ "H",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "I",
           ),
         full_fail_transactions = case_when(
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ full_fail_transactions,
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ full_fail_transactions,
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ full_fail_transactions,
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ full_fail_transactions,
         )
         ) %>%
  ggplot() +
  #geom_count(aes(x = hash, y = full_fail_transactions, group = honestPercent, col = honestidade)) + 
  geom_point(aes(x = hash, y = sucessTransactions, col = paste(honestidade, "%", sep = "")))+#, alpha = honestPercent)) + 
  #scale_x_continuous(breaks = 1:12) + 
  #scale_fill_gradient(low = "red", high = "green", limits = c(-8, 8), labels = percent) +
  facet_wrap(~profile) +
  labs(y='Successful Transactions',  
       x="Time (steps)", 
       #title="Comparativo de Correlação entre Honestidade e Sucesso ", 
       #subtitle="(Sucesso da População, Taxa de Honestidade na População)",
       color="Honesty")+
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
        panel.border=element_blank())


df %>% 
  mutate(full_fail_transactions = transactionFail - notMatch,
         sucessTransactions = totalTransactions - (transactionFail - notMatch),
         honestidade = sapply(honestPercent*100, as.factor),
         count=n(),
         profile = case_when(
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ "C",
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ "D",
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ "G",
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ "H",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "I",
         ),
         full_fail_transactions = case_when(
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ full_fail_transactions,
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ full_fail_transactions,
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ full_fail_transactions,
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ full_fail_transactions,
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ full_fail_transactions,
         )
  ) %>%
  ggplot() +
  #geom_count(aes(x = hash, y = full_fail_transactions, group = honestPercent, col = honestidade)) + 
  geom_point(aes(x = hash, y = full_fail_transactions, col = paste(honestidade, "%", sep = "")))+#, alpha = honestPercent)) + 
  #scale_x_continuous(breaks = 1:12) + 
  #scale_fill_gradient(low = "red", high = "green", limits = c(-8, 8), labels = percent) +
  facet_wrap(~profile) +
  labs(y='Fail Transactions',  
       x="Time (steps)", 
       #title="Comparativo de Correlação entre Honestidade e Sucesso ", 
       #subtitle="(Sucesso da População, Taxa de Honestidade na População)",
       color="Honesty")+
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
        panel.border=element_blank())


df %>% 
  mutate(full_fail_transactions = transactionFail - notMatch,
         honestidade = sapply(honestPercent*100, as.factor),
         count=n(),
         profile = case_when(
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ "C",
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ "D",
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ "G",
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ "H",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "I",
         ),
         AvoidedFailTransactions = case_when(
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ AvoidedFailTransactions,
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ AvoidedFailTransactions,#2
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ AvoidedFailTransactions,#10
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ AvoidedFailTransactions,#10
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ AvoidedFailTransactions,
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ AvoidedFailTransactions,
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ AvoidedFailTransactions,#10
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ AvoidedFailTransactions,#10
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ AvoidedFailTransactions,#10
         )
  ) %>%
  filter(hash <= 400) %>%
  ggplot() +
  #geom_line(aes(x = hash, y = AvoidedFailTransactions, group = honestPercent, col = honestPercent)) + 
  geom_point(aes(x = hash, y = AvoidedFailTransactions, col = paste(honestidade, "%", sep = "")))+#, alpha = honestPercent)) + 
  #scale_x_continuous(breaks = 1:12) + 
  facet_wrap(~profile) +
  labs(y='Avoided Unsuccessful Transactions',  
       x="Time (steps)", 
    #title="Comparativo de Correlação entre Honestidade e Sucesso ", 
    #subtitle="(Sucesso da População, Taxa de Honestidade na População)",
    color="Honesty")+
  theme(plot.title = element_text(face="bold",size = "15"),
        plot.subtitle = element_text(size = "10"),
        plot.caption = element_text(size="10"),
        axis.title.y = element_text(size="12"),
        axis.text.x = element_text(size="10"),
        axis.text.y = element_text(size="12"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #panel.background = element_blank(),
        #legend.position = "bottom",
        panel.border=element_blank())
#quantidade de transações desonestas evidatas em todas as configurações de validador

#mencione que desconsiderou o fato do depósito caução não ser eficaz em certos casos e mesmo assim ele se saiu pior


# R program to illustrate
# Spearman Correlation Testing

# Converte para binario
df_Binario <- df %>% mutate(full_fail_transactions = transactionFail - notMatch,
                            sucessTransactions = totalTransactions - (transactionFail - notMatch),
                            profile = case_when(
                              hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
                              hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
                              hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ "C",
                              hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ "D",
                              hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
                              hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
                              hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ "G",
                              hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ "H",
                              hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "I",
                            ),)%>%
  filter(honestPercent == 0.4,sucessTransactions >= 0) %>%
  select(-run,-hash,-type,-notMatch,-memory,-hasValidator,-typeAgnostic,
         -securityDeposit,-hasFeedback,-feedbackPercent,-honestPercent,
         -stepCount,-totalTransactions,-transactionFail,) #%>%
  #binarize(
  #  n_bins = 4, 
   # thresh_infreq = 0.01
 # )
df_Binario

# Exibe grafico
df_Binario %>%
  group_by(profile) %>% 
  select_if(
    is.double
  ) %>% 
  cor() %>% 
  cbind(df_Binario$profile) %>%
  ggcorrplot(hc.order = TRUE, 
             outline.col = "white",
             lab = TRUE ) + 
  facet_wrap(~profile)

df_Binario %>% 
  ggcorrplot( ) + 
  facet_wrap(~profile)



get_lower_tri<-function(x){
  x[upper.tri(x)] <- NA
  return(x)
}

df_Binario_2 <- df_Binario

colnames(df_Binario_2)[colnames(df_Binario_2) == "AvoidedFailTransactions"] ="Avoided"
colnames(df_Binario_2)[colnames(df_Binario_2) == "sucessTransactions"] ="Successful"
colnames(df_Binario_2)[colnames(df_Binario_2) == "full_fail_transactions"] ="Fail"

df2 <- do.call(rbind, lapply(split(df_Binario_2[,1:3], df_Binario_2$profile), 
                             function(x) melt(get_lower_tri(round(cor(x[1:3], method = 'spearman'), 3)),na.rm = FALSE)))

type <- data.frame(profile=c("A","B","C","D","E","F","G","H","I"))
type <- arrange(rbind(type,type,type,type,type,type,type,type,type),profile)

my_cors <- cbind(type,df2)

my_cors %>% 
  filter(  !is.na(value)) %>%
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 3)), color="black", size=3) +
  scale_fill_gradient2(low = 'coral3', 
                       high = 'green3', 
                       mid = 'white', 
                       midpoint = 0,
                       limit = c(-1, 1), 
                       space = "Lab", 
                       name = 'Spearman\nCorrelation') + 
  theme_grey()+
  coord_fixed() +
  theme(plot.title = element_text(face="bold",size = "15"),
        plot.subtitle = element_text(size = "10"),
        plot.caption = element_text(size="10"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size="10",angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size="12"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border=element_blank())+
  facet_wrap("profile",ncol = 5, nrow = 2)
