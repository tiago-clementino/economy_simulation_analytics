

library(tidyverse)
library(resample) # para bootstrap
library(lubridate)
library(here)
library(scales)
library(latex2exp)

#gráfico de barras para o resultado com CI real

#gráfico de linha da quantidade de transações recusadas por ciclo em fução da confiança (use a confiança por feedback, por web of trust, com e sem classificacao de transações)

#df <-
#  list.files(path = "../economy_simulation/output/", pattern = "timeline*.csv", full.names = TRUE) %>% 
#  lapply(function(x) read_csv(x , 
#                              col_names = TRUE, col_types = cols(
#                                run = col_character(),
#                                hash = col_integer(),
#                                type = col_integer(),
#                                notMatch = col_integer(),
#                                memory = col_integer(),
#                                hasValidator = col_integer(),
#                                typeAgnostic = col_integer(),
#                                securityDeposit = col_integer(),
#                                hasFeedback = col_integer(),
##                                feedbackPercent = col_number(),
#                                honestPercent = col_number(),
#                                stepCount = col_integer(),
#                                totalTransactions = col_integer(),
#                                transactionFail = col_integer(),
##                                AvoidedFailTransactions = col_integer()
#                              ))) %>%                              # Store all files in list
#  bind_rows
#df  


df <-
  list.files(path = "../economy_simulation/output/", pattern = "timeline*.csv", full.names = TRUE) %>% 
  lapply(function(x) read_csv(x)) %>%                              # Store all files in list
  bind_rows
  
colnames(df) <- c("run","hash","type","notMatch","memory","hasValidator","typeAgnostic",
                  "securityDeposit","hasFeedback","feedbackPercent","honestPercent","stepCount",
                  "totalTransactions","transactionFail","AvoidedFailTransactions")

as.data.frame(df)

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
           )
         #day = formatC(day, width = 2, flag = "0"),
         #month = lubridate::ymd(paste0(year, month, day)),
         #month = lubridate::floor_date(month, "month")
         ) %>% 
  ggplot() +
  #geom_count(aes(x = hash, y = full_fail_transactions, group = honestPercent, col = honestidade)) + 
  geom_point(aes(x = hash, y = full_fail_transactions, col = paste(honestidade, "%", sep = ""), alpha = count)) + 
  #scale_x_continuous(breaks = 1:12) + 
  #scale_fill_gradient(low = "red", high = "green", limits = c(-8, 8), labels = percent) +
  facet_wrap(~profile) +
  labs(#x='Taxa de honestidade (%)',  
       #y="Percentual de sucesso da população (%)", 
       #title="Comparativo de Correlação entre Honestidade e Sucesso ", 
       #subtitle="(Sucesso da População, Taxa de Honestidade na População)",
       color="Honestidade")



df %>% 
  mutate(profile = case_when(
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ "C",
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ "D",
           hasValidator == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
           hasValidator == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
           hasValidator == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ "G",
           hasValidator == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ "H",
         )
         #day = formatC(day, width = 2, flag = "0"),
         #month = lubridate::ymd(paste0(year, month, day)),
         #month = lubridate::floor_date(month, "month")
  ) %>% 
  ggplot() +
  geom_line(aes(x = hash, y = AvoidedFailTransactions, group = honestPercent, col = honestPercent)) + 
  #scale_x_continuous(breaks = 1:12) + 
  facet_wrap(~profile)
#gráfico de linha da quantidade de transações desonestas evidatas em todas as configurações de validador

#mencione que desconsiderou o fato do depósito caução não ser eficaz em certos casos e mesmo assim ele se saiu pior
#seria bom provar que o deposito caução não é eficaz em todos os casos

