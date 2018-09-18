#Prelim diss study 3

#Setting working directory where my files are and then reading the CSV is as a dataframe named 'data.original'
setwd("C:/path")
data <- read.csv("filename.csv")

data$ToxType_1 <- data[,1]

#for the na.replace function to convert NAs into the specified number (here it is 0)
library(imputeTS)
#To help chain funtions in a neat, piped way, connecting them with the %>% symbol
library(tidyverse)

#Engineering the ToxType feature by taking each variable the represents whether the participant selected that type of harassment and representing it as a 1, 2, 3, or 4 (depending on the harassment type), converting it to numeric, replacing the NAs with 0s, and then adding those into a single column
data <- data %>% 
    mutate(ToxType = (ToxType1 = na.replace(as.numeric(data$ToxType_1), 0)) + (ToxType2 = na.replace(as.numeric(data$ToxType_2*2), 0)) + (ToxType3 = na.replace(as.numeric(data$ToxType_3*3), 0)) + (ToxType4 = na.replace(as.numeric(data$ToxType_4*4), 0)))
#Dichotomizing ToxType. In retrospect, I could have probably done it neater in the code above, but it isn't worth my time right now to redo this
data <- data %>% mutate(ToxType_GenSex = as.factor(ifelse(data$ToxType <= 2, 1, (ifelse(data$ToxType >= 4, 2, ("NA"))))))

#Feature engineering the outcomes
#Whether lighter/more severe withdrawal severity occured following sexual/general toxicity
#Only keeping data from participants who don't work in gaming
data.noind <- data %>% 
    mutate(withdraw_sex_medium_binom = ifelse(withdraw_sex_medium == 2, 1, 0),
           withdraw_sex_severe_binom = ifelse(withdraw_sex_severe == 2, 1, 0),
           withdraw_gen_medium_binom = ifelse(withdraw_gen_medium == 2, 1, 0),
           withdraw_gen_severe_binom = ifelse(withdraw_gen_severe == 2, 1, 0),
           ToxType_OneTwo = as.factor(ifelse(data$ToxType <= 4, 1, (ifelse(data$ToxType = 4, 2, ("NA")))))) %>%
    filter(industry != 23)
           
#Getting a count of how many people responded no (1) and how many responded yes (2) for mild SEX withdrawal "When experiencing SEXUAL harassment, have you ever QUIT a match/game in the middle because of it?"
(n_mild_withdraw_sex <- data.noind %>% 
        group_by(withdraw_sex_medium) %>%
        summarise(n = n()))
#Same by severe sex:
(n_severe_withdraw_sex <- data.noind %>% 
        group_by(withdraw_sex_severe) %>%
        summarise(n = n()))
#Same by mild general:
(n_mild_withdraw_gen <- data.noind %>% 
        group_by(withdraw_gen_medium) %>%
        summarise(n = n()))
#Same by severe general:
(n_severe_withdraw_sex <- data.noind %>% 
        group_by(withdraw_gen_severe) %>%
        summarise(n = n()))


#chi-square test (2 ways) of whether sexual toxicity led to mild withdrawal
chisq.test((matrix(c(19, 27), 1, 2)), simulate.p.value=TRUE, B=100000)
#chisq.test(c(19, 27))

#chi-square test (2 ways) of whether sexual toxicity led to severe withdrawal
chisq.test((matrix(c(10, 36), 1, 2)), simulate.p.value=TRUE, B=100000)
#chisq.test(c(10, 36))

#chi-square test (2 ways) of whether general toxicity led to mild withdrawal
chisq.test((matrix(c(33, 26), 1, 2)), simulate.p.value=TRUE, B=100000)
#chisq.test(c(33, 26))

#chi-square test (2 ways) of whether general toxicity led to severe withdrawal
chisq.test((matrix(c(23, 36), 1, 2)), simulate.p.value=TRUE, B=100000)
#chisq.test(c(23, 36))
