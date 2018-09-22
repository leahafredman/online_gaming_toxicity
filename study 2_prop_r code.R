#Diss Study 2
#Setting working directory where my files are and then reading the CSV is as a dataframe named 'data.original'
setwd("C:/Users/Leah/Documents/dissertation/dissertation_data/surface")
#This will read the csv in as a tibble instead of a dataframe
#I want to skip rows 2 and 3 in import, but I think I need to skip a certain amount starting at the top. I want to skip them because they're text, and leaving them in turns all the columns to chr datatypes, which is wrong. 
#To keep column names I first import only the first row and save it as the column names, and then use it when importing all the data skipping the first three rows, which would've inclueded the column names
library("tidyverse")
col_names <- names(read_csv("diss 3.csv", n_max = 0))
data <- read_csv("diss 3.csv", col_names = col_names, skip = 3)

#Feature engineering
#Mean Withdrawal, short withdrawal, long withdrawal
#Predictor manipulation variable with which condition participants were in 
#Convert to 0s the NAs in the play hours variable
#Choose only people over 17
#keep only relevant columns
data.clean <- data %>%
           mutate(withdrawal.l = (rowMeans(.[,c("Withdrawal3","Withdrawal4")], na.rm = TRUE))) %>%
    # control condition = 0 ; betray = 1 ; sv = 2 ; future control = 3
        mutate(manipulation = ifelse(!is.na(data$"1212_Click Count"), 3, ifelse(!is.na(data$"Q278_Click Count"), 2, ifelse(!is.na(data$"Q280_Click Count"), 1, ifelse(!is.na(data$"Q282_Click Count"), 0, "NA")))), 
               manipulation_dich = ifelse(manipulation == 0, 0, 1),
               online_hours = ifelse(!is.na(data$online_hours), data$online_hours, 0)) %>%
    filter(age > 17) %>%
    dplyr::select(IPAddress, age, online_hours, starts_with("manip"), starts_with("withdraw", ignore.case = TRUE))

#Number of people 18+
(data.clean %>%
        summarise(n = n()))

#Removing duplicate IP addresses and counting how many left, along with relevant descriptive stats
data <- data.clean %>%
    distinct(IPAddress, .keep_all = TRUE)

(data %>%
        summarise(n(), withdrawal_mean = mean(withdrawal), withdrawal.l_mean = mean(withdrawal.l), withdrawal.l_sd = sd(withdrawal.l)))


library("psy")
library(psych)
#Withdrawal.l alpha
alpha(subset(data, select=c(Withdrawal3,Withdrawal4)))

#The hypothesis is that manipulating control, SV, and betrayal will lead to similar levels of long withdrawal, and that their level of withdrawal will be less compared to that of people in the control condition who didn't receive that manipulation
#Showing that the means of control, SV, and betrayal are the same by bootstrapping the means of each and showing that the CIs overlap
boot_mean_control = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] 
    mean_control = mean(d$withdrawal.l)
    return(mean_control) 
}

library(boot)
bootresults_control = boot(data = (subset(data, manipulation == 3)),
                   statistic = boot_mean_control, ##name of function we wrote above
                   R = 10000)

boot_control <- boot.ci(bootresults_control,
                        conf = .95,
                        type = "all")

boot_mean_sv = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] 
    mean_sv = mean(d$withdrawal.l)
    return(mean_sv) 
}

library(boot)
bootresults_sv = boot(data = (subset(data, manipulation == 2)),
                           statistic = boot_mean_sv, ##name of function we wrote above
                           R = 10000)

boot_sv <- boot.ci(bootresults_sv,
                        conf = .95,
                        type = "all")

boot_mean_betray = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] 
    mean_betray = mean(d$withdrawal.l)
    return(mean_betray) 
}

library(boot)
bootresults_betray = boot(data = (subset(data, manipulation == 1)),
                           statistic = boot_mean_betray, ##name of function we wrote above
                           R = 10000)

boot_betray <- boot.ci(bootresults_betray,
                        conf = .95,
                        type = "all")
bootresults_control
boot_control
bootresults_sv
boot_sv
bootresults_betray
boot_betray

#Since the CIs significantly overlap I'm treating the three manipulations as the same, and coding that as 1, and the control condition as 0. However, there is now a large class imbalance and about 3 times more manipulation long withdrawal scores compared to those in the control condition. To deal with the imbalance I am up-sampling--botstrapping--the control condition, so that its size is the same of the size of the manipulation condition
library("caret")
data$manipulation_dichF <- as.factor(data$manipulation_dich)
data_up <- upSample(data, data$manipulation_dichF)

#fails all assumptions so bootstrapping the manipulation beta to be sure of significance
summary(lm1 <- lm(withdrawal.l ~ manipulation_dich + age + online_hours, data = data_up)) 
library("car")
library("effects")
outlierTest(reg1)
data_up$resid.reg1 <- reg1$residuals
qqPlot(data_up$resid.reg1)   
shapiro.test(data_up$resid.reg1)
#tests autocorrelation
data_up$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- data_up[order(data_up$datanewvar.reg1), ] 
pacf(dataDS1.reg.ols.checks.ordered$resid.reg1)
library(lmtest)
bptest(reg1)
plot(allEffects(reg1))

#bootstrapping the regression's manipulation beta
manip_beta = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] 
    reg1 = lm(withdrawal.l ~ manipulation_dich + age + online_hours, data = d) 
    int_coef = reg1$coefficients[2]
    booted.paths = c(int_coef)
    return(booted.paths) 
}

library(boot)
bootresults = boot(data = data_up,
                   statistic = manip_beta, ##name of function we wrote above
                   R = 10000)

bootintbeta <- boot.ci(bootresults,
                       conf = .95,
                       type = "all")
bootresults
bootintbeta

library(effects)
plot(allEffects(lm1))
