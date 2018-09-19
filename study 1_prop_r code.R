#Diss study 1

#Setting working directory where my files are and then reading the CSV is as a dataframe named 'data.original'
setwd("C:/path")
#This will read the csv in as a tibble instead of a dataframe
#I want to skip rows 2 and 3 in import, but I think I need to skip a certain amount starting at the top. I want to skip them because they're text, and leaving them in turns all the columns to chr datatypes, which is wrong. 
#To keep column names I first import only the first row and save it as the column names, and then use it when importing all the data skipping the first three rows, which would've inclueded the column names
library("tidyverse")
col_names <- names(read_csv("diss 1.csv", n_max = 0))
data <- read_csv("filenam.csv", col_names = col_names, skip = 3)

#Keeping only the variables of interest
#Dropping anyone who didn't identify as male or female
#Didn't do the attention check
#Engineering outcome features with the mean of select variables, and ifelse statements

data.clean <- data %>%
    mutate(control = (rowMeans(.[,c("Control1","Control2","Control3","Control4")], na.rm = TRUE)), 
           betray = (rowMeans(.[,c("Betrayal1","Betrayal2","Betrayal3","Betrayal4")], na.rm = TRUE)),
           svfail = (rowMeans(.[,c("SVFailure1","SVFailure2","SVFailure3","SVFailure4")], na.rm = TRUE)), 
           identity = (rowMeans(.[,c("Betrayal1","Betrayal2","Betrayal3","Betrayal4","SVFailure1","SVFailure2","SVFailure3","SVFailure4")], na.rm = TRUE)),
           withdrawal = (rowMeans(.[,c("Withdrawal1","Withdrawa2","Withdrawa3","Withdrawa4")], na.rm = TRUE)), 
           withdrawal.s = (rowMeans(.[,c("Withdrawal1","Withdrawa2")], na.rm = TRUE)),
           withdrawal.l = (rowMeans(.[,c("Withdrawa3","Withdrawa4")], na.rm = TRUE))) %>%
    #manipulation: Sexual condition = 0, general = 1
    #manipulation.attn: Sexual men = 1, sexual women = 2, general = 3
    mutate(manipulation = ifelse(is.na(data$"sexual_time_Click Count"), 1, 0)) %>%
    mutate(manipulation.attn = ifelse(gender == 0 & manipulation == 0, 1, ifelse(gender == 1 & manipulation == 0, 2, 3)), online_play_hours = online_hours) %>%
    filter(age > 17, !is.na(attn), industry!= 23, gender < 2, !is.na(gender)) %>%
    mutate(attn.check = ifelse(attn == manipulation.attn, 1, 0)) %>%
    dplyr::select(IPAddress, attn.check, industry, age, gender, online_play_hours, manipulation, control, betray, svfail, identity, withdrawal, withdrawal.s, withdrawal.l, starts_with("Control"), starts_with("Betray"), starts_with("SV"), starts_with("Withdraw"))


#Number of people 18+
(data.clean %>%
        summarise(n = n()))

#Removing duplicate IP addresses and counting how many left
data.noind <- data.clean %>%
    distinct(IPAddress, .keep_all = TRUE)

(data.noind %>%
        summarise(n()))

#Removing data from people who failed the attention check and seeing how many are left, gender breakdown, and mean age
data.noind <- data.noind %>%
    filter(attn.check == 1)

(data.noind %>%
        summarise(n = n(), mean(age), sd(age), control_mean = mean(control), control_sd = sd(control), betray_mean = mean(betray), betray_sd = sd(betray), svfail_mean = mean(svfail), svfail_sd = sd(svfail), identity_mean = mean(identity), identity_sd = sd(identity), withdrawal_mean = mean(withdrawal), withdrawal_sd = sd(withdrawal), withdrawal.s_mean = mean(withdrawal.s), withdrawal.s_sd = sd(withdrawal.s), withdrawal.l_mean = mean(withdrawal.l), withdrawal.l_sd = sd(withdrawal.l)))
# men = 0 , women = 1
(data.noind %>%
        group_by(gender) %>%
        summarise(n = n(), control_mean = mean(control), control_sd = sd(control), betray_mean = mean(betray), betray_sd = sd(betray), svfail_mean = mean(svfail), svfail_sd = sd(svfail), identity_mean = mean(identity), identity_sd = sd(identity), withdrawal_mean = mean(withdrawal), withdrawal_sd = sd(withdrawal), withdrawal.s_mean = mean(withdrawal.s), withdrawal.s_sd = sd(withdrawal.s), withdrawal.l_mean = mean(withdrawal.l), withdrawal.l_sd = sd(withdrawal.l)))

library(psych)
#Control alpha
alpha(subset(data.noind, select=c("Control1","Control2","Control3","Control4"), na.rm = TRUE))
#Betray alpha
alpha(subset(data.noind, select=c("Betrayal1","Betrayal2","Betrayal3","Betrayal4"), na.rm = TRUE))
#SV failure alpha
alpha(subset(data.noind, select=c("SVFailure1","SVFailure2","SVFailure3","SVFailure4")))
#Identity alpha
alpha(subset(data.noind, select=c("Betrayal1","Betrayal2","Betrayal3","Betrayal4","SVFailure1","SVFailure2","SVFailure3","SVFailure4")))
#Withdawal alpha
alpha(subset(data.noind, select=c("Withdrawal1","Withdrawa2","Withdrawa3","Withdrawa4")))
#Withdrawal short alpha
alpha(subset(data.noind, select=c("Withdrawal1","Withdrawa2")))
#Withdrawal long alpha
alpha(subset(data.noind, select=c("Withdrawa3","Withdrawa4")))

#Insignificant interaction even with robust to correct for normality violation
library(visreg)
#Running OLS for withdrawal as outcome and genderXmanipulation as predictor along with covariates:
summary(reg1<-lm(withdrawal~manipulation*gender+age+online_play_hours, data = data.noind))
visreg(reg1,  "manipulation", by="gender", overlay=TRUE, partial=FALSE, rug = FALSE)

#OLS Checks: FAIL NORMALITY
library("car")
library("effects")
outlierTest(reg1)
data.noind$resid.reg1 <- reg1$residuals
qqPlot(data.noind$resid.reg1)   
shapiro.test(data.noind$resid.reg1)
#tests autocorrelation
data.noind$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- data.noind[order(data.noind$datanewvar.reg1), ] 
acf(dataDS1.reg.ols.checks.ordered$resid.reg1)
library(lmtest)
bptest(reg1)
plot(allEffects(reg1))

#Robust:
dataDS1.noind <- data.noind
library("MASS")
sumreg1 <- summary(reg1<-rlm(withdrawal~manipulation*gender+age+online_play_hours, data=dataDS1.noind))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd
visreg(reg1,  "manipulation", by="gender", overlay=TRUE, partial=FALSE, rug = FALSE)

#Insignificant interaction even with robust to correct for normality violation
#Running OLS for SHORT withdrawal as outcome and genderXmanipulation as predictor along with covariates:
summary(reg1<-lm(withdrawal.s~manipulation*gender+age+online_play_hours, data = dataDS1.noind))
visreg(reg1,  "manipulation", by="gender", overlay=TRUE, partial=FALSE, rug = FALSE)
#OLS Checks: FAIL NORMALITY
library("car")
library("effects")
outlierTest(reg1)
dataDS1.noind$resid.reg1 <- reg1$residuals
qqPlot(dataDS1.noind$resid.reg1)   
shapiro.test(dataDS1.noind$resid.reg1)
#tests autocorrelation
dataDS1.noind$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- dataDS1.noind[order(dataDS1.noind$datanewvar.reg1), ] 
acf(dataDS1.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))

#Robust:
sumreg1 <- summary(reg1<-rlm(withdrawal.s~manipulation*gender+age+online_play_hours, data=dataDS1.noind))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd
visreg(reg1,  "manipulation", by="gender", overlay=TRUE, partial=FALSE, rug = FALSE)

#Running OLS for LONG withdrawal as outcome and genderXmanipulation as predictor along with covariates:
#Female=1
summary(reg1<-lm(withdrawal.l~manipulation*gender+age+online_play_hours, data = data.noind))
visreg(reg1,  "manipulation", by="gender", overlay=TRUE, partial=FALSE, rug = FALSE)
library("car")
library("effects")
outlierTest(reg1)
dataDS1.noind$resid.reg1 <- reg1$residuals
qqPlot(dataDS1.noind$resid.reg1)   
shapiro.test(dataDS1.noind$resid.reg1)
#tests autocorrelation
dataDS1.noind$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- dataDS1.noind[order(dataDS1.noind$datanewvar.reg1), ] 
acf(dataDS1.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))

#Correcting for normality violation
#Robust:
sumreg1 <- summary(reg1<-rlm(withdrawal.l~manipulation*gender+age+online_play_hours, data=data.noind))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd
visreg(reg1,  "manipulation", by="gender", overlay=TRUE, partial=FALSE, rug = FALSE)

#Decomposing interaction with regressions:
#Does manipulation predict withdrawal for women:
dataf <- data.noind %>%
    filter(gender == 1) %>%
    dplyr::select(withdrawal.l, manipulation, age, online_play_hours, control, svfail, identity, betray)
summary(reg1<-lm(withdrawal.l~manipulation+age+online_play_hours, data=dataf))
outlierTest(reg1)
dataf$resid.reg1 <- reg1$residuals
qqPlot(dataf$resid.reg1)   
shapiro.test(dataf$resid.reg1)
dataf$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- dataf[order(dataf$datanewvar.reg1), ] 
acf(dataDS1.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)

#Correcting for heteroskedasticity
summary(reg1<-lm((withdrawal.l^1.4)~manipulation+age+online_play_hours, data=dataf))
outlierTest(reg1)
dataf$resid.reg1 <- reg1$residuals
qqPlot(dataf$resid.reg1)   
shapiro.test(dataf$resid.reg1)
dataf$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- dataf[order(dataf$datanewvar.reg1), ] 
acf(dataDS1.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)

#Sexual condition = 0, general = 1
#Correcting for normality violationsumreg1 <- summary(reg1<-rlm(withdrawal.l~manipulation+age+online_play_hours, data=dataf))
sumreg1 <- summary(reg1<-rlm((withdrawal.l^1.4)~manipulation+age+online_play_hours, data=dataf))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd

#Does manipulation predict withdrawal for men:
datam <- data.noind %>%
    filter(gender == 0) %>%
    dplyr::select(withdrawal.l, manipulation, age, online_play_hours)
summary(reg1<-lm(withdrawal.l~manipulation+age+online_play_hours, data=datam))
outlierTest(reg1)
datam$resid.reg1 <- reg1$residuals
qqPlot(datam$resid.reg1)   
shapiro.test(datam$resid.reg1)
datam$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- datam[order(datam$datanewvar.reg1), ] 
acf(dataDS1.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))

#Correcting for normality violation
sumreg1 <- summary(reg1<-rlm(withdrawal.l~manipulation+age+online_play_hours, data=datam))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd

#Does gender predict withdrawal for general toxicity:
dataDS1.noind.generaltox <- subset(data.noind, manipulation==1)
summary(reg1<-lm(withdrawal.l~gender+age+online_play_hours, data=dataDS1.noind.generaltox))
outlierTest(reg1)
dataDS1.noind.generaltox$resid.reg1 <- reg1$residuals
qqPlot(dataDS1.noind.generaltox$resid.reg1)   
shapiro.test(dataDS1.noind.generaltox$resid.reg1)
durbinWatsonTest(reg1)
dataDS1.noind.generaltox$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- dataDS1.noind.generaltox[order(dataDS1.noind.generaltox$datanewvar.reg1), ] 
acf(dataDS1.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))

#Correcting for normality violation
sumreg1 <- summary(reg1<-rlm(withdrawal.l~gender+age+online_play_hours, data=dataDS1.noind.generaltox))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd

#Does gender predict withdrawal for sexual toxicity:
dataDS1.noind.sexualtox <- subset(dataDS1.noind, manipulation == 0)
summary(reg1<-lm(withdrawal.l ~ gender+age+online_play_hours, data=dataDS1.noind.sexualtox))
outlierTest(reg1)
dataDS1.noind.sexualtox$resid.reg1 <- reg1$residuals
qqPlot(dataDS1.noind.sexualtox$resid.reg1)   
shapiro.test(dataDS1.noind.sexualtox$resid.reg1)
dataDS1.noind.sexualtox$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- dataDS1.noind.sexualtox[order(dataDS1.noind.sexualtox$datanewvar.reg1), ] 
acf(dataDS1.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)

#Correcting for heteroskedasticity
summary(reg1<-lm((withdrawal.l^2) ~ gender+age+online_play_hours, data=dataDS1.noind.sexualtox))
outlierTest(reg1)
dataDS1.noind.sexualtox$resid.reg1 <- reg1$residuals
qqPlot(dataf$resid.reg1)   
shapiro.test(dataf$resid.reg1)
dataDS1.noind.sexualtox$datanewvar.reg1<-reg1$fitted.values 
dataDS1.reg.ols.checks.ordered <- dataf[order(dataf$datanewvar.reg1), ] 
acf(dataDS1.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)

#Correcting normality violation
sumreg1 <- summary(reg1<-rlm((withdrawal.l^2) ~ gender+age+online_play_hours, data=dataDS1.noind.sexualtox))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd



#Looking at mediation
#Direct effect
data.noind.women = dataf
sumreg1 <- summary(reg1<-rlm((withdrawal.l^1.4) ~ manipulation+age+online_play_hours, data=dataf))

#Total indirect effect for just women SHOULD STILL BE ROBUST:
summary(reg1<-lm(ptsd~svfbetray + control + sexual+general, data=data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
durbinWatsonTest(reg1)
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))

#Control
summary(reg1<-lm(control ~ manipulation +age + online_play_hours, data=data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))

#Insignificant even as robust
#Correcting for normality violation
sumreg1 <- summary(reg1<-rlm(control ~ manipulation +age + online_play_hours, data=data.noind.women))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd

#SV Failure
summary(reg1<-lm(svfail ~ manipulation+age+online_play_hours, data=data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
shapiro.test(data.noind.women$resid.reg1)
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))

#Insignificant even as robust
#Correcting for normality violation 
sumreg1 <- summary(reg1<-rlm(svfail ~  manipulation + age + online_play_hours, data=data.noind.women))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd


#Sexual condition = 0, general = 1
#Betrayal
#BPTEST NOT GOOD LOOK AT TRANSFORMATIONS
summary(reg1<-lm(betray ~ manipulation + age + online_play_hours, data=data.noind.women))
#This transformation works:
summary(reg1<-lm((betray^1.5)~manipulation+age+online_play_hours, data=data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
shapiro.test(data.noind.women$resid.reg1)
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
library(lmtest)
bptest(reg1)
plot(allEffects(reg1))

#Bootstrapped mediational model
#Because the only significant model was betrayal, it's the only one in the mediation model:
indirectsaved = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] ##saved the dataset and told it to randomize by row because the first spot is for the rows. Doesn't make so much sense to randomize by column
    apath = lm(betray ~ manipulation + age + online_play_hours, data = d)
    bpath = lm(withdrawal.l ~ betray + manipulation + age + online_play_hours, data = d) ##Running a and b paths on the boot strapped randomized datasets
    indirecttotal = apath$coefficients[2]*bpath$coefficients[2]
    booted.paths = c(indirecttotal)
    return(booted.paths) ##running and returning the indirect
}

library(boot)
bootresults = boot(data = data.noind.women,
                   statistic = indirectsaved, ##name of function we wrote above
                   R = 10000)

boottotalindirectci <- boot.ci(bootresults, index = 1,
                               conf = .95,
                               type = "all")
bootresults
boottotalindirectci
plot(bootresults, index = 1)

#same mediation but playing around with the mediation library
library("mediate")
med.fit <- lm(betray ~ manipulation + age + online_play_hours, data = data.noind.women)
out.fit <- lm(withdrawal.l ~ betray + manipulation + age + online_play_hours, data = data.noind.women)
covs <- c("age", "online_hours_played")
med.out <- mediate(med.fit, out.fit, sims = 10000, boot = TRUE, boot.ci.type = "bca", mediator = "betray", treat = "manipulation", covariates = covs) 
summary(med.out)

#Exploratory regression models investigating b-paths for control and svfail 
summary(reg1<-lm(withdrawal.l ~ control + manipulation + age + online_play_hours, data = data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
shapiro.test(data.noind.women$resid.reg1)
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
library(lmtest)
bptest(reg1)

summary(reg1<-lm(withdrawal.l ~ svfail + manipulation + age + online_play_hours, data = data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
shapiro.test(data.noind.women$resid.reg1)
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
library(lmtest)
bptest(reg1)

