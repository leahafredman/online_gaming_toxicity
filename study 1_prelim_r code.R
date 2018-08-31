#Preliminary diss study 1

#Setting working directory where my files are and then reading the CSV is as a dataframe named 'data'
setwd("C:/path")
data <- read.csv("filename.csv")
data_all <- read.csv("filename.csv")

library("tidyverse")
library("dplyr")

#Selecting to keep columns of interest
#Using dplyr:: because select is masked from it because I also have MASS loaded
#Force age to become a numeric column and anything non-numeric to be converted into NAs
#Filtering data so that only participants 18+ are kept (i.e. remove those who didn't provide age and likely didn't complete the survey anyways)

data <- data %>%
    dplyr::select(StartDate, IPAddress, gender, starts_with("Serious"), starts_with("Upsetting"), Attn, age, PlayHours, OnlinePlayHours, industry)

data$age <- as.numeric(levels(data$age))[data$age]

data_filtered <- data %>%
    filter(age > 17)

#Number of people 18+
(data_filtered %>%
        summarise(n = n()))

#Removing duplicate IP addresses and counting how many left
data <- data_filtered %>%
    distinct(IPAddress, .keep_all = TRUE)

(data %>%
        summarise(n()))

#Creating a manipulation column by turning columns into numerics and converting NAs to 0s so that later I can add columns to make singe variables
data$Serious1GOGH <- as.numeric(levels(data$Serious1GOGH))[data$Serious1GOGH]
data$Serious2GOGH <- as.numeric(levels(data$Serious2GOGH))[data$Serious2GOGH]
data$Serious3GOGH <- as.numeric(levels(data$Serious3GOGH))[data$Serious3GOGH]
data$Serious4GOGH <- as.numeric(levels(data$Serious4GOGH))[data$Serious4GOGH]
data$Serious1OGSH <- as.numeric(levels(data$Serious1OGSH))[data$Serious1OGSH]
data$Serious2OGSH <- as.numeric(levels(data$Serious2OGSH))[data$Serious2OGSH]
data$Serious3OGSH <- as.numeric(levels(data$Serious3OGSH))[data$Serious3OGSH]
data$Serious4OGSH <- as.numeric(levels(data$Serious4OGSH))[data$Serious4OGSH]
data$UpsettingGOGH1 <- as.numeric(levels(data$UpsettingGOGH1))[data$UpsettingGOGH1]
data$UpsettingGOGH2 <- as.numeric(levels(data$UpsettingGOGH2))[data$UpsettingGOGH2]
data$UpsettingGOGH3 <- as.numeric(levels(data$UpsettingGOGH3))[data$UpsettingGOGH3]
data$UpsettingGOGH4 <- as.numeric(levels(data$UpsettingGOGH4))[data$UpsettingGOGH4]
data$UpsettingOGSHw <- as.numeric(levels(data$UpsettingOGSHw))[data$UpsettingOGSHw]
data$UpsettingOGSHw.1 <- as.numeric(levels(data$UpsettingOGSHw.1))[data$UpsettingOGSHw.1]
data$UpsettingOGSHw.2 <- as.numeric(levels(data$UpsettingOGSHw.2))[data$UpsettingOGSHw.2]
data$UpsettingOGSHw.3 <- as.numeric(levels(data$UpsettingOGSHw.3))[data$UpsettingOGSHw.3]

data$Serious1GOGH[is.na(data$Serious1GOGH)] <- 0
data$Serious2GOGH[is.na(data$Serious2GOGH)] <- 0
data$Serious3GOGH[is.na(data$Serious3GOGH)] <- 0
data$Serious4GOGH[is.na(data$Serious4GOGH)] <- 0
data$Serious1OGSH[is.na(data$Serious1OGSH)] <- 0
data$Serious2OGSH[is.na(data$Serious2OGSH)] <- 0
data$Serious3OGSH[is.na(data$Serious3OGSH)] <- 0
data$Serious4OGSH[is.na(data$Serious4OGSH)] <- 0
data$UpsettingGOGH1[is.na(data$UpsettingGOGH1)] <- 0
data$UpsettingGOGH2[is.na(data$UpsettingGOGH2)] <- 0
data$UpsettingGOGH3[is.na(data$UpsettingGOGH3)] <- 0
data$UpsettingGOGH4[is.na(data$UpsettingGOGH4)] <- 0
data$UpsettingOGSHw[is.na(data$UpsettingOGSHw)] <- 0
data$UpsettingOGSHw.1[is.na(data$UpsettingOGSHw.1)] <- 0
data$UpsettingOGSHw.2[is.na(data$UpsettingOGSHw.2)] <- 0
data$UpsettingOGSHw.3[is.na(data$UpsettingOGSHw.3)] <- 0

#Sexual condition = 0, general = 1
data$manipulation <- ifelse(data$Serious1GOGH > 0, 1, 0)

#Attention check
#1 = passed attention check, 0 = failed
data$attn_check <- ifelse(data$Attn == 1 & data$manipulation == 1 | data$Attn == 2 & data$manipulation == 0 & data$gender == 1 | data$Attn == 3 & data$manipulation == 0 & data$gender == 0, 1, 0)

#Subsetting only participants that passed the attention check
data <- data %>%
    filter(attn_check == 1) %>%
    filter(industry == 1)

#Number of people not in industry that passed attention check
(data %>%
        summarise(n = n()))

#making gender numeric:
data$gender <- as.numeric(levels(data$gender))[data$gender]

data$Upset1 <- data$UpsettingGOGH1 + data$UpsettingOGSHw
data$Upset2 <- data$UpsettingGOGH2 + data$UpsettingOGSHw.1
data$Upset3 <- data$UpsettingGOGH3 + data$UpsettingOGSHw.2
data$Upset4 <- data$UpsettingGOGH4 + data$UpsettingOGSHw.3
data$Serious1 <- data$Serious1GOGH + data$Serious1OGSH
data$Serious2 <- data$Serious2GOGH + data$Serious2OGSH
data$Serious3 <- data$Serious3GOGH + data$Serious3OGSH
data$Serious4 <- data$Serious4GOGH + data$Serious4OGSH

#Computing alphas for the Serious and Upset scales:
library(psych)
alpha(subset(data, select=c(Serious1, Serious2, Serious3, Serious4)))
alpha(subset(data, select=c(Upset1, Upset2, Upset3, Upset4)))

#creating Upset and Serious variables:
data <- data %>%
    mutate(Serious = rowMeans(.[,c("Serious1", "Serious2", "Serious3", "Serious4")], na.rm = TRUE)) %>%
    mutate(Upset = rowMeans(.[,c("Upset1", "Upset2", "Upset3", "Upset4")], na.rm = TRUE)) %>%
    dplyr::select(gender, Serious, Upset, age, OnlinePlayHours, manipulation)

View(data)

#Some summary stats
(data %>%
    summarise(mean_age = mean(age), sd_age = sd(age), n = n()))
#Some summary stats by gender
#Uses the tideverse package
#Think of %>% as "then". Here it takes the data dateframe, then groups by gender, and then gets the means and sds of Upset and Serious, and the number of participants in each of those groups
(data_grouped <- data %>% 
    group_by(gender) %>%
    summarise(mean_upset = mean(Upset), sd_upset = sd(Upset), mean_serious = mean(Serious), sd_serious = sd(Serious), n = n()))

(outcome_means <- data.no.na.anova2.checks %>%
        group_by(gender.f, manipulation.f) %>%
        summarise(mean(Serious), sd(Serious), mean(Upset), sd(Upset), n = n()))

library("MASS")
library(visreg)

#Labeling factor variables as factors and then using effect coding which is best from ANOVA (compared to, say, dummy coding)
#For more info on effect coding see https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-effect-coding/
data$manipulation.f <- as.factor(data$manipulation)
data$gender.f <- as.factor(data$gender)
data$OnlinePlay.f <- as.factor(data$OnlinePlay)
contrasts(data$manipulation.f) <- contr.sum
contrasts(data$gender.f) <- contr.sum
data$OnlinePlayHours <- as.numeric(levels(data$OnlinePlayHours))[data$OnlinePlayHours]
data$OnlinePlayHours[is.na(data$OnlinePlayHours)] <- 0

# Gender 0 = Men
# Gender 1 = Women
# Manipulation 0 = Sexual toxicity
# Manipulation 1 = General toxicity


data.no.na.anova1.checks <- data %>%
    dplyr::select(gender.f, Serious, Upset, age, OnlinePlayHours, manipulation.f) %>%
    na.omit(data.no.na.anova1.checks)

summary(anova1<-lm(Upset~manipulation.f*gender.f+OnlinePlayHours+age, data=data.no.na.anova1.checks))
library(car)
Anova(anova1, type="3")
#OLS Checks:
library("car")
library("effects")
outlierTest(anova1)
data.no.na.anova1.checks$resid.anova1 <- anova1$residuals
qqPlot(data.no.na.anova1.checks$resid.anova1) 
shapiro.test(data.no.na.anova1.checks$resid.anova1)
#tests autocorrelation
durbinWatsonTest(anova1)
data.no.na.anova1.checks$datanewvar.anova1<-anova1$fitted.values 
data.anova.ols.checks.ordered <- data.no.na.anova1.checks[order(data.no.na.anova1.checks$datanewvar.anova1), ] 
acf(data.anova.ols.checks.ordered$resid.anova1)
library(lmtest)
#Testing heteroskedasticity
bptest(anova1)

summary(anova1<-lm((Upset^3)~manipulation.f*gender.f+OnlinePlayHours+age, data=data.no.na.anova1.checks))
library(car)
Anova(anova1, type="3")
#OLS Checks:
library("car")
library("effects")
outlierTest(anova1)
data.no.na.anova1.checks$resid.anova1 <- anova1$residuals
qqPlot(data.no.na.anova1.checks$resid.anova1) 
shapiro.test(data.no.na.anova1.checks$resid.anova1)
#tests autocorrelation
durbinWatsonTest(anova1)
data.no.na.anova1.checks$datanewvar.anova1<-anova1$fitted.values 
data.anova.ols.checks.ordered <- data.no.na.anova1.checks[order(data.no.na.anova1.checks$datanewvar.anova1), ] 
acf(data.anova.ols.checks.ordered$resid.anova1)
library(lmtest)
#Testing heteroskedasticity
bptest(anova1)
plot(allEffects(anova1))

data.no.na.anova1$Upset3 <- data.no.na.anova1$Upset^3
sumreg1 <- summary(reg1<-rlm((Upset^3)~manipulation.f*gender.f+OnlinePlayHours+age, data=data.no.na.anova1.checks))
dd = data.frame(sumreg1$coefficients)
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd

visreg(reg1, "manipulation.f", by="gender.f", overlay=TRUE, partial=FALSE)
visreg(anova1, "gender.f", by="manipulation.f", overlay=TRUE, partial=FALSE)

#Decomposing UPSET interaction:
#Boostrapping function:
means_bootstrap_fun = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] 
    upset_means <- d %>%
        group_by(gender.f, manipulation.f) %>%
        summarise(upset_mean = mean(Upset)) %>%
        as.data.frame
    upset_mean_men_sexual = upset_means[1,3]
    upset_mean_men_general = upset_means[2,3]
    upset_mean_women_sexual = upset_means[3,3]
    upset_mean_women_general = upset_means[4,3]
    booted.paths = c(upset_mean_men_sexual, upset_mean_men_general, upset_mean_women_sexual, upset_mean_women_general)
    return(booted.paths) 
}

library(boot)
means_boot_results = boot(data = data.no.na.anova2.checks,
                          statistic = means_bootstrap_fun, ##name of function we wrote above
                          R = 5000)

upset_mean_men_sexual <- boot.ci(means_boot_results, index = 1,
                                   conf = .95,
                                   type = "all")
upset_mean_men_general <- boot.ci(means_boot_results, index = 2,
                                    conf = .95,
                                    type = "all")
upset_mean_women_sexual <- boot.ci(means_boot_results, index = 3,
                                     conf = .95,
                                     type = "all")
upset_mean_women_general <- boot.ci(means_boot_results, index = 4,
                                      conf = .95,
                                      type = "all")
means_boot_results
upset_mean_men_sexual
upset_mean_men_general
upset_mean_women_sexual
upset_mean_women_general
plot(means_boot_results)


#Investigating genderXmanipulation with SERIOUS outcome
data.no.na.anova2.checks <- data %>%
    dplyr::select(gender.f, Serious, Upset, age, OnlinePlayHours, manipulation.f) %>%
    na.omit(data.no.na.anova2.checks)

summary(anova2<-lm((Serious)~manipulation.f*gender.f+OnlinePlayHours+age, data=data.no.na.anova2.checks))
Anova(anova2, type="3")
#OLS Checks:
library("car")
library("effects")
outlierTest(anova2)
data.no.na.anova2.checks$resid.anova2 <- anova2$residuals
qqPlot(data.no.na.anova2.checks$resid.anova2) 
shapiro.test(data.no.na.anova2.checks$resid.anova2)
#tests autocorrelation
durbinWatsonTest(anova2)
data.no.na.anova2.checks$datanewvar.anova2 <- anova2$fitted.values 
data.anova.ols.checks.ordered <- data.no.na.anova2.checks[order(data.no.na.anova2.checks$datanewvar.anova2), ]
acf(data.anova.ols.checks.ordered$resid.anova2)
bptest(anova2)

summary(anova2<-lm((Serious^3)~manipulation.f*gender.f+OnlinePlayHours+age, data=data.no.na.anova2.checks))
Anova(anova2, type="3")
#OLS Checks:
library("car")
library("effects")
outlierTest(anova2)
data.no.na.anova2.checks$resid.anova2 <- anova2$residuals
qqPlot(data.no.na.anova2.checks$resid.anova2) 
shapiro.test(data.no.na.anova2.checks$resid.anova2)
#tests autocorrelation
durbinWatsonTest(anova2)
data.no.na.anova2.checks$datanewvar.anova2 <-anova2$fitted.values 
data.anova.ols.checks.ordered <- data.no.na.anova2.checks[order(data.no.na.anova2.checks$datanewvar.anova2), ] 
acf(data.anova.ols.checks.ordered$resid.anova2)
bptest(anova2)

sumreg1 <- summary(reg1<-rlm((Serious^3)~manipulation.f*gender.f+OnlinePlayHours+age, data=data.no.na.anova2.checks))
dd = data.frame(sumreg1$coefficients)
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd
visreg(anova2, "manipulation.f", by="gender.f", overlay=TRUE, partial=FALSE)
visreg(anova2, "gender.f", by="manipulation.f", overlay=TRUE, partial=FALSE)

#Decomposing SERIOUS interaction:
#Boostrapping function:
means_bootstrap_fun = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] 
    serious_means <- d %>%
        group_by(gender.f, manipulation.f) %>%
        summarise(serious_mean = mean(Serious)) %>%
        as.data.frame
    serious_mean_men_sexual = serious_means[1,3]
    serious_mean_men_general = serious_means[2,3]
    serious_mean_women_sexual = serious_means[3,3]
    serious_mean_women_general = serious_means[4,3]
    booted.paths = c(serious_mean_men_sexual, serious_mean_men_general, serious_mean_women_sexual, serious_mean_women_general)
    return(booted.paths) 
}

library(boot)
means_boot_results = boot(data = data.no.na.anova2.checks,
                   statistic = means_bootstrap_fun, ##name of function we wrote above
                   R = 5000)

serious_mean_men_sexual <- boot.ci(means_boot_results, index = 1,
                       conf = .95,
                       type = "all")
serious_mean_men_general <- boot.ci(means_boot_results, index = 2,
                                   conf = .95,
                                   type = "all")
serious_mean_women_sexual <- boot.ci(means_boot_results, index = 3,
                                   conf = .95,
                                   type = "all")
serious_mean_women_general <- boot.ci(means_boot_results, index = 4,
                                   conf = .95,
                                   type = "all")
means_boot_results
serious_mean_men_sexual
serious_mean_men_general
serious_mean_women_sexual
serious_mean_women_general
plot(means_boot_results)



#bootstrapping anova model for UPSET
#The ANOVA model I want bootstrapped:
model_bootstrap = (Upset~Manipulation*Gender+OnlinePlay+age)
#The coefficient of interest in that model:
stat_bootstrap = 6
#Boostrapping function:
anova_int_beta = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] 
    anova = lm(model_bootstrap, data = d) 
    int_coef = summary(anova)$coefficients[stat_bootstrap]
    booted.paths = c(int_coef)
    return(booted.paths) 
}

library(boot)
bootresults = boot(data = data,
                   statistic = anova_int_beta, ##name of function we wrote above
                   R = 50000)

bootintbeta <- boot.ci(bootresults,
                       conf = .95,
                       type = "all")
bootresults
bootintbeta
plot(bootintbeta)

#bootstrapping anova model for SERIOUS
#The ANOVA model I want bootstrapped:
model_bootstrap = (Serious~Manipulation*Gender+OnlinePlay+age)
#The coefficient of interest in that model:
stat_bootstrap = 6
#Boostrapping function:
anova_int_beta = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] 
    anova = lm(model_bootstrap, data = d) 
    int_coef = summary(anova)$coefficients[stat_bootstrap]
    booted.paths = c(int_coef)
    return(booted.paths) 
}

library(boot)
bootresults = boot(data = data,
                   statistic = anova_int_beta, ##name of function we wrote above
                   R = 50000)

bootintbeta <- boot.ci(bootresults,
                       conf = .95,
                       type = "all")
bootresults
bootintbeta
plot(bootintbeta)