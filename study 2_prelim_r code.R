#Prelim diss study 2

#Setting working directory where my files are and then reading the CSV is as a dataframe named 'data.original'
setwd("C:/putpathgere")
data.original <- read.csv("filename.csv")

data.original$Control1 <- data.original$ï..Control1
library("Hmisc")
data.original <- subset(data.original, data.original$Gender < 2)
View(data.original)
#data <- subset(data, is.na(data$Exclude))
#Data cleaning
#Dropping first column
data <- data.original[,c(-1)]
data <- subset(data, Attn==1)
View(data)

#Mean Control:
data$control <- rowMeans(data[,c("Control1","Control2")], na.rm=TRUE)

#Mean Betrayal:
data$betray <- rowMeans(data[,c("Betrayal1", "Betrayal2")], na.rm=TRUE)

#Mean SV Failure:
data$svfail <- rowMeans(data[,c("SVFailure1", "SVFailure2")], na.rm=TRUE) 

#Mean Disparate core values: 
data$values <- rowMeans(data[,c("DisparateValues1", "DesparateValues2")], na.rm=TRUE) 

#Mean Identity:
data$identity <- rowMeans(data[,c("DisparateValues1", "DesparateValues2", "SVFailure1", "SVFailure2", "Betrayal1", "Betrayal2")], na.rm=TRUE)

#Mean SV.Betrayal combo factor:
data$svfbetray <- rowMeans(data[,c("SVFailure1", "SVFailure2", "Betrayal1", "Betrayal2")], na.rm=TRUE)

#Mean sexual harassment frequency by intensity:
data$sjoke <- data$FSJoke_15*data$ISJoke_15
data$savatar <- data$FSAvatar_15*data$ISAvatar_15
data$sthreat <- data$FSThreat_15*data$ISThreat_15
data$sfamthreat <- data$FSFamThreat_15*data$ISFamThreat_15
data$sinsults <- data$FSInsults_15*data$ISInsults_15
data$sabilities <- data$FSAbilities_15*data$ISAbilities_15
data$sleave <- data$FSLeave_15*data$ISLeave_15
data$sreport <- data$FSReport_15*data$ISReport_15
data$srequest <- data$FSRequest_15*data$ISRequest_15
data$sswearing <- data$FSSwearing_15*data$ISSwearing_15
data$ssppearance <- data$FSAppearance_15*data$ISAppearance_15
data$sexual <- rowMeans(data[,c("sjoke", "savatar", "sthreat", "sfamthreat", "sinsults", "sabilities", "sleave", "sreport", "srequest", "sswearing", "ssppearance")], na.rm=TRUE) 

#Mean sexual harassment frequency:
data$sexualF <- rowMeans(data[,c("FSJoke_15", "FSAvatar_15", "FSThreat_15", "FSFamThreat_15", "FSInsults_15", "FSAbilities_15", "FSLeave_15", "FSReport_15", "FSRequest_15", "FSSwearing_15", "FSAppearance_15")], na.rm=TRUE) 

#Mean sexual harassment intensity:
data$sexualI <- rowMeans(data[,c("ISJoke_15", "ISAvatar_15", "ISThreat_15", "ISFamThreat_15", "ISInsults_15", "ISAbilities_15", "ISLeave_15", "ISReport_15", "ISRequest_15", "ISSwearing_15", "ISAppearance_15")], na.rm=TRUE) 

#Mean general harassment frequency by intensity:
data$gjoke <- data$FGJoke_15*data$IGJoke_15
data$gavatar <- data$FGAvatar_15*data$IGAvatar_15
data$gthreat <- data$FGThreat_15*data$IGThreat_15
data$gfamthreat <- data$FGFamThreat_15*data$IGFamThreat_15
data$ginsults <- data$FGInsults_15*data$IGInsults_15
data$gabilities <- data$FGAbilities_15*data$IGAbilities_15
data$gleave <- data$FGLeave_15*data$IGLeave_15
data$greport <- data$FGReport_15*data$IGReport_15
data$grequest <- data$FGRequest_15*data$IGRequest_15
data$gswearing <- data$FGSwearing_15*data$IGSSwearing_15
data$gsppearance <- data$FGAppearance_15*data$IGAppearance_15
data$general <- rowMeans(data[,c("gjoke", "gavatar", "gthreat", "gfamthreat", "ginsults", "gabilities", "gleave", "greport", "grequest", "gswearing", "gsppearance")], na.rm=TRUE) 

#Mean general harassment frequency:
data$generalF <- rowMeans(data[,c("FGJoke_15", "FGAvatar_15", "FGThreat_15", "FGFamThreat_15", "FGInsults_15", "FGAbilities_15", "FGLeave_15", "FGReport_15", "FGRequest_15", "FGSwearing_15", "FGAppearance_15")], na.rm=TRUE) 

#Mean general harassment intensity:
data$generalI <- rowMeans(data[,c("IGJoke_15", "IGAvatar_15", "IGThreat_15", "IGFamThreat_15", "IGInsults_15", "IGAbilities_15", "IGLeave_15", "IGReport_15", "IGRequest_15", "IGSSwearing_15", "IGAppearance_15")], na.rm=TRUE) 

#PTSD composite score:
data$ptsd <- rowSums(data[,c("PTSD1","PTSD2","PTSD3","PTSD4","PTSD5","PTSD6","PTSD7","PTSD8","PTSD9","PTSD10","PTSD11","PTSD12","PTSD13","PTSD14","PTSD15","PTSD16","PTSD17")])

data.noind <- subset(data, Industry!=1)
data.ind <- data
data.noind.women <- subset(data.noind, Gender==1)
data.ind.women <- subset(data.ind, Gender==1)
data.noind.men <- subset(data.noind, Gender==0)

datam <- data.noind.men
dataf <- data.noind.women

library("tidyverse")
#Summary of means by gender
(means_by_gender <- data.noind %>%
        group_by(Gender) %>%
        summarise(sexual_tox = mean(sexual), general_tox = mean(general), control = mean(control), identity = mean(identity), mean(svfail), mean(betray), mean(values), ptsd = mean(ptsd)) %>%
        as.data.frame
)
#Summary of sds by gender
(sds_by_gender <- data.noind %>%
        group_by(Gender) %>%
        summarise(sexual_tox = sd(sexual), general_tox = sd(general), control = sd(control), identity = sd(identity), sd(svfail), sd(betray), sd(values), ptsd = sd(ptsd)) %>%
        as.data.frame
)

library("psy")
library(psych)
#Control alpha
alpha(subset(data.noind, select=c(Control1,Control2)))
#Betray alpha
alpha(subset(data.noind, select=c(Betrayal1,Betrayal2)))
#SV failure alpha
alpha(subset(data.noind, select=c(DisparateValues1,DesparateValues2)))
#values alpha
alpha(subset(data.noind, select=c(SVFailure1,SVFailure2)))
#Identity alpha
alpha(subset(data.noind, select=c(SVFailure1,SVFailure2,DisparateValues1,DesparateValues2,Betrayal1,Betrayal2)))
#svfbetray alpha
alpha(subset(data.noind, select=c(SVFailure1,SVFailure2,Betrayal1,Betrayal2)))
#PTSD alpha
alpha(subset(data.noind, select=c(PTSD1,PTSD2,PTSD3,PTSD4,PTSD5,PTSD6,PTSD7,PTSD8,PTSD9,PTSD10,PTSD11,PTSD12,PTSD13,PTSD14,PTSD15,PTSD16,PTSD17)))

#Correlation table with p-values
library("Hmisc")
(corr.test(subset(data.noind, select=c(control,betray,svfail,values,sexual,general,ptsd))))

#sexual data subset
alpha(subset(data.noind, select=c(sjoke, savatar,sthreat,sfamthreat,sinsults,sabilities,sleave,sreport,srequest,sswearing,ssppearance)))
#general data subset
alpha(subset(data.noind, select=c(gjoke, gavatar,gthreat,gfamthreat,ginsults,gabilities,gleave,greport,grequest,gswearing,gsppearance)))

library(HH)

library("MASS")
library(visreg)

#Running OLS for ptsd as outcome and genderXsexual and genderXgeneral as predictors along with covariates:
summary(reg1<-lm(ptsd~sexual*Gender+Gender*general+age+OnlinePlayHours, data = data.noind))
#OLS Checks:
library("car")
library("effects")
outlierTest(reg1)
data.noind$resid.reg1 <- reg1$residuals
qqPlot(data.noind$resid.reg1)   
#tests autocorrelation
durbinWatsonTest(reg1)
data.noind$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind[order(data.noind$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))

#Robust:
sumreg1 <- summary(reg1<-rlm(ptsd~sexual*Gender+Gender*general+age+OnlinePlayHours, data=data.noind))
dd = data.frame(sumreg1$coefficients) 
dd$p.value =  2*pt(abs(dd$t.value), sumreg1$df[2], lower.tail=FALSE)
dd
visreg(reg1,  "general", by="Gender", overlay=TRUE, partial=FALSE, rug = FALSE)
visreg(reg1,  "sexual", by="Gender", overlay=TRUE, partial=FALSE, rug = FALSE)

#Decomposing interaction with correlations:
(corr.test(data.frame(data.noind.women$ptsd, data.noind.women$sexual)))
plot(data.noind.women$ptsd, data.noind.women$sexual)
(corr.test(data.frame(data.noind.men$ptsd, data.noind.men$sexual)))
plot(data.noind.men$ptsd, data.noind.men$sexual)

#Looking at mediation
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

#Betrayal
summary(reg1<-lm(betray~sexual+general, data=data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
durbinWatsonTest(reg1)
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
library(lmtest)
bptest(reg1)
plot(allEffects(reg1))
#SV Failure
summary(reg1<-lm(svfail~sexual+general, data=data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
durbinWatsonTest(reg1)
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))
#Values DOESNT WORK
summary(reg1<-lm(values~sexual+general, data=data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
durbinWatsonTest(reg1)
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
#Control
summary(reg1<-lm(control~sexual+general, data=data.noind.women))
outlierTest(reg1)
data.noind.women$resid.reg1 <- reg1$residuals
qqPlot(data.noind.women$resid.reg1)   
durbinWatsonTest(reg1)
data.noind.women$datanewvar.reg1<-reg1$fitted.values 
data.reg.ols.checks.ordered <- data.noind.women[order(data.noind.women$datanewvar.reg1), ] 
acf(data.reg.ols.checks.ordered$resid.reg1)
bptest(reg1)
plot(allEffects(reg1))

#bootstrapping mediation effect lm model (not much different if you run the total indirect as rlm, which should be expected)
indirectsaved = function (dataset, random) { ##place holder for your dataset name and where you want to randomize
    d = dataset[random, ] ##saved the dataset and told it to randomize by row because the first spot is for the rows. Doesn't make so much sense to randomize by column
    a1path = lm(svfbetray ~ sexual, data = d)
    a2path = lm(control ~ sexual, data = d)
    bpath = lm(ptsd ~ svfbetray + control + sexual + general, data = d) ##Running a and b paths on the boot strapped randomized datasets
    indirectsvfbetray = a1path$coefficients[2]*bpath$coefficients[2]
    indirectcontrol = a2path$coefficients[2]*bpath$coefficients[3]
    indirecttotal = (a1path$coefficients[2]*bpath$coefficients[2]) + (a2path$coefficients[2]*bpath$coefficients[3])
    booted.paths = c(indirectsvfbetray, indirectcontrol, indirecttotal)
    return(booted.paths) ##running and returning the indirect
}

library(boot)
bootresults = boot(data = data.noind.women,
                   statistic = indirectsaved, ##name of function we wrote above
                   R = 5000)

bootsvfbetrayci <- boot.ci(bootresults, index = 1,
                           conf = .95,
                           type = "all")
bootcontrolci <- boot.ci(bootresults, index = 2,
                         conf = .95,
                         type = "all")
boottotalindirectci <- boot.ci(bootresults, index = 3,
                               conf = .95,
                               type = "all")
bootresults
bootsvfbetrayci
bootcontrolci
boottotalindirectci
plot(bootresults, index = 1)
plot(bootresults, index = 2)
plot(bootresults, index = 3)