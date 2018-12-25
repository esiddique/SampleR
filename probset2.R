getwd()
setwd("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ 174/")
library(haven)
ps2 <- read_dta("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ 174/ps2_data.dta")
write.csv(ps2, file = "ps2.csv")
ps2 <- read.csv("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ 174/ps2.csv")
ps2

#2b
#I created the rank_norm variabel by subtracting the mean of rank from every value of rank
rank_norm <- ps2$rank - 6.5

#I used the rdd package to plot the RD, which provides separate regression lines below and above the threshold

install.packages('rdd')
library(rdd)
reg1 <- RDestimate(ps2$kalahimunicipality ~ rank_norm, cutpoint=0)
plot(reg1)
title(xlab = "Poverty ranking for grouped areas (normalized at 0)", ylab = "Prop of areas in anti-poverty program")

#2c
reg2 <- RDestimate(ps2$eninit_cas ~ rank_norm, cutpoint=0)
plot(reg2)
title(xlab = "Poverty ranking for grouped areas (normalized at 0)", ylab = "Causalties from insurgent-initiated incidents in areas")

reg3 <- RDestimate(ps2$totcas ~ rank_norm, cutpoint=0)
plot(reg3)
title(xlab = "Poverty ranking for grouped areas (normalized at 0)", ylab = "Total causalties from all incidents in areas")

#2e
#Here, I created assign 0 and 1 to control and treatment respectively
ps2$treatment <- 0
ps2[ps2$rank < 6.5,]$treatment <- 1

#To calculate with robust SE, I will need a few additional packages
install.packages('lmtest')
library(lmtest)
install.packages('sandwich')
library(sandwich)

#The following methods find robust SE
#This is a linear regression with no interactions (not allowing slope to vary)
ps2$rank_norm <- rank_norm
reg4 <- lm(totcas ~ treatment+rank_norm, data=ps2)
reg4adjust <- coeftest(reg4, vcov = vcovHC(reg4, type="HC1"))

#This is a linear regression with interaction terms (slope allowed to vary)
reg5 <- lm(totcas ~ treatment+rank_norm+treatment*rank_norm, data=ps2)
reg5adjust <- coeftest(reg5, vcov = vcovHC(reg5, type="HC1"))

#The stargazer function creates a regression table

install.packages('stargazer')
library(stargazer)

stargazer(reg4adjust, reg5adjust, type="html",
          dep.var.labels=c("Total Casualties from All Incidents in Areas"),
          covariate.labels = c('Received development assistance', 'Poverty ranking for Grouped Areas (normalized)', 
                               'Received development assistance*Poverty ranking for Grouped Areas (normalized)'), 
          out="ps2tablea1.htm") 

#2g
#This is a quadratic regression without interaction terms
rank_norm_sq <- rank_norm^2
ps2$rank_norm_sq <- rank_norm_sq
reg6 <- lm(totcas ~ treatment+rank_norm+rank_norm_sq, data=ps2)
reg6adjust <- coeftest(reg6, vcov = vcovHC(reg6, type="HC1"))

#This is a quadratic regression with interaction terms
reg7 <- lm(totcas ~ treatment + rank_norm + rank_norm_sq + (treatment*rank_norm) + (treatment*rank_norm_sq), data=ps2)
reg7adjust <- coeftest(reg7, vcov = vcovHC(reg7, type="HC1"))

stargazer(reg6adjust, reg7adjust, type="html",
          dep.var.labels=c("Total Casualties from All Incidents in Areas"),
          covariate.labels = c('Received development assistance', 'Poverty ranking for Grouped Areas (normalized)', 
                               'Poverty ranking for Grouped Areas (normalized) squared', 'Received development assistance*Poverty ranking for Grouped Areas (normalized)',
                               'Received development assistance*Poverty ranking for Grouped Areas (normalized) squared'), 
          out="ps2table2.htm") 
