getwd()
setwd("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ 174/PSET\ 3/")
library(haven)
ps3_1 <- read_dta("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ 174/PSET\ 3/ps3_data.dta")
write.csv(ps3_1, file = "ps3.csv")
ps3 <- read.csv("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ 174/PSET\ 3/ps3.csv")

#2c

install.packages('lmtest')
require(lmtest)
install.packages("multiwayvcov")
library(multiwayvcov)

#Here, I create a new column and generate an interaction term

ps3$interaction <- 0
ps3$interaction <- ps3$time * ps3$treatment

#I use the above mentioned packages to cluster at the province level

ps31 <- lm(index1~time+treatment+interaction, data=ps3)
vcov_ps31 <- cluster.vcov(ps31, ps3$province_id)
ps31out <- coeftest(ps31, vcov_ps31)

ps32 <- lm(index2~time+treatment+interaction, data=ps3)
vcov_ps32 <- cluster.vcov(ps32, ps3$province_id)
ps32out <- coeftest(ps32, vcov_ps32)

ps33 <- lm(index3~time+treatment+interaction, data=ps3)
vcov_ps33 <- cluster.vcov(ps33, ps3$province_id)
ps33out <- coeftest(ps33, vcov_ps33)

ps34 <- lm(index4~time+treatment+interaction, data=ps3)
vcov_ps34 <- cluster.vcov(ps34, ps3$province_id)
ps34out <- coeftest(ps34, vcov_ps34)

ps35 <- lm(index5~time+treatment+interaction, data=ps3)
vcov_ps35 <- cluster.vcov(ps35, ps3$province_id)
ps35out <- coeftest(ps35, vcov_ps35)

install.packages('stargazer')
library(stargazer)

stargazer(ps31out, ps32out, ps33out, ps34out, ps35out, type="html", column.labels = c('Infrastructure index', 'Agricultural services index','Health services index',
                                                                                        'Education index','Communication index'),
          dep.var.labels=c('Index i'),
          covariate.labels = c('Post-reform dummy', 'Village government recentralized dummy', 'Post-reform dummy x Village government recentralized dummy'), out="ps3table1.htm") 

#2e

#To get the province fixed effects, I use the 'factor' function

ps36 <- lm(index1 ~ time + treatment + interaction + factor(province_id), data=ps3)
vcov_ps36 <- cluster.vcov(ps36, ps3$province_id)
ps36out <- coeftest(ps36, vcov_ps36)

ps37 <- lm(index2 ~ time + treatment + interaction + factor(province_id), data=ps3)
vcov_ps37 <- cluster.vcov(ps37, ps3$province_id)
ps37out <- coeftest(ps37, vcov_ps37)

ps38 <- lm(index3 ~ time + treatment + interaction + factor(province_id), data=ps3)
vcov_ps38 <- cluster.vcov(ps38, ps3$province_id)
ps38out <- coeftest(ps38, vcov_ps38)

ps39 <- lm(index4 ~ time + treatment + interaction + factor(province_id), data=ps3)
vcov_ps39 <- cluster.vcov(ps39, ps3$province_id)
ps39out <- coeftest(ps39, vcov_ps39)

ps310 <- lm(index5 ~ time + treatment + interaction + factor(province_id), data=ps3)
vcov_ps310 <- cluster.vcov(ps310, ps3$province_id)
ps310out <- coeftest(ps310, vcov_ps310)

stargazer(ps36out, ps37out, ps38out, ps39out, ps310out, type="html", column.labels = c('Infrastructure index', 'Agricultural services index','Health services index',
                                                                                       'Education index','Communication index'), omit='factor',
          dep.var.labels=c('Index i'),
          covariate.labels = c('Post-reform dummy', 'Village government recentralized dummy', 'Post-reform dummy x Village government recentralized dummy'), out="ps3table2.htm")
