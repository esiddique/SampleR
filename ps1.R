getwd()
setwd("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ 174/")
library(haven)
mydata <- read_dta("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ 174/ps1_data.dta")
write.csv(mydata, file = "mydata.csv")
ps1 <- read.csv("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ 174/mydata.csv")
ps1

#3a
#Using the length function finds how many entries are in an array
#Using the unique function eliminates duplicates
length(unique(ps1$hplstudentkey))
length(unique(ps1$apfschoolcode))

#3b
#I use ps1$incentive==0 so that only the non-incentive schools will be selected, and find the year 0 test scores of those schools.
#This gives me zeroswithna. I use the same method to find oneswithna.
  
zeroswithna <- ps1$y0_nts[ps1$incentive==0]
oneswithna <- ps1$y0_nts[ps1$incentive==1]

#I use the !is.na function to remove all NAs from the array and then find the mean
#control mean = mean(zeroswithna[!is.na(zeroswithna)])
#treatment mean = mean(oneswithna[!is.na(oneswithna)])

#The following are the control and treatment means for year 0 test scores respectively
mean(zeroswithna[!is.na(zeroswithna)])
mean(oneswithna[!is.na(oneswithna)]) 

#I use the same process to determine the proportion of males in treatment and control
e <- ps1$Male[ps1$incentive==0]
f <- ps1$Male[ps1$incentive==1]

mean(e[!is.na(e)])
mean(f[!is.na(f)])

#I do the same for the proportion of students who are scheduled caste

g <- ps1$SC[ps1$incentive==0]
h <- ps1$SC[ps1$incentive==1]
  
mean(g[!is.na(g)])
mean(h[!is.na(h)])

#I do the same for household affluence

a <- ps1$hh_affluence_index[ps1$incentive==0]
b <- ps1$hh_affluence_index[ps1$incentive==1]
mean(a[!is.na(a)])
mean(b[!is.na(b)])

#I do the same for parent literacy

c <- ps1$parent_literacy_index[ps1$incentive==0]
d <- ps1$parent_literacy_index[ps1$incentive==1]
mean(c[!is.na(c)])
mean(d[!is.na(d)])

#To calculate the coefficient and standard errors, 
#I will require the multivayvcov and lmtest packages in order to use the coeftest function
#to find coefficients and standard errors

install.packages('lmtest')
require(lmtest)
install.packages("multiwayvcov")
library(multiwayvcov)

#To find the clustered SE at the school level, I use the following functions from the above packages

m1 <- lm(y0_nts ~ incentive, data=ps1)
vcov_apf1 <- cluster.vcov(m1, ps1$apfschoolcode)
coeftest(m1, vcov_apf1)

#Here, slope = -0.031672, SE = 0.044952

#I do the same for gender regressed on incentive

m2 <- lm(Male ~ incentive, data=ps1)
vcov_apf2 <- cluster.vcov(m2, ps1$apfschoolcode)
coeftest(m2, vcov_apf2)

#Here, slope = -0.0136067, SE = 0.0104868

#I do the same for SC regressed on incentive

m3 <- lm(SC ~ incentive, data=ps1)
vcov_apf3 <- cluster.vcov(m3, ps1$apfschoolcode)
coeftest(m3, vcov_apf3)

#Here, slope = 0.0481562, SE = 0.024701, *

#I do the same for household affluence

m4 <- lm(hh_affluence_index ~ incentive, data=ps1)
vcov_apf4 <- cluster.vcov(m4, ps1$apfschoolcode)
coeftest(m4, vcov_apf4)

#Here, slope = 0.018019, SE = 0.078943 

#I do the same for parent literacy

m5 <- lm(parent_literacy_index ~ incentive, data=ps1)
vcov_apf5 <- cluster.vcov(m5, ps1$apfschoolcode)
coeftest(m5, vcov_apf5)

#Here, slope = 0.078515, SE= 0.049507

#3d 
#I will be using the same process as above in order to cluster SE at the school level

m6 <- lm(y2_nts_level_mean ~ incentive, data=ps1)
vcov_apf6 <- cluster.vcov(m6, ps1$apfschoolcode)
m6adjusted <- coeftest(m6, vcov_apf6)
m6adjusted

#In this regression, with no controls slope = 0.2434350, SE = 0.078790
#This is statistically significant at the 0.01 level ***

#3e
#I will be using the same clustering method as above. Instead, now I will be controlling for several variables

m7 <- lm(y2_nts_level_mean ~ incentive+hh_affluence_index+parent_literacy_index+SC+Male+y0_nts, data=ps1)
vcov_apf7 <- cluster.vcov(m7, ps1$apfschoolcode)
m7adjusted <- coeftest(m7, vcov_apf7)
m7adjusted

#3g
#I will be using the same clustering SE method, and adding one additional control: year 1 scores
#I will be using stargazer to create the regression table

m8 <- lm(y2_nts_level_mean ~ incentive+hh_affluence_index+parent_literacy_index+SC+Male+y0_nts+y1_nts_level_mean, data=ps1)
vcov_apf8 <- cluster.vcov(m8, ps1$apfschoolcode)
m8adjusted <- coeftest(m8, vcov_apf8)
m8adjusted

#Intercept on incentive is 0.231139 (0.066149)
#Intercept on y1 year is 0.468719 (0.040511)

#In order to create regression table combining all of the 3 regressions, I will be using the stargazer package
install.packages('stargazer')
library(stargazer)

stargazer(m6adjusted, m7adjusted, m8adjusted, type="html",
          dep.var.labels=c("Year 2 math test scores"),
          covariate.labels = c('Incentive', 'Household affluence', 'Parent literacy',
                               'Student is Scheduled caste', 'Male', 'Year 0 test scores', 'Year 1 math test scores'), out="ps1tableall2.htm") 

