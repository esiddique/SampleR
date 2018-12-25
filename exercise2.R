setwd("/Users/emaansiddique/Desktop/Emaan\ Siddique/School/ECON\ C171")
hwk2 <- read.csv("ENIGH_2004and2014.csv")

#1
mean2004 <- mean(hwk2$monthly_expenditures_per_adult_equivalent_2010pesos[hwk2$year == 2004])
mean2014 <- mean(hwk2$monthly_expenditures_per_adult_equivalent_2010pesos[hwk2$year == 2014])

#2
((mean2014/mean2004)^(1/10)) - 1

#3 
incomes2004 <- hwk2$monthly_expenditures_per_adult_equivalent_2010pesos[hwk2$year == 2004]
incomes2014 <- hwk2$monthly_expenditures_per_adult_equivalent_2010pesos[hwk2$year == 2014]

lessthan5000_2004 <- incomes2004 < 5000
lessthan5000_2014 <- incomes2014 < 5000

rank2004 <- rank(incomes2004[lessthan5000_2004==TRUE])/length(lessthan5000_2004)
rank2014 <- rank(incomes2014[lessthan5000_2014==TRUE])/length(lessthan5000_2014)

plot(rank2004, incomes2004[lessthan5000_2004==TRUE], col='red', main="Poverty Profile: Mexico, 2004-2014", xlab='Population rank from poorest to richest', ylab='Per capita consumption of individuals (income)')
points(rank2014, incomes2014[lessthan5000_2014==TRUE], col='blue')
abline(h = 1700)
text(0,1700, "z")

legend('topleft', c('2004', '2014'), col=c('red', 'blue'), lwd=3, cex=0.5)

#4

lessthan1700_2004 <- incomes2004 < 1700
pzero2004 <- (1/length(incomes2004))*length(lessthan1700_2004[lessthan1700_2004==TRUE])
pzero2004


lessthan1700_2014 <- incomes2014 < 1700
pzero2014 <- (1/length(incomes2014))*length(lessthan1700_2014[lessthan1700_2014==TRUE])
pzero2014

#5
pone2004 <- (1/length(incomes2004))*sum(1 - (incomes2004/1700))
pone2004

pone2014 <- (1/length(incomes2014))*sum(1 - (incomes2014)/1700)
pone2014

#7
#Lorenz curve for 2004
install.packages('ineq')
library(ineq)

lorenz2004 <- Lc(incomes2004)
lorenz2014 <- Lc(incomes2014)

plot(lorenz2004, col='red', main="Lorenz Curves for 2004 and 2014", xlab='% Populaton', ylab='% Income')
lines(lorenz2014, col = 'blue')

legend('topleft', c('Lorenz Curve 2004', 'Lorenz Curve 2014'), col=c('red', 'blue'), lwd=3, cex=0.5)

#ALTERNATIVE 7

sort(incomes2004)
quantiles2004 <- c(quantile(incomes2004, 0),
                   quantile(incomes2004, 0.1),
                   quantile(incomes2004, 0.2),
                   quantile(incomes2004, 0.3),
                   quantile(incomes2004, 0.4),
                   quantile(incomes2004, 0.5),
                   quantile(incomes2004, 0.6),
                   quantile(incomes2004, 0.7),
                   quantile(incomes2004, 0.8),
                   quantile(incomes2004, 0.9), quantile(incomes2004, 1))

incomesum2004 <- sum(quantile(incomes2004, 0),
                     quantile(incomes2004, 0.1),
                     quantile(incomes2004, 0.2),
                     quantile(incomes2004, 0.3),
                     quantile(incomes2004, 0.4),
                     quantile(incomes2004, 0.5),
                     quantile(incomes2004, 0.6),
                     quantile(incomes2004, 0.7),
                     quantile(incomes2004, 0.8),
                     quantile(incomes2004, 0.9), quantile(incomes2004, 1))

percentincome2004 <- quantiles2004 / incomesum2004
percentpop <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

plot(percentpop, percentincome2004)

quantiles2014 <- c(quantile(incomes2014, 0),
                   quantile(incomes2014, 0.1),
                   quantile(incomes2014, 0.2),
                   quantile(incomes2014, 0.3),
                   quantile(incomes2014, 0.4),
                   quantile(incomes2014, 0.5),
                   quantile(incomes2014, 0.6),
                   quantile(incomes2014, 0.7),
                   quantile(incomes2014, 0.8),
                   quantile(incomes2014, 0.9), quantile(incomes2014, 1))

incomesum2014 <- sum(quantile(incomes2014, 0),
                     quantile(incomes2014, 0.1),
                     quantile(incomes2014, 0.2),
                     quantile(incomes2014, 0.3),
                     quantile(incomes2014, 0.4),
                     quantile(incomes2014, 0.5),
                     quantile(incomes2014, 0.6),
                     quantile(incomes2014, 0.7),
                     quantile(incomes2014, 0.8),
                     quantile(incomes2014, 0.9), quantile(incomes2014, 1))

percentincome2014 <- quantiles2014 / incomesum2014
percentpop <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

plot(percentpop, percentincome2014)

#8

gini2004 <- 2/(length(incomes2004)*mean(incomes2004)) * var(rank(incomes2004), incomes2004)
gini2014 <- 2/(length(incomes2014)*mean(incomes2014)) * var(rank(incomes2014), incomes2014)
gini2004
gini2014
Gini(incomes2004)
Gini(incomes2014)

#9
(gini2014 - gini2004) / gini2004 * 100
#Inequality has decreased according to Gini by 10.23%

#10
quantile(incomes2004, 0.8)
quantile(incomes2004, 0.2)
kuznets2004 = 1251.254 / 273.7394 
kuznets2004

quantile(incomes2014, 0.8)
quantile(incomes2014, 0.2)
kuznets2014 <- 1791.422 / 556.1471 
kuznets2014

(kuznets2014 - kuznets2004) / kuznets2004 * 100
#Inequality has decreased even more according to Kuznets inequality ratio by 29.53%

#11
incomes2014doubled <- 2*incomes2014
incomes2014doubled
gini2014doubled <- 2/(length(incomes2014doubled)*mean(incomes2014doubled)) * var(rank(incomes2014doubled), incomes2014doubled)
(gini2014doubled - gini2014)/gini2014 * 100
#Doubling incomes does not change Gini because inequality remains unchanged

#12
incomes2014plus1000 <- incomes2014 + 1000
gini2014plus1000 <- 2/(length(incomes2014plus1000)*mean(incomes2014plus1000)) * var(rank(incomes2014plus1000), incomes2014plus1000)
(gini2014plus1000 - gini2014) / gini2014 * 100
#Adding 1000 pesos to everyones income has decreased Gini by 39.95%

#13

install.packages('lubridate')
library(lubridate)

percentiles2004 <- quantile(incomes2004, probs = seq(0, 1, by =0.01))
percentiles2014 <- quantile(incomes2014, probs = seq(0, 1, by =0.01))
percentilechange <- (percentiles2014 - percentiles2004) / percentiles2004

meanchange <- (mean2014 - mean2004) / mean2004
medianchange <- (median(incomes2014) - median(incomes2004)) / median(incomes2004)

percentiles <- 100*seq(0, 1, by = 0.01)
plot(percentiles, percentilechange, main="Growth Incidence Curve for Mexico, 2004-2014", xlab='The poorest p% of population ranked by per capita income', ylab='Annual growth in income per person (%)')
lines(percentiles, percentilechange)
abline(h = meanchange)
text(0,meanchange, "Mean", cex=0.8)
abline(h = medianchange)
text(0, medianchange, "Median", cex=0.8)


