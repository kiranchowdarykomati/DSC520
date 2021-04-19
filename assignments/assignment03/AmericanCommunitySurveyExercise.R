library(ggplot2)
library(pastecs)
setwd("C:/Users/kiran/dsc520")
ACS <- read.csv(file = 'data/acs-14-1yr-s0201.csv')
str(ACS)
nrow(ACS)
ncol(ACS)
ggplot(ACS,aes(HSDegree)) + geom_histogram(bins=30) 

ggplot(ACS,aes(HSDegree)) + geom_histogram(bins=30) + ggtitle('High School Degree Percent Vs No. Of Counties') + xlab('% of High School Degree')+ylab('No. of Counties')


ACS_HIST <- ggplot(ACS,aes(HSDegree)) + geom_histogram(aes(y = ..density..),binwidth = 1, colour = "black")
ACS_HIST + stat_function(fun = dnorm,args=list(mean=mean(ACS$HSDegree),sd=sd(ACS$HSDegree)),color="black",size=1)

options(scipen=100)
options(digits=2)
stat.desc(ACS$HSDegree, basic = FALSE, norm = TRUE)
#3.2.2.5
ggplot(ACS,aes(HSDegree)) + geom_density()
qqnorm(ACS$HSDegree, main = "Normal Q-Q Plot for High School Degree") 