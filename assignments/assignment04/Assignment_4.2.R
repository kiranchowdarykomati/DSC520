setwd("C:/Users/kiran/dsc520")
scores <- read.csv(file = 'data/scores.csv')
scores

#1.1 sections are the observational units here
#1.2 Section is categorical and count , score are quantitative
factor(scores$Section)

#1.3
SportsScores <- subset(scores,Section=='Sports')
SportsScores

RegularScores <- subset(scores,Section=='Regular')

#1.4
require(ggplot2)
plot(SportsScores$Score,SportsScores$Count,type='o',col='red',main='Scores Vs Counts',xlab='Scores',ylab='Counts')
lines(RegularScores$Score,RegularScores$Count,type='o',col='blue')

#1.4.a  mean for regular section is higher than the sports section
#1.4.b few scores in one group are less than the mean of the other group

summary(RegularScores)
summary(SportsScores)
#1.4.c names of application areas used in the other section



library("readxl")

HousingDS <- read_excel("data/week-6-housing.xlsx")
HousingDS

#2.a
max_sale_price<-apply(HousingDS[,c(2)],2,max,na.rm=TRUE)
#2.b
aggregate(`Sale Price` ~ `Sale Date`, data = HousingDS , mean)

#2.c
library(plyr)
max_sale_price_by_zip <- ddply(HousingDS,~zip5,summarize,max_sale_price=max(`Sale Price`))
max_sale_price_by_zip

#2.d
install.packages("car")
library(car)
library(ggplot2)
qqnorm(HousingDS$`Sale Price`) # not a straight line
ggplot(HousingDS,aes(`Sale Price`)) + geom_density() #Right Skewed

#2.e
boxplot(HousingDS$`Sale Price`) #one is nearer to zero

#2.f
renamed_sale_price<-rename(HousingDS, c(`Sale Price` = "Sale_Price"))
renamed_sale_price
order_by_saleprice <- arrange(renamed_sale_price,Sale_Price)
order_by_saleprice