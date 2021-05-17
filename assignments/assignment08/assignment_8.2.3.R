library("tidyverse")
library("readxl")
HDF <- read_excel('data/week-6-housing.xlsx')

library(dplyr)

HDF = rename(HDF,sale_price = `Sale Price`, sale_date=`Sale Date`)


HDFSS <- separate(HDF, sale_date, c("sale_year", "sale_month", "sale_day"), sep = "-") %>%
    dplyr::select(sale_price,sq_ft_lot,square_feet_total_living,bedrooms,bath_full_count,year_built,building_grade)


HDFSS$sale_year <- as.numeric(as.character(HDFSS$sale_year))


cor(HDFSS,use="everything",method="pearson")

HDF_lm <-  lm(sale_price ~ sq_ft_lot,data = HDF)
HDFSS_lm <-  lm(sale_price ~ sq_ft_lot + square_feet_total_living + +building_grade  + bath_full_count + year_built + bedrooms, data=HDFSS)

summary(HDF_lm)
summary(HDFSS_lm)



# Used correlation to identify the variables that are related to the sale price and picked those that have relatively strong correlation

#R2 and adjusted R2 increased after adding the new predictors to the linear model which means the multiple linear regression with the 
#selected variables did a relatively good job in predicting prices and accounts for nearly 22% of the values.

#install.packages('QuantPsyc')
library('QuantPsyc')

lm.beta(HDFSS_lm)

#These estimates tell us the number of standard deviations by which the outcome will change as a result of one standard deviation change in the predictor.

confint(HDFSS_lm)

#This confidence interval tells us that the predictors (sq ft living total,bath_full_count) have very tight confidence intervals, indicating that
#the estimates for the current model are likely to be representative of the true population
#values. The interval for sq_ft_lot,building_grade,bath_full_count,year_built is wider (but still does not cross zero), indicating that
#the parameter for this variable is less representative, but nevertheless significant.For predictor "bedrooms" the value crossed zero indicating that in some samples the predictor has a negative relationship
#to the outcome whereas in others it has a positive relationship.

anova(HDF_lm,HDFSS_lm)

#The value of F is 693.34,  we can say that albumSales.3 significantly improved
#the fit of the model to the data compared to albumSales.2, F(5, 12858) = 693.34, p < .001.


HDFSS$standardized.residuals<- rstandard(HDFSS_lm)
HDFSS$studentized.residuals<-rstudent(HDFSS_lm)
HDFSS$cooks.distance<-cooks.distance(HDFSS_lm)
HDFSS$dfbeta<-dfbeta(HDFSS_lm)
HDFSS$dffit<-dffits(HDFSS_lm)
HDFSS$leverage<-hatvalues(HDFSS_lm)
HDFSS$covariance.ratios<-covratio(HDFSS_lm)


HDFSS$large.residuals <- HDFSS$standardized.residuals > 2 | HDFSS$standardized.residuals < -2

sum(HDFSS$large.residuals) 

HDFSS_LargeResiduals <- HDFSS[HDFSS$large.residuals,c("sale_price","sq_ft_lot","square_feet_total_living","bedrooms","bath_full_count","year_built","building_grade","standardized.residuals")]
    
HDFSS[HDFSS$large.residuals , c("cooks.distance", "leverage", "covariance.ratios")]
    
#xi  none of them has a Cook's distance greater than 1

HDFSS %>% 
    filter(cooks.distance >1) 

7/12865

#around 436 cases where leverage greater than three times average


k <- ncol(HDF)
n <- nrow(HDF)
avg <- (k+1)/n
HDFSS %>% 
    filter(leverage >3*avg) 


#covariance ratio. There are 796 records where the covariance ratio is not with in the boundaries. But none of them are way beyond the limits
#which means that there are no significant cases that influence the model.

CVR_Upperlimit<-1+(3*(k+1)/n)
CVR_Lowerlimit<-1-(3*(k+1)/n)

HDFSS %>% 
    filter(covariance.ratios> CVR_Upperlimit | covariance.ratios< CVR_Lowerlimit) %>%
    dplyr::select(covariance.ratios,cooks.distance)

library('car')
durbinWatsonTest(HDFSS_lm)

#here the value is 0.541 which is less than 1 and it means that the assumption of independence is not met.

vif(HDFSS_lm)

1/vif(HDFSS_lm)

mean(vif(HDFSS_lm))

#All the VIF values are well below 10 and the tolerance statistics are well above 0.2 , the mean VIF is 1.953. we can safely conclude that 
#there is no collinearity within our data.


plot(HDFSS_lm)

#the plot function shows that values are evenly distributed around zero which indicates that the assumptions
#of linearity, randomness and homoscedasticity have been met. 

#the second graph is skewed which shows that there's deviation from normality. 

hist(HDFSS$studentized.residuals)

library(ggplot2)

histogram<-ggplot(HDFSS, aes(studentized.residuals)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), 
            colour = "black", fill = "white") +labs(x = "Studentized Residual", y = "Density")

histogram + stat_function(fun = dnorm, args = list(mean = mean(HDFSS$studentized.residuals, na.rm = TRUE), sd = sd(HDFSS$studentized.residuals, na.rm = TRUE)), colour
                          = "red", size = 1)

library(car)

qqplot.resid <- qplot(sample=studentized.residuals,data=HDFSS) +xlab('Theoretical Values') +ylab ("Observed Values") 

library(tidyverse)


library(plotly)

HDFSS$fitted <- HDFSS_lm$fitted.values

scatter <- ggplot(HDFSS, aes(fitted,studentized.residuals))

scatter + geom_point() + geom_smooth(method = "lm", colour = "Blue") + labs(x = "Fitted Values", y = "Studentized Residual")

# For a model to be unbiased, there are several assumptions that must be true. For our model, assumption of independence is not met
# and  errors are not normally distributed. 
#If a model is unbiased, it means that on an average the regression model from sample is same as the population model.