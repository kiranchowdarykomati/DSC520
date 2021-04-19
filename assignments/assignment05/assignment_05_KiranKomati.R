# Assignment: ASSIGNMENT 5
# Name: Komati, Kiran
# Date: 2021-04-18

## Set the working directory to the root of your DSC 520 directory
setwd("c:/Users/kiran/dsc520")

# Using either the same dataset(s) you used in the previous weeks' exercise or a brand-new dataset of your choosing, 
# perform the following transformations (Remember, anything you learn about the Housing dataset in these two weeks can be used 
# for a later exercise!)
# Using the dplyr package, use the 6 different operations to analyze/transform the data - 
# GroupBy, Summarize, Mutate, Filter, Select, and Arrange - Remember this isn't just modifying data, you are learning about 
# your data also - so play around and start to understand your dataset in more detail
# Using the purrr package - perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, etc.
# Use the cbind and rbind function on your dataset
# Split a string, then concatenate the results back together

install.packages("tidyverse")
library("tidyverse")

library("readxl")
HDF <- read_excel('data/week-6-housing.xlsx')
head(HDF)

HDF %>% 
    filter('Sale Price' > 500000) 

HDF %>% 
    select('Sale Date',starts_with("year"))

HDF %>% 
    arrange(desc(year_built))

HDF %>% 
    group_by(bedrooms) %>% 
    summarise(
        n=n(),
        Avg_SqFt=mean(square_feet_total_living,na.rm = TRUE)
    )

HDF2 <- HDF %>% 
    mutate(price_per_sqft= `Sale Price` / sq_ft_lot) %>% 
    select('Sale Date',addr_full,'price_per_sqft','Sale Price',sq_ft_lot)

#1.b

keep(HDF$`Sale Price`,~ .x>1000000)
discard(HDF$`Sale Price`,~ .x<1000000)


houses_gt_500k <- HDF %>% 
    filter('Sale Price' > 500000) 

houses_gt_300k <- HDF %>% 
    filter('Sale Price' > 300000)

#1.c
houses_all <- rbind(houses_gt_500k,houses_gt_300k)

houses_all

new_house_details <- cbind(HDF$addr_full,HDF$`Sale Price`,HDF2$price_per_sqft)

new_house_details

#1.d
split_test <- "Old McDonald has a farm"
output<-strsplit(split_test," ")[[1]]
print(output)

original <- paste(output,collapse=' ')
original