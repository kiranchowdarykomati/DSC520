setwd("C:/Users/kiran/dsc520/assignments/assignment10/CarDekhoDatSet")
library('dplyr')
CD.df1Raw <- read.csv("CarDekho1.csv")
CD.df2Raw <- read.csv('CarDekho2.csv')
CD.df3Raw <- read.csv('CarDekho3.csv')
CD.df1<-select(CD.df1Raw, -Present_Price) %>%  
                     rename(name=Car_Name,
                     year=Year,
                     selling_price=Selling_Price,
                     km_driven=Kms_Driven,
                     fuel=Fuel_Type,
                     seller_type=Seller_Type,
                     transmission=Transmission,
                     owner=Owner) %>%
        mutate(owner=recode(owner,`0`="First Owner",
                            `1`="Second Owner",
                            `3`="Fourth Owner"))

CD.df1$selling_price<- CD.df1$selling_price*100000

CD.df2 <- CD.df2Raw
CD.df3 <- select(CD.df3Raw,-engine,-max_power,-torque,-seats,-mileage)
CarData <- rbind(CD.df1,CD.df2,CD.df3)

head(CarData)
CarData_SS <- select(CarData,selling_price,km_driven,year)
cor(CarData_SS)


hist(CarData$year,
     xlab   = "Year",
     main   = "Histogram of Year")

barplot(table(CarData$seller_type),
        xlab   = "Seller Type",
        ylab   = "Frequency",
        main   = "Sellers",
        col    = "dodgerblue",
        border = "darkorange")


plot(selling_price ~ year,CarData,
     xlab = "Sale Year",
     ylab = "Selling Price",
     main = "Year Vs Price",
     pch  = 20,
     cex  = 2,
     col  = "dodgerblue")

summary(CarData_SS)
