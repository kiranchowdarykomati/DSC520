library('foreign')
setwd("c:/Users/kiran/dsc520")
ThS <- read.arff('data/ThoraricSurgery.arff')
#Ths_glm <- glm(formula=Risk1Yr ~ DGN,data = ThS,family=binomial())
Ths_glm <- glm(formula = Risk1Yr ~ DGN + PRE14, family = binomial(), data = ThS)
summary(Ths_glm)

ThS$predicted.probabilities<-fitted(Ths_glm)
factor(ThS$Risk1Yr)
res <- predict(hs_glm,type="response")
confMatrix <- table(Actual_Value=ThS$Risk1Yr,Predicted_Value=res>0.5)
confMatrix
confMatrix[[1,1]]/sum(confMatrix)