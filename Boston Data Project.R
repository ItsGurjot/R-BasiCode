
##################### Project ######################

## Train & Test data

getwd()
setwd("E:/R Language/Projects")

Boston_train_data <- read.csv("train.csv")
View(Boston_train_data)

Boston_test_data <- read.csv("Test.csv")
View(Boston_test_data)

dim(Boston_train_data)
dim(Boston_test_data)

table(is.na(Boston_train_data))
table(is.na(Boston_test_data))

sum(is.na(Boston_test_data))
sum(is.na(Boston_train_data))

## Binding Data

## Since the colms are not same,
## So would have to store 1 as the value for Medv.

Boston_test_data$medv <- 1
View(Boston_test_data)

## Now we can proceed :

Binding_data <- rbind(Boston_train_data,Boston_test_data)
Binding_data

dim(Binding_data)
View(Binding_data)

## Structure :

str(Boston_train_data)
str(Boston_test_data)
str(Binding_data)

unique(Binding_data$chas)
unique(Binding_data$rad)
unique(Binding_data$medv)

Binding_data$tax <- as.numeric(Binding_data$tax)
class(Binding_data$tax)

## For Correlation

Corr <- cor(Binding_data[,c(2:4,6:9,11:15)],method = "pearson")
Corr
View(Corr)

library(ppcor)

Partial_Corr <- data.frame(pcor(Binding_data[,c(2:4,6:9,11:15)],method = "pearson"))
Partial_Corr
## Where, +1 = strong (+ve) correlation
## Where, -1 = strong (-ve) correlation

##gp means corrl. bw two values


############### Outliers Detection & Treatment ###############

boxplot(Binding_data$crim)$out

quantiles <- quantile(Binding_data$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(Binding_data$crim)
normal_crim <- subset(Binding_data,Binding_data$crim>(quantiles[1]-range)&Binding_data$crim<(quantiles[2]+range))
normal_crim

boxplot(normal_crim$crim,col="red")$out

quantiles <- quantile(normal_crim$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim$crim)
normal_crim_1 <- subset(normal_crim,normal_crim$crim>(quantiles[1]-range)&normal_crim$crim<(quantiles[2]+range))
normal_crim_1

boxplot(normal_crim_1$crim,col="red")$out

quantiles <- quantile(normal_crim_1$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim_1$crim)
normal_crim_2 <- subset(normal_crim_1,normal_crim_1$crim>(quantiles[1]-range)&normal_crim_1$crim<(quantiles[2]+range))
normal_crim_2

boxplot(normal_crim_2$crim,col="red")$out

quantiles <- quantile(normal_crim_2$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim_2$crim)
normal_crim_3 <- subset(normal_crim_2,normal_crim_2$crim>(quantiles[1]-range)&normal_crim_2$crim<(quantiles[2]+range))
normal_crim_3

boxplot(normal_crim_3$crim,col="red")$out

quantiles <- quantile(normal_crim_3$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim_3$crim)
normal_crim_4 <- subset(normal_crim_3,normal_crim_3$crim>(quantiles[1]-range)&normal_crim_3$crim<(quantiles[2]+range))
normal_crim_4

boxplot(normal_crim_4$crim,col="red")$out

quantiles <- quantile(normal_crim_4$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim_4$crim)
normal_crim_5 <- subset(normal_crim_4,normal_crim_4$crim>(quantiles[1]-range)&normal_crim_4$crim<(quantiles[2]+range))
normal_crim_5

boxplot(normal_crim_5$crim,col="red")$out

quantiles <- quantile(normal_crim_5$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim_5$crim)
normal_crim_6 <- subset(normal_crim_5,normal_crim_5$crim>(quantiles[1]-range)&normal_crim_5$crim<(quantiles[2]+range))
normal_crim_6

boxplot(normal_crim_6$crim,col="red")$out

quantiles <- quantile(normal_crim_6$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim_6$crim)
normal_crim_7 <- subset(normal_crim_6,normal_crim_6$crim>(quantiles[1]-range)&normal_crim_6$crim<(quantiles[2]+range))
normal_crim_7

boxplot(normal_crim_7$crim,col="red")$out

quantiles <- quantile(normal_crim_7$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim_7$crim)
normal_crim_8 <- subset(normal_crim_7,normal_crim_7$crim>(quantiles[1]-range)&normal_crim_7$crim<(quantiles[2]+range))
normal_crim_8
boxplot(normal_crim_8$crim,col="red")$out

quantiles <- quantile(normal_crim_8$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim_8$crim)
normal_crim_9 <- subset(normal_crim_8,normal_crim_8$crim>(quantiles[1]-range)&normal_crim_8$crim<(quantiles[2]+range))
normal_crim_9

boxplot(normal_crim_9$crim,col="red")$out

quantiles <- quantile(normal_crim_9$crim,probs=c(0.25,0.75))
range <- 1.5*IQR(normal_crim_9$crim)
normal_crim_10 <- subset(normal_crim_9,normal_crim_9$crim>(quantiles[1]-range)&normal_crim_9$crim<(quantiles[2]+range))
normal_crim_10

boxplot(normal_crim_10$crim,col="red")$out

## ZN has some outliers :

boxplot(Binding_data$zn,col="red")$out

quantiles <- quantile(Binding_data$zn, probs = c(0.25,0.75))
range <- 1.5*IQR(Binding_data$zn)
normalize_zn <- subset(Binding_data,Binding_data$zn>(quantiles[1]-range)&Binding_data$zn<(quantiles[2]+range))
normalize_zn

boxplot(normalize_zn$zn,col="red")$out

quantiles <- quantile(normalize_zn$zn, probs = c(0.25,0.75))
range <- 1.5*IQR(normalize_zn$zn)
normalize_zn_1 <- subset(normalize_zn,normalize_zn$zn>(quantiles[1]-range)&normalize_zn$zn<(quantiles[2]+range))
normalize_zn_1

boxplot(normalize_zn_1$zn,col="red")$out

## RM has some outliers :

boxplot(Binding_data$rm,col="red")$out

quantiles <- quantile(Binding_data$rm, probs = c(0.25,0.75))
range <- 1.5*IQR(Binding_data$rm)
normalize_rm <- subset(Binding_data,Binding_data$rm>(quantiles[1]-range)&Binding_data$rm<(quantiles[2]-range))
normalize_rm

boxplot(normalize_rm$rm,col="red")$out

## DIS has some outliers :

boxplot(Binding_data$dis,col="red")

quantiles <- quantile(Binding_data$dis, probs = c(0.25,0.75))
range <- 1.5*IQR(Binding_data$dis)
normalize_dis <- subset(Binding_data,Binding_data$dis>(quantiles[1]-range)&Binding_data$dis<(quantiles[2]+range))
normalize_dis

boxplot(normalize_dis$dis,col="red")$out

## PTratio has some outliers :

boxplot(Binding_data$ptratio,col="red")

quantiles <- quantile(Binding_data$ptratio, probs = c(0.25,0.75))
range <- 1.5*IQR(Binding_data$ptratio)
normalize_ptratio <- subset(Binding_data,Binding_data$ptratio>(quantiles[1]-range)&Binding_data$ptratio<(quantiles[2]+range))
normalize_ptratio

boxplot(normalize_ptratio$ptratio,col="red")$out

## Black has some outliers ;

boxplot(Binding_data$black,col="red")$out

quantiles <- quantile(Binding_data$black, probs = c(0.25,0.75))
range <- 1.5*IQR(Binding_data$black)
normalize_black <- subset(Binding_data,Binding_data$black>(quantiles[1]-range)&Binding_data$black<(quantiles[2]+range))
normalize_black

boxplot(normalize_black$black,col="red")$out

quantiles <- quantile(normalize_black$black, probs = c(0.25,0.75))
range <- 1.5*IQR(normalize_black$black)
normalize_black_1 <- subset(normalize_black,normalize_black$black>(quantiles[1]-range)&normalize_black$black<(quantiles[2]+range))
normalize_black_1

boxplot(normalize_black_1$black,col="red")$out

quantiles <- quantile(normalize_black_1$black, probs = c(0.25,0.75))
range <- 1.5*IQR(normalize_black_1$black)
normalize_black_2 <- subset(normalize_black_1,normalize_black_1$black>(quantiles[1]-range)&normalize_black_1$black<(quantiles[2]+range))
normalize_black_2

boxplot(normalize_black_2$black,col="red")$out

quantiles <- quantile(normalize_black_2$black, probs = c(0.25,0.75))
range <- 1.5*IQR(normalize_black_2$black)
normalize_black_3 <- subset(normalize_black_2,normalize_black_2$black>(quantiles[1]-range)&normalize_black_2$black<(quantiles[2]+range))
normalize_black_3

boxplot(normalize_black_3$black,col="red")$out

quantiles <- quantile(normalize_black_3$black, probs = c(0.25,0.75))
range <- 1.5*IQR(normalize_black_3$black)
normalize_black_4 <- subset(normalize_black_3,normalize_black_3$black>(quantiles[1]-range)&normalize_black_3$black<(quantiles[2]+range))
normalize_black_4

boxplot(normalize_black_4$black,col="red")$out

quantiles <- quantile(normalize_black_4$black, probs = c(0.25,0.75))
range <- 1.5*IQR(normalize_black_4$black)
normalize_black_5 <- subset(normalize_black_4,normalize_black_4$black>(quantiles[1]-range)&normalize_black_4$black<(quantiles[2]+range))
normalize_black_5

boxplot(normalize_black_5$black,col="red")$out 

## LSTAT has some outliers :

boxplot(Binding_data$lstat,col="red")$out

quantiles <- quantile(Binding_data$lstat, probs = c(0.25,0.75))
range <- 1.5*IQR(Binding_data$lstat)
normalize_lstat <- subset(Binding_data,Binding_data$lstat>(quantiles[1]-range)&Binding_data$lstat<(quantiles[2]+range))
normalize_lstat

boxplot(normalize_lstat$lstat,col="red")$out

## CHAS has some outliers :

unique(Binding_data$chas)

boxplot(Binding_data$chas,col="red")$out

## Since, chas has only been given two values,
## So we cannot take out the outliers, as it would misinterpret the data.

############### Histogram & Visualizations ###############

hist(Binding_data$crim,col="red")
hist(Binding_data$zn,col="red")
hist(Binding_data$indus,col="red")
hist(Binding_data$chas,col="red")
hist(Binding_data$nox,col="red")
hist(Binding_data$rm,col="red")
hist(Binding_data$age,col="red")
hist(Binding_data$dis,col="red")
hist(Binding_data$rad,col="red")
hist(Binding_data$tax,col="red")
hist(Binding_data$ptratio,col="red")
hist(Binding_data$black,col="red")
hist(Binding_data$lstat,col="red")
hist(Binding_data$medv,col="red")

## Corrplot

library(corrplot)
corrplot(Corr,method = "circle")

## For Skewness & Kurtosis :

library(psych)
describe(Binding_data)

# Names    skew    kurtosis
# ID       0.00    -1.21 
# crim     5.19    36.60 
# zn       2.21     3.95 
# indus    0.29    -1.24 
# chas     3.39     9.48 
# nox      0.72    -0.09 
# rm       0.40     1.84 
# age     -0.60    -0.98 
# dis      1.01     0.46 
# rad      1.00    -0.88 
# tax      0.67    -1.15 
# ptratio -0.80    -0.30 
# black   -2.87     7.10 
# lstat    0.90     0.46 
# medv     0.52    -0.24 


############### Data Modelling ###############

summary(Binding_data)

Binding_data_1 <- Binding_data[,c(2:4,6:9,11:15)]
Binding_data_1

New_Train <- Binding_data_1[1:nrow(Boston_train_data),]
New_Train

New_Test <- Binding_data_1[1:nrow(Boston_test_data),]
New_Test

dim(New_Train)
dim(New_Test)

## Linear Regression :

## Here the dependent variable is medv : median value for owner occupied homes.

Boston_Model <- lm(log(medv)~crim+rm+dis+tax+lstat,data = New_Train)
options(scipen = 999)
summary(Boston_Model)


Fitted_Values <- Boston_Model$fitted.values
Fitted_Values

Model_Resd. <- Boston_Model$residuals
Model_Resd.

## Checking Heteroscedasticity :

plot(Boston_Model$fitted.values,Boston_Model$residuals)

# Since it doesn't gives a funnel shape, 
# There's No Heteroscedasticity.

## For Checking Normality of Errors :

#1. Histogram :
hist(Model_Resd.)

## The model is not normally distributed rather is (+vely) skewed.

#2. QQ-Plot :

qqnorm(Model_Resd.)
qqline(Model_Resd.)

# Since it is very close to the straight line
# There's a sign of normality.

#3. KS-Test :

Resd. <- Model_Resd.
KS_Test <-(Resd.-mean(Resd.))/sqrt(var(Resd.))
ks.test(KS_Test,rnorm(length(KS_Test)))

## Checking Multicollinearity :

library(usdm)
vif(Binding_data_1)

# As a rule of thumb, if VIF > 5, 
# There's a sign of Multicollinearity.
# However, here we have No Multicollinearity.

## Checking Autocorrelation :

# Ho : There's No Autocorrelation.
# Ha : There's Autocorrelation.

library(lmtest)

dwtest(log(medv)~crim+rm+dis+tax+lstat,data = Binding_data)

# There's an evidence of (+ve) Autocorrelation.

############### Accuracy ###############

plot(Boston_Model)

## 1. : It's the qqnorm showing normality.
## 2. : It shows standardised residuals.

# Prediction :

prediction <- predict(Boston_Model,Boston_test_data)
prediction

install.packages("Metrics")
library(Metrics)

Boston_RMSE <- rmse(prediction,Boston_test_data[,15])
Boston_RMSE

## Where the value of rmse = 2.0567, states that the model is somewhat 
## A Better Fit.
## Where rmse is the Root Mean Squared Error


