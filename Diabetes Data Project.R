
########### Logistics Regression ##############

#################### 1. Loading Dataset :

getwd()
setwd("E:/R Language/Projects")

Diabetes_Data <- read.csv("dataset.csv")

View(Diabetes_Data)
dim(Diabetes_Data)

######################## Checking Class, Str , & Summary :

library(bit64)
library(data.table)
library(sqldf)
library(caret)
library(plyr)

str(Diabetes_Data)

unique(Diabetes_Data$Pregnancies)
unique(Diabetes_Data$Glucose)
unique(Diabetes_Data$BloodPressure)
unique(Diabetes_Data$SkinThickness)
unique(Diabetes_Data$Insulin)
unique(Diabetes_Data$BMI)
unique(Diabetes_Data$DiabetesPedigreeFunction)
unique(Diabetes_Data$Age)
unique(Diabetes_Data$Outcome)

Diabetes_Data$Outcome <- as.factor(Diabetes_Data$Outcome)
class(Diabetes_Data$Outcome)

str(Diabetes_Data)
summary(Diabetes_Data)

Diabetes_Data_1 <- data.frame(Diabetes_Data)
Format <- data.frame(cbind(names(Diabetes_Data_1),sapply(Diabetes_Data_1,class)))
View(Format)

colSums(is.na(Diabetes_Data))

######################## Categorizing Age ;

Diabetes_Data$Age_Category <- ifelse(Diabetes_Data$Age<21,"<21",
                                     ifelse(((Diabetes_Data$Age>=21) & (Diabetes_Data$Age<=25)),"21-25",
                                     ifelse(((Diabetes_Data$Age>25) & (Diabetes_Data$Age<=30)),"25-30",
                                     ifelse(((Diabetes_Data$Age>30) & (Diabetes_Data$Age<=35)),"30-35",
                                     ifelse(((Diabetes_Data$Age>35) & (Diabetes_Data$Age<=40)),"35-40",
                                     ifelse(((Diabetes_Data$Age>40) & (Diabetes_Data$Age<=50)),"40-50",
                                     ifelse(((Diabetes_Data$Age>50) & (Diabetes_Data$Age<=60)),"50-60",">60"))))))
                                     )
Diabetes_Data$Age_Category                                     
View(Diabetes_Data)

head(Diabetes_Data$Age)
head(Diabetes_Data$Age_Category)

class(Diabetes_Data$Age_Category)
str(Diabetes_Data)

## Since the age category is in character class,
## We must change it to factor.

Diabetes_Data$Age_Category <- as.factor(Diabetes_Data$Age_Category)
class(Diabetes_Data$Age_Category)

str(Diabetes_Data)

######################## Visualizations :

hist(Diabetes_Data$Age,col = "light blue")

hist(Diabetes_Data$Pregnancies,col = "light blue")
hist(Diabetes_Data$Glucose,col="blue")
hist(Diabetes_Data$BloodPressure,col="blue")
hist(Diabetes_Data$Insulin,col="blue")
hist(Diabetes_Data$BMI,col="blue")
hist(Diabetes_Data$SkinThickness,col="red")
hist(Diabetes_Data$DiabetesPedigreeFunction,col="blue")

library(ggplot2)

ggplot(Diabetes_Data,aes(Age_Category))+geom_bar(fill="blue")
ggplot(Diabetes_Data,aes(Age_Category,BMI))+geom_boxplot(fill="blue")
ggplot(Diabetes_Data,aes(Age_Category,Glucose))+geom_boxplot(fill="blue")
ggplot(Diabetes_Data,aes(Age_Category,Pregnancies))+geom_boxplot(fill="blue")
ggplot(Diabetes_Data,aes(Age_Category,BloodPressure))+geom_boxplot(fill="blue")
ggplot(Diabetes_Data,aes(Age_Category,SkinThickness))+geom_boxplot(fill="blue")
ggplot(Diabetes_Data,aes(Age_Category,Insulin))+geom_boxplot(fill="blue")
ggplot(Diabetes_Data,aes(Age_Category,DiabetesPedigreeFunction))+geom_boxplot(fill="blue")
ggplot(Diabetes_Data,aes(Age_Category,Age))+geom_boxplot(fill="blue")

names(Diabetes_Data)
?ggplot

######################## Checking Correlation
## ( Selecting relevant variables for correlation):

str(Diabetes_Data)

DB_Corr <- cor(Diabetes_Data[,c(1,2,3,5,6,8)])
View(DB_Corr)

library(corrplot)
corrplot(DB_Corr,method = "circle")


######################## Splitting into Test & Train Data

set.seed(1234)

library(caret)
ind<-as.vector(createDataPartition(Diabetes_Data$Outcome,p=0.60,list = FALSE))

Diabetes_Train<- as.data.frame(Diabetes_Data[ind,])
Diabetes_Train
Diabetes_Test<-as.data.frame(Diabetes_Data[-ind,])
Diabetes_Test

nrow(Diabetes_Train)
nrow(Diabetes_Test)

# Data gets divided into test data , train data

######################## Fitting the Logistics Regression Model :

## Mutilcollinearity :

library(usdm)
vif(Diabetes_Data)

Diabetes_Model <- glm(Outcome~.,data = Diabetes_Train,
                      family = "binomial")
options(scipen = 999)
summary(Diabetes_Model)

Step_model <- step(Diabetes_Model)
## This is done to get the important variables.

New_Diabetes_Model <- glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction,data = Diabetes_Train,family = "binomial")
New_Diabetes_Model
summary(New_Diabetes_Model)

######################## Prediction ;

Prediction <- predict(New_Diabetes_Model,data=Diabetes_Test,type="response")
Prediction

summary(Prediction)

######################## Accuracy : 

## Now since we are not given any preferences
## We could take threshold = 0.5 

DB_Fitted_Values <- Diabetes_Model$fitted.values
DB_Fitted_Values
summary(DB_Fitted_Values)

DB_Residuals <- Diabetes_Model$residuals
DB_Residuals
summary(DB_Residuals)

fitted.results <- ifelse(Prediction>0.5,1,0)
fitted.results
View(fitted.results)

# Now, Acuuracy -- 2 ways
# 1. use confusion matrix
# 2. using where predicted == actuals

Error <- mean(fitted.results!=Diabetes_Test$Outcome)
print(paste('Accuracy = ',1-Error))

## Gives us ACC : 0.5336225.

# confusion matrix
table(Diabetes_Train$Outcome,(Prediction>0.5))
# accuracy is ((265+90) / (265+90+71+35)) = 0.770065.

## Check ROC Curve

library(ROCR)
ROCRpred <- prediction(Prediction,Diabetes_Train$Outcome)
ROCRperf <- performance(ROCRpred,'tpr','fpr')
plot(ROCRperf,colorize=TRUE)

## Check Accuracy for area under the curve
auc <- performance(ROCRpred,measure = "auc")
auc
auc <- auc@y.values[[1]]
auc

## Gives us the accuracy of 0.8371429.

 #################################################################