#Install the required packages
#Read the Packages
library(readxl) 
library(psych)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret) #to split the data
library(Hmisc) #For rcorr() function 
library(corrplot)
library(lmtest)
library(car)

#Set Workind Directory
setwd('D:/Business Analytics/Statistics For Business/Assignment 2')

#Read the excel sheet into variable test and train
test <- bank_test
train <- bank_train

#Combine the train and test data into BANK
bank <- rbind(train,test)

#To remove 10E values
options(scipen = 10000)

# :: Summarize the Data ::
#Analyze the Columns with NA
colSums(is.na(bank))

#Summarise the data
  summary(bank)

#::::: Data Quality Issues and Action ::::::

#AGE: Maximum Age is 170 and minimmum is 3. Change the age at appropriate value.
#Re summarize Age above 98 and below 17 as mean value
bank$age[(bank$age > 98)] <- mean(bank$age)
bank$age[(bank$age < 17)] <- mean(bank$age)

#Convert job into factor
bank$job <- as.factor(bank$job)

#Convert marital into factor
bank$marital <- as.factor(bank$marital)

#Convert education into  factor and Combine basic education
bank$education[bank$education == 'basic.4y'] <- 'basic'
bank$education[bank$education == 'basic.6y'] <- 'basic'
bank$education[bank$education == 'basic.9y'] <- 'basic'
bank$education <- as.factor(bank$education)

#In Contact change the name of 1 Mobile to cellular and contact as factor
bank$contact[bank$contact == 'mobile'] <- 'cellular'
bank$contact <- as.factor(bank$contact)

#Convert housing into factor
bank$housing <- as.factor(bank$housing)

#Convert loan into factor and change the name pf NA's to unknown
bank$loan <- as.factor(bank$loan)
bank$loan[is.na(bank$loan)] <- 'unknown'

#Convert Month in factor and level-up in sequence to show proper interpretation 
bank$month <- as.factor(bank$month)
bank$month <- factor(bank$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), ordered = TRUE)

#Convert day_of_week in factor and level-up in sequence to show proper interpretation (it consists 83 NA's)
bank$day_of_week <- as.factor(bank$day_of_week)
bank$day_of_week <- factor(bank$day_of_week, levels = c("mon", "tue", "wed", "thu", "fri"), ordered = TRUE)

#Convert y variable into factor as 'yes' or 'no'
bank$y[bank$y == 'yes'] <- 'Subscribed'
bank$y[bank$y == 'no'] <- 'Not Subscribed'
bank$y <- as.factor(bank$y)

#bank with y column as yes
bank %>% filter(y == 'Subscribed') -> yes

#::: Descriptive Statistics :::

#1. descriptive statistics of jobs of customers with respect to age.
describeBy(x = bank$age, group = bank$job)

#2. Credit Default with respect to Marketing Campaign duration
describeBy(x = bank$duration, group = bank$default, na.rm = TRUE)

#3. Subscribed Customers with respect to Marketing Campaign duration 
describeBy(x = bank$duration, group = bank$y, na.rm = TRUE)

#4. Customers who have taken Housing and Personal Loan
summary(bank$loan)
summary(bank$housing)

#5. Customer Count of Marital status
summary(bank$marital)

#6. Eduaction status count of customer
summary(bank$education)

#:: GGPLOT VISUALISATIONS::

#1. Customers subscribed for term deposit with respect to profession (BAR)
yes %>% ggplot(aes(x=job,,fill = job))+
  geom_bar()+
  labs(title = "CUSTOMERS SUBSCRIBED FOR TERM DEPOSIT", x="Profession", ,
       y= "Customer Count", fill = 'Jobs')+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#2. subscribed customer with term deposit with marital status and age 
yes %>% ggplot(aes(x = age, y=duration, color = marital),stat = "Summary", fun.y = "mean")+
  geom_point()+
  labs(title = "Duration of Call with Age and Marital Status", x="Age of Customer", y= "Duration of Call", color = "Marital Status")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
scale_fill_brewer(palette = "Dark2")

#3. subscribed customer with term deposit and education background
yes %>% ggplot(aes(x=education))+
  geom_bar()+
  labs(title = "CUSTOMERS SUBSCRIBED WITH TERM DEPOSIT", x="Education",
       y= "Customer Count")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#4.subscribed customer with term deposit and Months 
  yes %>% ggplot(aes(x=month, fill = month))+
  geom_bar()+
  labs(title = "CUSTOMERS SUBSCRIBED WITH TERM DEPOSIT", x="Month",
       y= "Customer Count", fill = 'Months')+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#5. Customers who have taken personal loan with respect to subscription
  bank %>% ggplot(aes(x = loan, fill = y))+
    geom_bar()+
    labs(title = "Personal Loan Vs Customers Count", x="Peresonal Loan", y= "Customer Count", fill = "Subscription")+
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  scale_fil_brewer(palette = "Dark2")
  
#::: MEASURES OF ASSOSCIATION::::
#Chi Square Tests in R
  
#1.cross tabs for the variables MONTH
table(bank$y,bank$month)
#chisq.test() function to perform the test
chisq.test(bank$y, bank$month, correct = FALSE) 

#2.cross tabs for the variables Outcome of previous Marketing
table(bank$y,bank$poutcome)
#chisq.test() function to perform the test
chisq.test(bank$y, bank$poutcome, correct = FALSE) 

#3.cross tabs for the variables MONTH
table(bank$y,bank$marital)
#chisq.test() function to perform the test
chisq.test(bank$y, bank$marital, correct = FALSE)   

#4. cross tabs for the variables MONTH
table(bank$y,bank$contact)
#chisq.test() function to perform the test
chisq.test(bank$y, bank$contact, correct = FALSE) 

#5. Relationship between Consumer price index and Consumer confidence index 
cor.test(x=bank$cons.conf.idx, y=bank$cons.price.idx)
    
#::::: SPLIT THE BANK DATA INTO TRAINING AND TEST::::
  #Delete TEST and TRAIN data first, it would set it as empty
  test <- NULL
  train <- NULL
  #to create a partition with 80%
  bank <- bank %>% filter(!is.na(day_of_week))
  bank<- bank %>% mutate_if(is.character, as.factor)
  set.seed(123) #generate a sequence of random numbers
  index <- createDataPartition(bank$y, p = 0.8, list = FALSE,)
  train <- bank[index, ] #first 80% for training 
  test <- bank[-index, ] #bottom 20% for testing
  

  # ::: BUILD THE MODEL :::
  
#1. :::: Logistic Regression MODEL 1 ::::
formula1 <- y ~ default + contact + poutcome
model1 <- glm(formula1, data = train, family = "binomial")
#Summary of Logistic Regression MODEL 1
summary(model1)
#prediction using the model
predictions1 <- predict(model1,test,type ="response")
#Convert probabilities to yes or no
class_pred1 <-as.factor(ifelse(predictions1 > 0.5,"yes","no"))
#evaluate the accuracy of the predictions
  postResample(class_pred1,test$y)
  
#Confusion Matrix
  confusionMatrix(class_pred1, test$y)
  
  
#2. :::: Logistic Regression MODEL 2 ::::
  formula2 <- y ~ default + contact + poutcome + month + duration + emp.var.rate
  model2 <- glm(formula2, data = train, family = "binomial")
  #Summary of Logistic Regression MODEL 2
  summary(model2)
  #prediction using the model
  predictions2 <- predict(model2,test,type ="response")
  #Convert probabilities to yes or no
  class_pred2<-as.factor(ifelse(predictions2 > 0.5,"yes","no"))
  #evaluate the accuracy of the predictions
  postResample(class_pred2,test$y)
  
  #Confusion Matrix
  confusionMatrix(class_pred2, test$y)
  
  #3. :::: Logistic Regression MODEL 3 ::::  
  Formula3 <- y ~ default + contact + poutcome + month + duration + emp.var.rate + job + euribor3m + nr.employed
  model3 <- glm(Formula3, data = train, family = "binomial")
  #Summary of Logistic Regression MODEL 3
  summary(model3)
  #prediction using the model
  Predictions3 <- predict(model3,test,type ="response")
  #Convert probabilities to yes or no
  class_pred3<-as.factor(ifelse(Predictions3 > 0.5,"yes","no"))
  #evaluate the accuracy of the predictions
  postResample(class_pred3,test$y)
  
  #Confusion Matrix
  confusionMatrix(class_pred3, test$y)
  
  
  
  
  
  
#Assessing Model R-Square
  logisticPseudoR2s <- function(LogModel) {
    dev <- LogModel$deviance 
    nullDev <- LogModel$null.deviance 
    modelN <- length(LogModel$fitted.values)
    R.l <-  1 -  dev / nullDev
    R.cs <- 1- exp ( -(nullDev - dev) / modelN)
    R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
    cat("Pseudo R^2 for logistic regression\n")
    cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
    cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
    cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
  }
  logisticPseudoR2s(model1)
  logisticPseudoR2s(model2)
  logisticPseudoR2s(model3)
#Odds Ratio (Exponential of coefficient)  
  exp(model1$coefficients)
  exp(model2$coefficients)
  exp(model3$coefficients)
#confidence interval
  exp(confint(model1))
  exp(confint(model2))
  exp(confint(model3))
  
  
#:::::evaluate the model assumption::::
  
#::MODEL 1 ASSUMPTIONS::  
  #Add the predicted probabilities to the data frame
  train$predictedProbabilities <- fitted(model1)
  
  #This shows the probability of churn, and the actual outcome.
  head(data.frame(train$predictedProbabilities, train$y))
  
  #Add the standardised and Studentised residuals can be added to the data frame
  train$standardisedResiduals <- rstandard(model1)
  train$studentisedResiduals <- rstudent(model1)
  
  #count the residuals above 1.96
  sum(train$standardisedResiduals > 1.96)
  
  #COOKs Distance 
  train$cook <- cooks.distance(model1)
  sum(train$cook > 1)

  train$leverage <- hatvalues(model1)
  #check if any values are above 0.0009
  sum(train$leverage > 0.0009)

  #VIF to identify if there is a potential problem with multicolinearity
  vif(model1)
  
  #::MODEL 2 ASSUMPTIONS::  
  #Add the predicted probabilities to the data frame
  train$predictedProbabilities <- fitted(model2)
  
  #This shows the probability of churn, and the actual outcome.
  head(data.frame(train$predictedProbabilities, train$y))
  
  #Add the standardised and Studentised residuals can be added to the data frame
  train$standardisedResiduals <- rstandard(model2)
  train$studentisedResiduals <- rstudent(model2)
  
  #count the residuals above 1.96
  sum(train$standardisedResiduals > 1.96)
  
  #COOKs Distance 
  train$cook <- cooks.distance(model2)
  sum(train$cook > 1)
  
  train$leverage <- hatvalues(model2)
  #check if any values are above 0.0009
  sum(train$leverage > 0.0009)
  
  #VIF to identify if there is a potential problem with multicolinearity
  vif(model2)
  
  #::MODEL 3 ASSUMPTIONS::  
  #Add the predicted probabilities to the data frame
  train$predictedProbabilities <- fitted(model3)
  
  #This shows the probability of churn, and the actual outcome.
  head(data.frame(train$predictedProbabilities, train$y))
  
  #Add the standardised and Studentised residuals can be added to the data frame
  train$standardisedResiduals <- rstandard(model3)
  train$studentisedResiduals <- rstudent(model3)
  
  #count the residuals above 1.96
  sum(train$standardisedResiduals > 1.96)
  
  #COOKs Distance 
  train$cook <- cooks.distance(model3)
  sum(train$cook > 1)
  
  train$leverage <- hatvalues(model3)
  #check if any values are above 0.0009
  sum(train$leverage > 0.0009)
  
  #VIF to identify if there is a potential problem with multicolinearity
  vif(model3)
  

  