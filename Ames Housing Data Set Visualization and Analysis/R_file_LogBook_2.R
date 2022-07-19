library(readxl)
library(dplyr)
library(caret) #to split the data
library(Hmisc) #For rcorr() function 
library(corrplot)
library(lmtest)
library(car)

setwd('D:/Business Analytics/Statistics For Business/Log Book 2')
train <- read_excel('ames_train.xlsx')
test <- read_excel('ames_test.xlsx')


#Comine the train and test data into AMES
ames <- rbind(train,test)

options(scipen = 10000) #To remove 10E values

#:::::: PRE process the ames data AND Data Quality ISSUES:::::

#Change the Kitchen.Qual conditions to Number
ames$Kitchen.Qual[ames$Kitchen.Qual == 'Ex'] <- 5
ames$Kitchen.Qual[ames$Kitchen.Qual == 'Gd'] <- 4
ames$Kitchen.Qual[ames$Kitchen.Qual == 'TA'] <- 3
ames$Kitchen.Qual[ames$Kitchen.Qual == 'Fa'] <- 2
ames$Kitchen.Qual[ames$Kitchen.Qual == 'Po'] <- 1
ames$Kitchen.Qual[ames$Kitchen.Qual == 'NA'] <- 0
ames$Kitchen.Qual[is.na(ames$Kitchen.Qual)] <- 0

#Change the Bsmt.Qual conditions to Number
ames$Bsmt.Qual[ames$Bsmt.Qual == 'Ex'] <- 5
ames$Bsmt.Qual[ames$Bsmt.Qual == 'Gd'] <- 4
ames$Bsmt.Qual[ames$Bsmt.Qual == 'TA'] <- 3
ames$Bsmt.Qual[ames$Bsmt.Qual == 'Fa'] <- 2
ames$Bsmt.Qual[ames$Bsmt.Qual == 'Po'] <- 1
ames$Bsmt.Qual[ames$Bsmt.Qual == 'NA'] <- 0
ames$Bsmt.Qual[is.na(ames$Bsmt.Qual)] <- 0

#Remove the outliers which are above 4000 with the mean value
ames$Gr.Liv.Area[ames$Gr.Liv.Area > 4000] <- mean(ames$Gr.Liv.Area)

#All the NAs are converted into 0 for Lot.Frontage
ames$Lot.Frontage[is.na(ames$Lot.Frontage)] <- 0

#Neighborhood are converted into factors
ames$Neighborhood <- as.factor(ames$Neighborhood)

#Change the rating for Overall.Qual which consists of 11 to median value and factor
ames$Overall.Qual[ames$Overall.Qual == 11] <- median(ames$Overall.Qual, na.rm = TRUE)
ames$Overall.Qual <- as.factor(ames$Overall.Qual)

#Convert it into factor
ames$Overall.Cond <- as.factor(ames$Overall.Cond)

#Change Year.Built consisting 999 with median year
ames$Year.Built[ames$Year.Built == 999] <- median(ames$Year.Built)
 
#NA to 0
ames$Total.Bsmt.SF[is.na(ames$Total.Bsmt.SF)] <- 0
#For Total.Bsmt.SF values above 2000 are converted into mean value
ames$Total.Bsmt.SF[ames$Total.Bsmt.SF > 2000] <- mean(ames$Total.Bsmt.SF, na.rm = TRUE)

#Convert Bedroom.AbvGr into factor 
ames$Bedroom.AbvGr <- as.factor(ames$Bedroom.AbvGr)

#Convert the NA in Garage.Cars to median value
ames$Garage.Cars[is.na(ames$Garage.Cars)] <- median(ames$Garage.Cars, na.rm = TRUE)
ames$Garage.Cars <- as.factor(ames$Garage.Cars)


#Garage area ABOVE 900 are changed to mean value
ames$Garage.Area[ames$Garage.Area > 900] <- mean(ames$Garage.Area , na.rm = TRUE)
ames$Garage.Area[is.na(ames$Garage.Area)] <- mean(ames$Garage.Area , na.rm = TRUE)

#TotRms.AbvGrd into factor
ames$TotRms.AbvGrd <- as.factor(ames$TotRms.AbvGrd)

#Re-summarize the Saleprice to MEAN above 780K USD
ames$SalePrice[ames$SalePrice > 780000] <- mean(ames$SalePrice, na.rm = TRUE)

#:::: RELATIONSHIPS BETWEEN DIFFERENT VARIABLES ::::::

#Change few attributes in numerical for corrplot
ames$Overall.Qual <- as.numeric(ames$Overall.Qual)
ames$Garage.Cars <- as.numeric(ames$Garage.Cars)
ames$Kitchen.Qual <- as.numeric(ames$Kitchen.Qual)
ames$Bsmt.Qual <- as.numeric(ames$Bsmt.Qual)


#Subset and Visualising Correlation Using Corrplot for multiple attributes
subdata<- ames[c("SalePrice","Overall.Qual","Gr.Liv.Area", "Kitchen.Qual", "Garage.Cars", "Garage.Area")]
cor <- cor(subdata)
cor_sort <- as.matrix(sort(cor[,'SalePrice'], decreasing = TRUE))
corrplot.mixed(cor, tl.col="black", tl.pos="lt")

#Relationship between Overall.Qual and Sale Price with p-value and confidence interval
cor.test(x=ames$Overall.Qual, y=ames$SalePrice)

#Relationship between Living Area and Sale Price with p-value and confidence interval
cor.test(x=ames$Gr.Liv.Area, y=ames$SalePrice)

#Relationship between Garage Cars and Sale Price with p-value and confidence interval
cor.test(x=ames$Garage.Cars, y=ames$SalePrice)

#Relationship between First floor area and Sale Price with p-value and confidence interval
cor.test(x=ames$X1st.Flr.SF, y=ames$SalePrice)

#Relationship between Basement area and Sale Price with p-value and confidence interval
cor.test(x=ames$Total.Bsmt.SF, y=ames$SalePrice)


#::::: SPLIT THE AMES DATA INTO TRAINING AND TEST::::

#Delete TEST and TRAIN data first, it would set it as empty
test <- NULL
train <- NULL

#to create a partition with 80%
set.seed(123) #generate a sequence of random numbers
index <- createDataPartition(ames$SalePrice, p = 0.8, list = FALSE,)

train <- ames[index, ] #first 80% for training  
test <- ames[-index, ] #bottom 20% for testing

#           ::: BUILD THE MODEL :::

#1. :::: Simple Linear Regression MODEL ::::

simple_model <- lm(SalePrice ~ Gr.Liv.Area, data = train)

#Summary of Simple Linear Regression MODEL
summary(simple_model)

#prediction using the model
simple_price_prediction <- predict(simple_model, newdata = test)

#RMSE is the difference between observed and predicted values calculated as:

sqrt(mean((simple_price_prediction - test$SalePrice)^2))
postResample(pred = simple_price_prediction, obs = test$SalePrice)

#2. ::::Multiple Linear Regression MODEL 1::::

Model1 <- lm(SalePrice ~ as.factor(Overall.Qual) + Gr.Liv.Area + as.factor(Garage.Cars) + X1st.Flr.SF + Year.Built, data = train)

#review the model
summary(Model1)

#prediction using the model
price_prediction_1 <- predict(Model1, newdata = test)

#evaluate the accuracy of the predictions
#(i.e. difference between the actual sale value and the predicted sale value)
postResample(pred = price_prediction_1, obs = test$SalePrice)

#Durbin-Watson test 
dwtest(Model1)

#Cooks distance
diag1<- (train[c("SalePrice","Overall.Qual","Gr.Liv.Area","Garage.Cars","X1st.Flr.SF", "Year.Built")])
diag1$residuals <- resid(Model1)
diag1$standardized_residuals<- rstandard(Model1)
diag1$cooks_distance <- cooks.distance(Model1)
diag1$dfbeta <- dfbeta(Model1)
diag1$dffits <- dffits(Model1)
diag1$leverage <- hatvalues(Model1)
diag1$covariance_ratios <- covratio(Model1)

diag1$large_residual <- diag1$standardized_residuals > 2 | diag$standardized_residuals < -2
sum(diag1$large_residual)

#No MultiColinearity
vif(Model1)
mean(vif(Model1))


#2.   ::::Multiple Regression Model 2::::

Model2 <- lm(SalePrice ~ as.factor(Overall.Qual) + Gr.Liv.Area + Garage.Area + X1st.Flr.SF + Year.Built + Year.Remod.Add + Total.Bsmt.SF + as.factor(Kitchen.Qual) + Lot.Area + as.factor(Bsmt.Qual) , data = train)

#review the model
summary(Model2)

#prediction using the model
price_prediction_2 <- predict(Model2, newdata = test)

#evaluate the accuracy of the predictions
#(i.e. difference between the actual sale value and the predicted sale value)
postResample(pred = price_prediction_2, obs = test$SalePrice)

#Durbin-Watson test 
dwtest(Model2)

#Cooks distance
diag<- train
diag$residuals <- resid(Model2)
diag$standardized_residuals<- rstandard(Model2)
diag$cooks_distance <- cooks.distance(Model2)
diag$dfbeta <- dfbeta(Model2)
diag$dffits <- dffits(Model2)
diag$leverage <- hatvalues(Model2)
diag$covariance_ratios <- covratio(Model2)

diag$large_residual <- diag$standardized_residuals > 2 | diag$standardized_residuals < -2
sum((diag$large_residual))

#No MultiColinearity
vif(Model2)
mean(vif(Model2))
