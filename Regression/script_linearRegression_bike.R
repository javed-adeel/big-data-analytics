library(plyr)
library(car)
library(ggrepel)
library(plotrix)
library(cowplot)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(readr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(Amelia)
library(mosaic)
library(scales)
library(caTools)

# remove env variables
rm(list=ls())

# set working directory 
setwd("C:/Users/mariam.daoud/Documents/Daoud/Course dev- applied research-/Certificate in Big Data Analytics- YU/CSDA1010 Basic methods of Data analytics/Lab2/Bike-Sharing-Dataset")

# data saved in "day.csv"
dataset <- read.csv("day.csv", header=TRUE)

### creating new binary variables for mutli-valued categorical variables 
dataset$instant <- NULL
dataset$dteday <- NULL
dataset$atemp <- NULL
dataset$registered <- NULL
dataset$casual <- NULL
dataset$yr <- NULL


######


dataset$jan [dataset$mnth == "1"] <- "1"
dataset$jan [dataset$mnth != "1"] <- "0"

dataset$feb [dataset$mnth == "2"] <- "1"
dataset$feb [dataset$mnth!= "2"] <- "0"

dataset$mar [dataset$mnth == "3"] <- "1"
dataset$mar [dataset$mnth!= "3"] <- "0"

dataset$apr [dataset$mnth == "4"] <- "1"
dataset$apr [dataset$mnth!= "4"] <- "0"

dataset$may [dataset$mnth == "5"] <- "1"
dataset$may [dataset$mnth!= "5"] <- "0"

dataset$june [dataset$mnth == "6"] <- "1"
dataset$june [dataset$mnth!= "6"] <- "0"

dataset$jul [dataset$mnth == "7"] <- "1"
dataset$jul [dataset$mnth!= "7"] <- "0"

dataset$aug [dataset$mnth == "8"] <- "1"
dataset$aug [dataset$mnth!= "8"] <- "0"

dataset$sept [dataset$mnth == "9"] <- "1"
dataset$sept [dataset$mnth!= "9"] <- "0"

dataset$oct [dataset$mnth == "10"] <- "1"
dataset$oct [dataset$mnth!= "10"] <- "0"

dataset$nov [dataset$mnth == "11"] <- "1"
dataset$nov [dataset$mnth!= "11"] <- "0"

dataset$dec [dataset$mnth == "12"] <- "1"
dataset$dec [dataset$mnth != "12"] <- "0"

dataset$mnth <- NULL

#######

dataset$weather1 [dataset$weathersit == "1"] <- "1"
dataset$weather1 [dataset$weathersit != "1"] <- "0"

dataset$weather2 [dataset$weathersit == "2"] <- "1"
dataset$weather2 [dataset$weathersit != "2"] <- "0"

dataset$weather3 [dataset$weathersit == "3"] <- "1"
dataset$weather3 [dataset$weathersit != "3"] <- "0"

dataset$weather4 [dataset$weathersit == "4"] <- "1"
dataset$weather4 [dataset$weathersit != "4"] <- "0"

dataset$weathersit <- NULL

######


dataset$mon [dataset$weekday == "0"] <- "1"
dataset$mon [dataset$weekday != "0"] <- "0"

dataset$tue [dataset$weekday == "1"] <- "1"
dataset$tue [dataset$weekday != "1"] <- "0"

dataset$wed [dataset$weekday == "2"] <- "1"
dataset$wed [dataset$weekday != "2"] <- "0"

dataset$thur [dataset$weekday == "3"] <- "1"
dataset$thur [dataset$weekday != "3"] <- "0"

dataset$fri [dataset$weekday == "4"] <- "1"
dataset$fri [dataset$weekday != "4"] <- "0"

dataset$sat [dataset$weekday == "5"] <- "1"
dataset$sat [dataset$weekday != "5"] <- "0"

dataset$sun [dataset$weekday == "6"] <- "1"
dataset$sun [dataset$weekday != "6"] <- "0"

dataset$weekday <- NULL

attach(dataset)

########## dataset characteristics #######
describe(dataset)
dim(dataset)
str(dataset)
View(dataset)

# list types for each attribute
sapply(dataset, class)

# Checking missing values (missing values or empty values)
colSums(is.na(dataset)|dataset=='')

############# relationships between variables : scatter plots #######
# we want only numerical attributes for scatter plots 
# exclude categorical / non meaningful attributes : 
# instant: Id of each instance  
# dteday: actual date, month and year are already defined as seperate attributes. 
# the following two attributes casual and registered add up to the total daily count of rental bikes (cnt)
# as our goal is to predict the total daily count of rental bikes (cnt), we exclude those two attributes from scatter plots.  
# casual: count of casual users
# registered: count of registered users

# list of attributes 
# instant + dteday 
# season + yr + mnth + holiday + weekday + workingday  + weathersit + temp +     atemp + hum + windspeed + casual + registered + cnt
# we also remove yr as it corresponds to 0: 2011, 1:2012. This is consisdered categorical attribute and cannot be considered as a predictor to the count of rental bikes. 

pairs(~cnt + season + mnth +holiday + weekday + workingday  )

pairs(~cnt  + weathersit + temp + atemp + hum + windspeed)

# It is obvious that there is a linear relationship between the attribute temperature and atemperature. here is how these attributes are derived: 
#temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
# atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
# This is referred as Multicollinearity. As regression modeling is based on the assumption that There must be no correlation among independent variables, we need to romove one of these two variables. 
# we decide to remove atemperature attribute. 

################ plots ###################
plot(density(dataset$temp))
plot(density(dataset$hum))
plot(density(dataset$windspeed))

data_num_subset = dataset[ , c("temp", "hum", "windspeed")]
head(data_num_subset)

#Correlation Matrix - default one in R
cor(data_num_subset)

#correlation matrix with statistical significance
cor_result=rcorr(as.matrix(data_num_subset))
cor_result$r

############ data modeling ############
###### split the dataset ##########
dim(dataset)
set.seed(123)
split = sample.split(dataset$cnt, SplitRatio = 0.7)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

########### regression modeling ##############
### STEP 1:  Develop the model on the training data  and use it to predict the cnt  on test data
# src: http://r-statistics.co/Linear-Regression.html 

# Stepwise Regression
library(MASS)
fit <- lm(cnt ~ .,data=train_set)
step <- stepAIC(fit, direction="both")
step$anova # display results


# use all variables as indicators 
lm.fit1 <- lm(cnt ~ season + mnth + holiday + weekday + workingday + weathersit + temp + hum+ windspeed, data=train_set)
summary(lm.fit1)


# Global test of model assumptions
#install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(lm.fit1) 
summary(gvmodel)


# Normality of Residuals
# qq plot for studentized resid
qqPlot(lm.fit1, main="QQ Plot")

# plot studentized residuals vs. fitted values 
spreadLevelPlot(lm.fit1)

# Assessing Outliers
outlierTest(lm.fit1) # Bonferonni p-value for most extreme obs

# model 2 
# remove workingday, month
lm.fit2 <- lm(cnt ~ season  + holiday + weekday  + weathersit + temp + hum+ windspeed, data=train_set)
summary(lm.fit2)

# Normality of Residuals
# qq plot for studentized resid
qqPlot(lm.fit2, main="QQ Plot")

# plot studentized residuals vs. fitted values 
spreadLevelPlot(lm.fit2)

# model 3



####################################
##### comparison of the regression models on the train/test data 

lm_predicted = predict(dataset.fit2, newdata = train_set, interval = "confidence")
lm_predicted

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=train_set$cnt, predicteds=lm_predicted)) 
head(actuals_preds)

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

#??? Min_Max Accuracy => mean(min(actual, predicted)/max(actual, predicted))::  Higher the better
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy

# MeanAbsolutePercentageError (MAPE): Lower the better
mape <- mean(abs((actuals_preds$fit - actuals_preds$actuals))/actuals_preds$actuals) 
mape

#par(mfrow = c(2, 2))
#plot(m)

#https://www.ritchieng.com/machine-learning-evaluate-linear-regression-model/


#https://www.ritchieng.com/machine-learning-evaluate-linear-regression-model/