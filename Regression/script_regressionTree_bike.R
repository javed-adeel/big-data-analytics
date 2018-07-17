

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
dataset$yr <- NULL
dataset$registered <- NULL
dataset$casual <- NULL

######


########## dataset characteristics #######
describe(dataset)
dim(dataset)
str(dataset)
View(dataset)
attach(dataset)
dim(dataset)


# list types for each attribute
sapply(dataset, class)

# Checking missing values (missing values or empty values)
colSums(is.na(dataset)|dataset=='')

# scatter plots 
pairs(~cnt + season + mnth +holiday + weekday + workingday  )

pairs(~cnt  + weathersit + temp + atemp + hum + windspeed)


################ plots ###################
plot(density(dataset$temp))
plot(density(dataset$hum))
plot(density(dataset$windspeed))

ggplot(dataset,aes(y=temp,x=1))+geom_boxplot() + ylab("temperature")+xlab("")

ggplot(dataset,aes(y=atemp,x=1))+geom_boxplot() + ylab("feel like temp")+xlab("")

ggplot(dataset,aes(y=hum,x=1))+geom_boxplot() + ylab("humidity")+xlab("")

ggplot(dataset,aes(y=windspeed,x=1))+geom_boxplot()
+ ylab("windspeed")+xlab("")

#Several groups defined by a categorical variable
boxplot(windspeed~weathersit,data=dataset,ylab="windspeed", xlab="weathersit")
boxplot(windspeed~season,data=dataset,ylab="windspeed", xlab="season")
boxplot(windspeed~mnth,data=dataset,ylab="windspeed", xlab="month")


boxplot(hum~weathersit,data=dataset,ylab="hum", xlab="weathersit")


#ylim=c(80,250)
#########################################
data_num_subset = dataset[ , c("season" , "mnth", "holiday", "weekday",  "workingday", "weathersit" , "temp", "atemp", "hum",  "windspeed")]
head(data_num_subset)

#Correlation Matrix - default one in R
cor(data_num_subset)
cor_result=rcorr(as.matrix(data_num_subset))
cor_result$r
library(corrplot)
corrplot(cor_result$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

############ data modeling ############
# feature selection : Random forest / importance 

library(randomForest)
fit_rf = randomForest(cnt~., data=dataset)
importance(fit_rf)
varImpPlot(fit_rf)

###### split the dataset ##########
dim(dataset)
set.seed(123)
split = sample.split(dataset$cnt, SplitRatio = 0.7)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
dim(train_set)

# regression trees modeling
# attributes ranked by improtance 
# atemp, temp, hum, season,mnth, windspeed,weathersit, weekday, workingday, holiday


Rtree_fit_top4 <- rpart(cnt~   atemp + temp+ hum+ season, method="anova", data=train_set, control = rpart.control(cp = 0)) 

Rtree_fit_top5 <- rpart(cnt~   atemp + temp+ hum+ season +mnth, method="anova", data=train_set, control = rpart.control(cp = 0))

Rtree_fit_top6 <- rpart(cnt~   atemp + temp+ hum+ season+mnth + windspeed, method="anova", data=train_set, control = rpart.control(cp = 0))

Rtree_fit_top7 <- rpart(cnt~   atemp + temp+ hum+ season+mnth + windspeed +weathersit, method="anova", data=train_set, control = rpart.control(cp = 0))

Rtree_fit_top8 <- rpart(cnt~   atemp + temp+ hum+ season+mnth + windspeed +weathersit +weekday, method="anova", data=train_set, control = rpart.control(cp = 0))

Rtree_fit_top9 <- rpart(cnt~   atemp + temp+ hum+ season+mnth + windspeed +weathersit +weekday +workingday, method="anova", data=train_set, control = rpart.control(cp = 0))

Rtree_fit_top10 <- rpart(cnt~   atemp + temp+ hum+ season+mnth + windspeed +weathersit +weekday +workingday + holiday, method="anova", data=train_set, control = rpart.control(cp = 0))


# Find the best CP 
bestcp <- Rtree_fit_top6$cptable[which.min(Rtree_fit_top6$cptable[,"xerror"]),"CP"]
bestcp

#plotcp(Rtree_fit)

Rtree_fit.pruned <- prune(Rtree_fit_top6, cp = bestcp)

plot(Rtree_fit.pruned, uniform=TRUE, main="Regression Tree for rental bikes")
text(Rtree_fit.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

summary(Rtree_fit.pruned)
#printcp(Rtree_fit.pruned) # display the results 
#plotcp(Rtree_fit.pruned)

# Compute the accuracy of the pruned tree
pred <- predict(Rtree_fit.pruned, newdata = test_set)
RMSE(pred = pred, obs = test_set$cnt)
pred

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=test_set$cnt, predicteds=pred)) 
head(actuals_preds)

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 
mape

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# Min_Max Accuracy => mean(min(actual, predicted)/max(actual, predicted))::  Higher the better
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy


# plot line
plot(test_set$cnt,pred,
     xlab="actual",ylab="predicted")



