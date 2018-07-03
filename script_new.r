
install.packages("RSQLite")
#install.packages("caret", dependencies=c("Depends", "Suggests"))


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

rm(list=ls())

setwd("C:/Users/mariam.daoud/Documents/Daoud/Course dev- applied research-/Certificate in Big Data Analytics- YU/CSDA1010 Basic methods of Data analytics/Lab1")
?read.csv
dataset <- read.table("bank-full.csv", header=TRUE, sep=';')

describe(dataset)
dim(dataset)
str(dataset)


# list types for each attribute
sapply(dataset, class)

# list the levels for the class
levels(dataset$y)

# summarize the class distribution
percentage <- prop.table(table(dataset$y)) * 100
cbind(freq=table(dataset$y), percentage=percentage)

# summarize attribute distributions
summary(dataset)

#################### Class distribution  
#Variable y
#1. Frequency table
attach(dataset)

freq_tbl=table(y)
head(freq_tbl)

#1B. Absolute #s are hard to work with. Let's move to proportion. Input freq table to get prop.
prop.table(freq_tbl)
barplot(prop.table(freq_tbl)*100, main="Class distribution", col=c("darkblue"))



######## AGE ######
hist(dataset$age)

# Stacked Bar Plot with Colors and Legend
counts <- table(dataset$y, dataset$age)
barplot(counts, main="Customer Distribution by age",
        xlab="Age", col=c("darkblue","red"),
        legend = rownames(counts)) 



###### BALANCE #####
hist(dataset$balance)
# Stacked Bar Plot with Colors and Legend
counts <- table(dataset$y, dataset$balance)
barplot(counts, main="Customer Distribution by balance",
        xlab="Balance", col=c("darkblue","red"),
        legend = rownames(counts)) 

###### DAY ######

hist(dataset$day)

# Stacked Bar Plot with Colors and Legend
counts <- table(dataset$y, dataset$day)
barplot(counts, main="Customer Distribution by day",
        xlab="Balance", col=c("darkblue","red"),
        legend = rownames(counts)) 

poutcome_previous_loan <- ggplot(data = dataset, aes(x = poutcome, y = previous, color = y)) + 
  geom_point(alpha = 0.2) +
  scale_y_continuous(limits = c(0,100))
poutcome_previous_loan


#### data preperation and cleaning #####

# remove the duration column 

dataset$duration <- NULL

# Checking missing values (missing values or empty values)
colSums(is.na(dataset)|dataset=='')


################ FEATURE SELECTION 

#library(mlbench)
# Fit a logistic regression model
#fit_glm = glm(y~.,dataset,family = "binomial")
#varImp(fit_glm)

library(randomForest)
fit_rf = randomForest(y~., data=dataset)
# Create an importance based on mean decreasing gini
importance(fit_rf)
# Create a plot of importance scores by random forest
varImpPlot(fit_rf)

#########################################################
###### dataset split 
dt = sort(sample(nrow(dataset), nrow(dataset)*.67))
train<-dataset[dt,]
test<-dataset[-dt,]

testycol<- test$y



dim(train)
dim(test)

########################### Classification using Decision trees 

### tuning the number of variables/col 
#### considering all the attributes 
# month + balance+ age+ day+poutcome+job+pdays+campaign+ education+ previous+ marital+ housing+ contact+ loan+ default 


DT_top3col <- rpart(y ~ month + balance+ age, train, method = "class", control = rpart.control(cp = 0))

DT_top5col <- rpart(y ~ month + balance+ age+ day+poutcome, train, method = "class", control = rpart.control(cp = 0))

DT_top6col <- rpart(y ~ month + balance+ age+ day+poutcome+job, train, method = "class", control = rpart.control(cp = 0))

DT_top9col <- rpart(y ~ month + balance+ age+ day+poutcome+job+pdays+campaign+ education, train, method = "class", control = rpart.control(cp = 0))

DT_top12col <- rpart(y ~ month + balance+ age+ day+poutcome+job+pdays+campaign+ education+ previous+ marital+ housing, train, method = "class", control = rpart.control(cp = 0))


DT_top15col <- rpart(y ~ month + balance+ age+ day+poutcome+job+pdays+campaign+ education+ previous+ marital+ housing+ contact+ loan+ default, train, method = "class", control = rpart.control(cp = 0))

# accuracy on training data 
t_pred <- predict(DT_top6col, train, type = "class")
confMat <- table(train$y,t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

summary(DT_allcol)
printcp(DT)
rpart.plot(DT, type=1, extra = 102)

#[3] 0.8997062
#[5] 0.911954
#[6] 0.9130105
#[9] 0.9146611
#[12] 0.9158826 
#[15] 0.9179294

nbcol = c(3,5,6,9,12,15) 
accuracy = c(0.8997062, 0.911954, 0.9130105, 0.9146611, 0.9158826, 0.9179294) 
df = data.frame(nbcol, accuracy)       # df is a data frame 
df
plot(df)


# ### experiment: build decision tree model with changing the depth and impact on accuracy 
# using top 6 col from feature selection  
DT_top6col <- rpart(y ~ month + balance+ age+ day+poutcome+job, train, method = "class", control = rpart.control(cp = 0))

DT_top6col_depth3 <- rpart(y ~ month + balance+ age+ day+poutcome+job, train, method = "class", control = rpart.control(cp = 0, maxdepth = 3))
DT_top6col_depth5 <- rpart(y ~ month + balance+ age+ day+poutcome+job, train, method = "class", control = rpart.control(cp = 0, maxdepth = 5))
DT_top6col_depth8 <- rpart(y ~ month + balance+ age+ day+poutcome+job, train, method = "class", control = rpart.control(cp = 0, maxdepth = 8))
DT_top6col_depth10 <- rpart(y ~ month + balance+ age+ day+poutcome+job, train, method = "class", control = rpart.control(cp = 0, maxdepth = 10))
DT_top6col_depth15 <- rpart(y ~ month + balance+ age+ day+poutcome+job, train, method = "class", control = rpart.control(cp = 0, maxdepth = 15))
DT_top6col_depth20 <- rpart(y ~ month + balance+ age+ day+poutcome+job, train, method = "class", control = rpart.control(cp = 0, maxdepth = 20))

# accuracy on training data 
t_pred <- predict(DT_top6col, train, type = "class")
confMat <- table(train$y,t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy


# results 
# 5 0.896603
# 8 0.901918
# 10 0.90687
# 15 0.9123172
# 20  0.9130105
# max   0.9130105

#summary(DT_top6col_depth5)
#printcp(DT_top6col_depth5)
rpart.plot(DT_top6col_depth3, type=1, extra = 102)

############################# testing stage 
testycol <- test$y  
test$y<-NULL 
dim(test)
# testing accuracy
dt_pred <- predict(DT_top6col_depth5, test, type = "class")

test$y <- testycol
dim(test)
confMat <- table(test$y,dt_pred)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy
confMat


# results decision trees on testing data 
# 5  0.8896113
# 10 0.8845845
# 15 0.8810992
# 20 0.8770107
# max 0.8770107

############################## RANDOM FOREST classification  
###### dataset split 

rf <- randomForest(y ~ month + balance+ age+ day+poutcome+job,
                    data=train, 
                    importance=TRUE, 
                    ntree=20)

varImpPlot(rf)
plot(rf)

# calculate accuracy on training data

rfPred = predict(rf, newdata=train)
CM = table(rfPred, train$y)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
CM

# calculate accuracy on testing data
dim(test)
testycol<-test$y
test$y<-NULL 
dim(test)
rfPred = predict(rf, newdata=test)
test$y <- testycol

CM = table(rfPred, test$y)
CM
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
#######################################################
#Classsification using Naive Bayes 
#install.packages("e1071")
library(e1071)
#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(y ~., data=train)
#What does the model say? Print the model summary
Naive_Bayes_Model
#Prediction on the training dataset
NB_Predictions_train=predict(Naive_Bayes_Model,train)
#Confusion matrix to check accuracy
CM = table(NB_Predictions_train,train$y)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy

dim(train)
dim(test)
test$y

#Prediction on the testing dataset
testycol <- test$y 
test$y <- NULL 
NB_Predictions_test=predict(Naive_Bayes_Model,test)
# put back the class label column 
test$y <- testycol
#Confusion matrix to check accuracy
CM = table(NB_Predictions_test,test$y)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy

###################Comparison of the different algorithms ######################################
# split the dataset and use the same training and testing samples for all algorithms














############################ END ##################
library(klaR)
### Run algorithms using 10-fold cross validation ####
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(y~., data=dataset, method="rpart", metric=metric, trControl=control)
## naive bayes
set.seed(7)
fit.nb <- train(y~., data=dataset, method="nb", metric=metric, trControl=control)


# Random Forest
set.seed(7)
fit.rf <- train(y~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list( cart=fit.cart,  rf=fit.rf, nb=fit.nb)) #lda=fit.lda, knn=fit.knn, svm=fit.svm,
summary(results)




