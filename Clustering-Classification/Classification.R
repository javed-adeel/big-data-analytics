#### Channel Split
df=dataset

df$log_shares=NULL

## business data channel
dfb=subset(df,df$data_channel_is_bus==1)
dfb$data_channel_is_lifestyle=NULL
dfb$data_channel_is_entertainment=NULL
dfb$data_channel_is_tech=NULL
dfb$data_channel_is_world=NULL
dfb$data_channel_is_socmed=NULL


## life style data channel
dfl=subset(df,df$data_channel_is_lifestyle==1)
dfl$data_channel_is_bus=NULL
dfl$data_channel_is_entertainment=NULL
dfl$data_channel_is_tech=NULL
dfl$data_channel_is_world=NULL
dfl$data_channel_is_socmed=NULL

## entertainment data channel
dfe=subset(df,df$data_channel_is_entertainment==1)
dfe$data_channel_is_bus=NULL
dfe$data_channel_is_lifestyle=NULL
dfe$data_channel_is_tech=NULL
dfe$data_channel_is_world=NULL
dfe$data_channel_is_socmed=NULL

## tech data channel
dft=subset(df,df$data_channel_is_tech==1)
dft$data_channel_is_bus=NULL
dft$data_channel_is_lifestyle=NULL
dft$data_channel_is_entertainment=NULL
dft$data_channel_is_world=NULL
dft$data_channel_is_socmed=NULL

## world data channel
dfw=subset(df,df$data_channel_is_world==1)
dfw$data_channel_is_bus=NULL
dfw$data_channel_is_lifestyle=NULL
dfw$data_channel_is_entertainment=NULL
dfw$data_channel_is_tech=NULL
dfw$data_channel_is_socmed=NULL

## social media data channel
dfs=subset(df,df$data_channel_is_socmed==1)
dfs$data_channel_is_bus=NULL
dfs$data_channel_is_lifestyle=NULL
dfs$data_channel_is_entertainment=NULL
dfs$data_channel_is_tech=NULL
dfs$data_channel_is_world=NULL

table(dfb$pop2)
table(dfb$popularity)


table(dfl$pop2)
table(dfl$popularity)


table(dfe$pop2)
table(dfe$popularity)


table(dft$pop2)
table(dft$popularity)


table(dfw$pop2)
table(dfw$popularity)


table(dfs$pop2)
table(dfs$popularity)





## CLASSIFICATION BASED ON DATA CHANNEL SPLIT TO PREDICT-UNPOPULAR and POPULAR TWEETS

##                         VARIABLES DESCRIPTION: 
## POPULARITY: UNPOPULAR/NORMAL/POPULAR TWEET COLUMN
## dfb: BUSINESS CHANNEL DATAFRAME
## dfl: LIFESTYLE CHANNEL DATAFRAME
## dfe: ENTERTAINMENT CHANNEL DATAFRAME
## dft: TECHNOLOGY CHANNEL DATAFRAME
## dfw: WORLD CHANNEL DATAFRAME
## dfs: SCIENCE CHANNEL DATAFRAME
## df: original dataframe
                          ## TRAIN-TEST SPLIT
#dftrain=slice(df,1:round(nrow(df)*0.7))
#dftest=slice(df,nrow(dftrain)+1:nrow(df))

dt=sort(sample(nrow(df), nrow(df)*.67))
dftrain=df[dt,]
dftest=df[-dt,]



                          ## DECISION TREE
#ktrain=trainControl(method = "cv",number=10)
#kdt=train(popularity~.,dftrain,trControl=ktrain,method="rpart")
kdt=rpart(popularity~.-shares,dftrain,method="class",control=rpart.control(maxdepth=4,cp=0.001))
rpart.plot(kdt,type=1,extra=102)

## PREDICT Y ON TRAIN SAMPLE
dt_ytrain_pred=predict(kdt,dftrain,type="class")

## TRAIN SAMPLE-CONFUSION MATRIX AND ACCURACY
confMAT=table(dt_ytrain_pred,dftrain$popularity)
confMAT
diag(confMAT)/sum(confMAT)

## PREDICT Y ON TEST SAMPLE
dt_ytest_pred=predict(kdt,dftest,type="class")

## TEST SAMPLE-CONFUSION MATRIX AND ACCURACY
konfmat=table(dt_ytest_pred,dftest$popularity)
konfmat
diag(konfmat)/sum(konfmat)


                              ## RANDOM FOREST
krf=randomForest(popularity~.-shares,dftrain,importance=TRUE,ntree=20)

## PREDICT Y ON TRAIN SAMPLE
rf_ytrain_pred=predict(krf,dftrain)

## TRAIN SAMPLE-CONFUSION MATRIX AND ACCURACY
confMAT=table(rf_ytrain_pred,dftrain$popularity)
confMAT
diag(confMAT)/sum(confMAT)

## PREDICT Y ON TEST SAMPLE
rf_ytest_pred=predict(krf,dftest)

## TEST SAMPLE-CONFUSION MATRIX AND ACCURACY
konfmat=table(rf_ytest_pred,dftest$popularity)
konfmat
diag(konfmat)/sum(konfmat)



#### NAIVE
nb=naiveBayes(popularity~.-shares,dftrain)

#Prediction on the training dataset
nb_ytrain_predict=predict(nb,dftrain)

#Confusion matrix to check accuracy
CM = table(nb_ytrain_predict,dftrain$popularity)
CM
accuracy = diag(CM)/sum(CM)
accuracy

#Prediction on the testing dataset
nb_ytest_predict=predict(nb,dftest)

CM = table(nb_ytest_predict,dftest$popularity)
CM
accuracy = diag(CM)/sum(CM)
accuracy