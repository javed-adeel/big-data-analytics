# install.packages("RSQLite")
# install.packages("reshape2")
# install.packages("ggplot2")
# install.packages("Hmisc")
# install.packages("corrplot")
# install.packages("mice")
# install.packages("VIM")
# install.packages("pROC")
# install.packages("caret")
# install.packages("sqldf")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("Amelia")
# install.packages("mosaic")
# install.packages("scales")
# install.packages("cowplot")
# install.packages("plotrix")
# install.packages("ggrepel")
# install.packages("caTools")
# install.packages("outliers")
# install.packages("MASS")
# install.packages("rAverage")
# install.packages("tcltk")


library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(RSQLite)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(Amelia)
library(corrgram)
library(mlbench)
library(randomForest)
library(e1071)
library(klaR)
library(car)
library(gridExtra)
library(outliers)
library(MASS)
library(cowplot)

setwd("/Users/Eshaal/Documents/York University/Big Data Analytics/csda1010/R")
dataset <- read.csv("./Project/OnlineNewsPopularity.csv", sep=",")


dataset$url <- NULL
dataset$timedelta <- NULL
dataset$n_tokens_content <- NULL
###################################   Dataset Cleaning  ################################################


newdata <- dataset

colnames( dataset )

mycols <- c("n_tokens_title","n_tokens_content", "n_unique_tokens", "n_non_stop_words", "n_non_stop_unique_tokens", "num_hrefs", "num_self_hrefs", "num_imgs", "num_videos", "average_token_length", "num_keywords", "kw_min_min", "kw_max_min", "kw_avg_min", "kw_min_max", "kw_max_max",  "kw_avg_max",  "kw_min_avg", "kw_max_avg", "kw_avg_avg", "self_reference_min_shares", "self_reference_max_shares", "self_reference_avg_sharess" )

boxplot(newdata$kw_min_min)
summary(newdata$kw_min_min)

newdata <- dataset[mycols]


######### Removing Extreme Outliers ###########
boxplot(newdata$n_unique_tokens) 
boxplot(newdata$n_non_stop_words)
boxplot(newdata$n_non_stop_unique_tokens)
boxplot(newdata$num_hrefs)
boxplot(newdata$num_self_hrefs)
boxplot(newdata$num_imgs)
boxplot(newdata$num_videos)
boxplot(newdata$average_token_length)
boxplot(newdata$num_keywords)



newdata$n_unique_tokens[newdata$n_unique_tokens > 3*sd(newdata$n_unique_tokens,na.rm=FALSE) | newdata$n_unique_tokens < 0.1] <- median(newdata$n_unique_tokens)
newdata$n_non_stop_words[newdata$n_non_stop_words > 3*sd(newdata$n_non_stop_words,na.rm=FALSE) | newdata$n_non_stop_words < 0.1] <- median(newdata$n_non_stop_words)
newdata$n_non_stop_unique_tokens[newdata$n_non_stop_unique_tokens > 3*sd(newdata$n_non_stop_unique_tokens,na.rm=FALSE) | newdata$n_non_stop_unique_tokens < 0.1] <- median(newdata$n_non_stop_unique_tokens)
newdata$num_hrefs[newdata$num_hrefs > 3*sd(newdata$num_hrefs,na.rm=FALSE) | newdata$num_hrefs < 0.1] <- median(newdata$num_hrefs)
newdata$num_self_hrefs[newdata$num_self_hrefs > 70] <- median(newdata$num_self_hrefs)
newdata$num_imgs[newdata$num_imgs > 105] <- median(newdata$num_imgs)
newdata$num_videos[newdata$num_videos > 80] <- median(newdata$num_videos)
newdata$average_token_length[newdata$average_token_length < 2] <- median(newdata$average_token_length)
newdata$num_keywords[newdata$num_keywords < 2] <- median(newdata$num_keywords)

newdata$kw_min_min[newdata$kw_min_min < 0] <- mean(newdata$kw_min_min)
newdata$kw_min_avg[newdata$kw_min_avg < 0] <- mean(newdata$kw_min_avg)
newdata$kw_min_max[newdata$kw_min_max < 0] <- mean(newdata$kw_min_max)

newdata$kw_max_min[newdata$kw_max_min < 0] <- mean(newdata$kw_max_min)
newdata$kw_max_avg[newdata$kw_max_avg < 0] <- mean(newdata$kw_max_avg)
newdata$kw_max_max[newdata$kw_max_max < 0] <- mean(newdata$kw_max_max)

newdata$kw_avg_min[newdata$kw_avg_min < 0] <- mean(newdata$kw_avg_min)
newdata$kw_avg_avg[newdata$kw_avg_avg < 0] <- mean(newdata$kw_avg_avg)
newdata$kw_avg_max[newdata$kw_avg_max < 0] <- mean(newdata$kw_avg_max)

newdata$self_reference_min_shares[newdata$self_reference_min_shares < 0] <- mean(newdata$self_reference_min_shares)
newdata$self_reference_max_shares[newdata$self_reference_max_shares < 0] <- mean(newdata$self_reference_max_shares)
newdata$self_reference_avg_sharess[newdata$self_reference_avg_sharess < 0] <- mean(newdata$self_reference_avg_sharess)

summary(newdata$kw_min_min)


#### Applying LOG2 Transformations to reduce the range #######
summary(newdata$n_tokens_content)
# newdata$n_tokens_content <- log2(newdata$n_tokens_content + 1.0)

#summary(newdata$kw_min_min)

newdata$kw_min_min =log2(newdata$kw_min_min + 1.0 )
newdata$kw_max_min =log2(newdata$kw_max_min + 1.0 )
newdata$kw_avg_min=log2(newdata$kw_avg_min + 1.0)

newdata$kw_min_max=log2(newdata$kw_min_max + 1.0)
newdata$kw_max_max=log2(newdata$kw_max_max + 1.0)
newdata$kw_avg_max=log2(newdata$kw_avg_max + 1.0)

newdata$kw_min_avg=log2(newdata$kw_min_avg + 1.0)
newdata$kw_max_avg=log2(newdata$kw_max_avg + 1.0)
newdata$kw_avg_avg=log2(newdata$kw_avg_avg + 1.0)

newdata$self_reference_min_shares =log2(newdata$self_reference_min_shares + 1.0)
newdata$self_reference_max_shares =log2(newdata$self_reference_max_shares + 1.0)
newdata$self_reference_avg_sharess=log2(newdata$self_reference_avg_sharess + 1.0)

###########

dataset <- newdata

#### SCALING #######
# scaled_newdata <- scale(newdata, center = TRUE, scale= TRUE)
# head(scaled_newdata)

#### Adding excluded columns from Dataset and combining Datasets ######

catgData <- select(dataset, -mycols)

final <- cbind(newdata, catgData)
dataset <- final



############# End of Data Cleaning #############

########### Creating Share Levels (popular, normal, unpopular) ###############



###################### Shares Levels ######################

share <- dataset
share$log_shares <- log(share$shares)


summary(share$log_shares)


q1<- qplot(share$log_shares,
      geom="histogram",
      binwidth = 0.2,  
      main = "Histogram for Log Shares", 
      xlab = "Log_Shares",  
      ylab = "Counts",
      fill=I("blue"), 
      col=I("black"), 
      alpha=I(.2)
)+
  theme(legend.position = "none")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

share$popularity <- cut(share$log_shares, breaks=c(-.1,mean(share$log_shares)-sd(share$log_shares), mean(share$log_shares)+sd(share$log_shares), Inf), labels=c("unpopular", "unpopular", "popular"))




q2 <- ggplot(filter(share),aes(x=popularity,fill=popularity))+
  geom_bar(position="dodge",  aes(y = (..count..))) +
  ylab("Shares") +
  ggtitle("Share per Popularity Level")+
  scale_x_discrete(labels=c("unpopular","normal", "popular"," "))+
  theme(legend.position = "none")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dataset <- share
####### Exploratory Data Analysis ############



# ggplot(share, x=share$n_tokens_title,y=share$shares)+
#  geom_bar() +
#   ylab("No. of Words in Content") +
#   xlab("No. of Words in Title")
#   ggtitle("No. of Words in Content and Title")+
#   scale_x_discrete(labels=c("unpopular","normal", "popular"," "))+
#   theme(legend.position = "none")+
#   theme_bw(base_size = 25) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))



##### Plot: Number of Words in Title and Shares ########
 p1 <- ggplot(filter(share, popularity=="unpopular"), aes(n_tokens_title, shares)) + geom_bar(stat = "identity", color="red")+
    ggtitle("Shares for Unpopular \nArticles")+
    xlab("Title Length (words)")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p2 <- ggplot(filter(share, popularity=="normal"), aes(n_tokens_title, shares)) + geom_bar(stat = "identity", color="green")+
    ggtitle("Shares for Normal \nArticles")+
    xlab("Title Length (words)")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p3 <- ggplot(filter(share, popularity=="popular"), aes(n_tokens_title, shares)) + geom_bar(stat = "identity", color="blue")+
    ggtitle("Shares for Popular \nArticles")+
    xlab("Title Length (words)")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
 # q3 <- plot_grid(p1,p2,p3, nrow=1, ncol=3)
  
  
  ##### Plot: Categories and Shares ########
 
  p4<-  ggplot(filter(share, data_channel_is_lifestyle  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Lifestyle Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
  p5<-  ggplot(filter(share, data_channel_is_entertainment  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Entertainment Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  p6<-  ggplot(filter(share, data_channel_is_bus  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Business Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p7<-  ggplot(filter(share, data_channel_is_socmed  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Social Media Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p8<-  ggplot(filter(share, data_channel_is_tech  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Technology Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p9<-  ggplot(filter(share, data_channel_is_world  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for World Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #q4 <- plot_grid(p4,p5,p6,p7,p8,p9, nrow=2, ncol=3)
  
  
  
  
  ##### Plot: Weekday and Shares ########
  
  p10<-  ggplot(filter(share, weekday_is_monday  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Monday Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p11<-  ggplot(filter(share, weekday_is_tuesday  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Tuesday Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p12<-  ggplot(filter(share, weekday_is_wednesday  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Wednesday Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p13<-  ggplot(filter(share, weekday_is_thursday  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Thursday Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p14<-  ggplot(filter(share, weekday_is_friday  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Friday Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p15<-  ggplot(filter(share, weekday_is_saturday  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Saturday Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p16<-  ggplot(filter(share, weekday_is_sunday  ==1), aes(popularity, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Sunday Articles")+
    xlab("Popularity")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #plot_grid(p10,p11,p12,p13, nrow=2, ncol=2)
  
  #plot_grid(p14,p15,p16, nrow=2, ncol=2)
 

  ##### Plot: Global Subjectivity and Shares ########

  p17<- ggplot(share,aes(global_subjectivity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    ggtitle("Shares for Global Subjectivity")+
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$global_subjectivity), sd=sd(share$global_subjectivity)))+
    xlab("Sentiment Polarity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
p17


# ##### Plot: Global Sentiment Polarity and Shares ########

p18<-  ggplot(share,aes(global_sentiment_polarity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    ggtitle("Global Sentiment Polarity")+
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$global_sentiment_polarity), sd=sd(share$global_sentiment_polarity)))+
    xlab("Global Sentiment Polarity")+
  ylab("Counts")+
  theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p18

  ##### Plot: Global Rate Positive Words and Shares ########
  
  p19<-  ggplot(filter(share,global_rate_positive_words >0.001) ,aes(global_rate_positive_words)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
   stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$global_rate_positive_words), sd=sd(share$global_rate_positive_words)))+
    ggtitle("Global Rate Positive Words")+
    xlab("Global Rate Positive Words")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p19
  
  
  ##### Plot: Global Rate Negative Words and Shares ########
  
  p20<-  ggplot(filter(share,global_rate_negative_words >0.001) ,aes(global_rate_negative_words)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$global_rate_negative_words), sd=sd(share$global_rate_negative_words)))+
    ggtitle("Global Rate Negative Words")+
    xlab("Global Rate Negative Words")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p20

  
  ##### Plot:  Rate Positive Words and Shares ########
  
  p21<-  ggplot(filter(share,rate_positive_words >0.001) ,aes(rate_positive_words)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$rate_positive_words), sd=sd(share$rate_positive_words)))+
    ggtitle("Rate Positive Words")+
    xlab("Rate Positive Words")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p21
  
  ##### Plot:  Rate Positive Words and Shares ########
  
  p22<-  ggplot(filter(share,rate_negative_words >0.001) ,aes(rate_negative_words)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$rate_negative_words), sd=sd(share$rate_negative_words)))+
    ggtitle("Rate Negative Words")+
    xlab("Rate Negative Words")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p22
  
  ##### Plot:  Average Positive Polarity and Shares ########
  
  p23<-  ggplot(filter(share,avg_positive_polarity >0.001) ,aes(avg_positive_polarity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$avg_positive_polarity), sd=sd(share$avg_positive_polarity)))+
    ggtitle("Average Positive Polarity")+
    xlab("Average Positive Polarity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p23
  
  ##### Plot:  Min Positive Polarity and Shares ########
  
  p24<-  ggplot(filter(share,min_positive_polarity >0.001) ,aes(min_positive_polarity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$min_positive_polarity), sd=sd(share$min_positive_polarity)))+
    ggtitle("Minimum Positive Polarity")+
    xlab("Minimum Positive Polarity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p24
  
  
  ##### Plot:  Max Positive Polarity and Shares ########
  
  p25<-  ggplot(filter(share,max_positive_polarity >0.001) ,aes(max_positive_polarity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$max_positive_polarity), sd=sd(share$max_positive_polarity)))+
    ggtitle("Maximum Positive Polarity")+
    xlab("Minimum Maximum Polarity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p25
  
  
  
  ##### Plot:  Average Negative Polarity and Shares ########
  
  p26<-  ggplot(share ,aes(avg_negative_polarity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$avg_negative_polarity), sd=sd(share$avg_negative_polarity)))+
    ggtitle("Average Negative Polarity")+
    xlab("Average Negative Polarity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p26
  
  ##### Plot:  Min Negative Polarity and Shares ########
  
  p27<-  ggplot(share ,aes(min_negative_polarity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$min_negative_polarity), sd=sd(share$min_negative_polarity)))+
    ggtitle("Minimum Negative Polarity")+
    xlab("Minimum Negative Polarity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p27
  
  
  ##### Plot:  Max Negative Polarity and Shares ########
  
  p28<-  ggplot(share ,aes(max_negative_polarity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$max_negative_polarity), sd=sd(share$max_negative_polarity)))+
    ggtitle("Maximum Negative Polarity")+
    xlab("Maximum Negative Polarity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p28
  

  ##### Plot:  Title Subjectivity and Shares ########
  
  p29<-  ggplot(share ,aes(title_subjectivity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$title_subjectivity), sd=sd(share$title_subjectivity)))+
    ggtitle("Title Subjectivity")+
    xlab("Title Subjectivity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p29
  
  
  ##### Plot:  Title Sentiment Polarity and Shares ########
  p30<-  ggplot(share ,aes(title_sentiment_polarity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$title_sentiment_polarity), sd=sd(share$title_sentiment_polarity)))+
    ggtitle("Title Sentiment Polarity")+
    xlab("Title Sentiment Polarity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p30
  
  ##### Plot:  ABS Title Subjectivity and Shares ########
  p31<-  ggplot(share ,aes(abs_title_subjectivity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$abs_title_subjectivity), sd=sd(share$abs_title_subjectivity)))+
    ggtitle("Title Absolute Subjectivity Level")+
    xlab("ABS Title Subjectivity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p31
  
  ##### Plot:  ABS Title Sentiment Polarity and Shares ########
  p32<-  ggplot(share ,aes(abs_title_sentiment_polarity)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$abs_title_sentiment_polarity), sd=sd(share$abs_title_sentiment_polarity)))+
    ggtitle("Title Absolute Polarity Levels")+
    xlab("ABS Title Sentiment Polarity")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p32
  
  ##### Plot:  kw_avg_min: Average Share of Worst Keyword ########
  p33<-  ggplot(share ,aes(kw_avg_min)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$kw_avg_min), sd=sd(share$kw_avg_min)))+
    ggtitle("Average Shares of Worst Keyword")+
    xlab("Worst Keyword (log)")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p33
  
  ##### Plot:  kw_avg_max: Average Share of best Keywords ########
  p34<-  ggplot(share ,aes(kw_avg_max)) + 
    geom_histogram(color="red", bins = 10, aes(y=..density..,fill=..count..))+ 
    stat_function(fun=dnorm, color="blue", args = list(mean = mean(share$kw_avg_max), sd=sd(share$kw_avg_max)))+
    ggtitle("Average Shares of Best Keyword")+
    xlab("Best Keyword (log)")+
    ylab("Counts")+
    theme(legend.position = "none")+
    theme_bw(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  p34
  
  
  ##### Plot:  num_images: Numer of Images and Shares ########
  p35<-  ggplot(filter(share,share$num_imgs<25), aes(num_imgs, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Images")+
    xlab("No. of Images")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p35
  
  ##### Plot:  num_images: Numer of Videos and Shares ########
  p36<-  ggplot(filter(share,share$num_videos<10), aes(num_videos, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Videos")+
    xlab("No. of Videos")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p36
  
  ##### Plot:  num_images: Numer of links and Shares ########
  p37<-  ggplot(share, aes(num_hrefs, shares, fill=popularity)) + geom_bar(stat = "identity")+
    ggtitle("Shares for Links")+
    xlab("No. of Links")+
    ylab("No. of Shares")+
    theme(legend.position = "none")+
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p37
  
  q1
  q2
  q3 <- plot_grid(p1,p2,p3, nrow=1, ncol=3)
  q4 <- plot_grid(p4,p5,p6,p7,p8,p9, nrow=2, ncol=3)
  
 q5<- plot_grid(p10,p11,p12,p13, nrow=2, ncol=2)
 q6<- plot_grid(p14,p15,p16, nrow=2, ncol=2)
 q7<- plot_grid(p17, p29, p31, nrow=1, ncol=2)
 q8<- plot_grid(p18, p30, p13, nrow=1, ncol=2)
 q9<- plot_grid(p19, p20, p21,p22, nrow=2, ncol=2)
 q10<- plot_grid(p23, p24, p25, nrow=2, ncol=2)
 q11<- plot_grid(p26, p27, p28, nrow=2, ncol=2)
 q12<- plot_grid(p33, p34, nrow=1, ncol=2)
 q13<- plot_grid(p35,p36,p37, nrow=1,ncol=3)
  
  for(q in 1:13){
    ggsave(filename=paste("q",q,".pdf", sep = ""), width = 21, height = 29.7, units = "cm", scale=2, plot = get(plotlist[[p]]))
  }
  
  #### StepWise Regression ####
  fit <- lm(shares ~., data=dataset)
  step <- stepAIC(fit, direction="both")
  step$anova



# Final Model:
#   shares ~ n_unique_tokens + num_hrefs + num_self_hrefs + num_imgs + 
#   num_videos + num_keywords + kw_min_min + kw_avg_min + kw_min_max + 
#   kw_avg_max + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + 
#   self_reference_max_shares + self_reference_avg_sharess + 
#   n_non_stop_words + data_channel_is_entertainment + data_channel_is_lifestyle + 
#   data_channel_is_socmed + data_channel_is_bus + data_channel_is_tech + 
#   data_channel_is_world + weekday_is_monday + weekday_is_tuesday + 
#   weekday_is_thursday + weekday_is_wednesday + weekday_is_friday + 
#   LDA_00 + LDA_01 + LDA_02 + LDA_04 + LDA_03 + global_subjectivity + 
#   global_sentiment_polarity + global_rate_negative_words + 
#   rate_positive_words + min_positive_polarity + max_positive_polarity + 
#   title_subjectivity + title_sentiment_polarity + abs_title_subjectivity


# finalData <- dataset[,c("n_unique_tokens","num_hrefs", "num_self_hrefs", "num_imgs", "num_videos", "num_keywords", "kw_min_min", "kw_max_min", "kw_avg_min", "kw_min_max", "kw_avg_max", "kw_min_avg", "kw_max_avg", "kw_avg_avg", "self_reference_min_shares", "self_reference_max_shares", "self_reference_avg_sharess", "LDA_01", "global_subjectivity", "global_rate_positive_words","global_rate_negative_words", "rate_positive_words", "min_positive_polarity","max_positive_polarity", "avg_negative_polarity", "max_negative_polarity", "title_subjectivity", "title_sentiment_polarity", "abs_title_subjectivity", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech",  "data_channel_is_world", "weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday" )]








  ########## CLUSTERING ###########
  
  cdataset <- read.csv("./Project/OnlineNewsPopularity.csv", sep=",")

  cdataset$url <- NULL
  cdataset$timedelta <- NULL
  cdataset$shares <- NULL
  
  
  #scale the variables
  scaled_wd <- scale(cdataset, center = TRUE, scale= TRUE)
  #Hierarchical Clustering
  d <- dist(cdataset,method = "euclidean") #distance matrix
  h_clust <- hclust(d, method = "ward") #clustering
  plot(h_clust,labels = water_data$V1) #dendrogram
  
  rect.hclust(h_clust,k=4)
  
  #extract clusters
  groups <- cutree(h_clust,k=4)
  groups
  
  #pca
  pcmp <- princomp(scaled_wd)
  pred_pc <- predict(pcmp, newdata=scaled_wd)[,1:2]
  
  comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups), Labels = water_data$V1)
  ggplot(comp_dt,aes(Comp.1,Comp.2))+
    geom_point(aes(color = cluster),size=3)
  
  ## kmeans
  
 
  
  kclust <- kmeans(cdataset,centers = 3,iter.max = 100)
  
  ggplot(comp_dt,aes(Comp.1,Comp.2))+
    geom_point(aes(color = as.factor(kclust$cluster)),size=3)
  
  tunek <- kmeansruns(scaled_wd,krange = 1:10,criterion = "ch")
  tunek$bestk #3
  tunekw <- kmeansruns(scaled_wd,krange = 1:10,criterion = "asw")
  tunekw$bestk #4


########################## Random stuff #####
  
  
 
  colnames(share)
  
  df_bus <- filter(share, share$data_channel_is_bus==1)
  df_lifestyle <- filter(share, share$data_channel_is_lifestyle==1)
  df_entertainment <- filter(share, share$data_channel_is_entertainment==1)
  df_socmed <- filter(share, share$data_channel_is_socmed==1)
  df_tech <- filter(share, share$data_channel_is_tech==1)
  df_world <- filter(share, share$data_channel_is_world==1)
  
summary(df_bus$popularity)
summary(df_lifestyle$popularity)
summary(df_entertainment$popularity)
summary(df_socmed$popularity)
summary(df_tech$popularity)
summary(df_world$popularity)


######### ########

## business data channel
dfb=dataset
share <-dataset
df_bus <- dataset
dim(dfb)
dfb$data_channel_is_lifestyle=NULL
dfb$data_channel_is_entertainment=NULL
dfb$data_channel_is_tech=NULL
dfb$data_channel_is_world=NULL
dfb$data_channel_is_socmed=NULL

df_bus <- subset(share, share$data_channel_is_bus==1)

dfb=filter(dfb,dfb$data_channel_is_bus==1)
summary(df_bus)
is.array(dfb)
######### #######

###### Channels vs Shares #####

df <- share

channels=c("data_channel_is_bus", "data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world")

df$channel=NA

for(colname in channels)({
  coldata=df[,colname]
  df$channel[which(coldata==1)]=colname
})

df[["channel"]][is.na(df[["channel"]])] <- "other"
summary(df$channel)

ggplot(df, aes(x=df$channel, fill=popularity))  + 
  geom_bar(position="dodge")+
  scale_x_discrete(labels=c("Business", "Entertainment", "Lifestyle", "Social Media", "Technology", "World", "Other"))+
  ggtitle("Shares by Channels")+
  xlab("Channels")+
  ylab("Number of Shares")+
  theme(legend.position = "none")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


###### Weekdays vs Shares #####

df <- share
colnames(df)
weekday=c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday", "weekday_is_sunday")

df$weekday=NA

for(colname in weekday)({
  coldata=df[,colname]
  df$weekday[which(coldata==1)]=colname
})

df[["weekday"]][is.na(df[["weekday"]])] <- "other"
summary(df$weekday)

ggplot(df, aes(x=df$weekday, fill=popularity))  + 
geom_bar(position="dodge")+
  ggtitle("Shares by Weekdays")+
  scale_x_discrete(labels=c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday"))+
  xlab("Weekdays")+
  ylab("Number of Shares")+
  theme(legend.position = "none")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
