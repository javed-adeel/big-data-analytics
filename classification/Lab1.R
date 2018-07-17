#### Working Directory #################
setwd("./Documents/Big Data Analytics/classification/R")

#### LIBRARIES #################
install.packages("RSQLite")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("mice")
install.packages("VIM")
install.packages("pROC")
install.packages("caret")
install.packages("sqldf")
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("Amelia")
install.packages("mosaic")
install.packages("scales")
install.packages("cowplot")

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


#### LOAD FILES ################

rm(list = ls())
dataset <- read.csv(file.choose(), sep=";") 

names(dataset)[names(dataset) == "y"] <- "Subscribed"

#### EXPLORATORY DATA ANALYSIS #################

#### VISUALIZATION ############

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Ages.
y <- "Subscribed"
y
str(dataset)
sapply(dataset, class)
ggplot(dataset,aes(x=factor(age),fill=Subscribed))+
geom_bar(position="dodge")

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of jobs (categorical)

str(dataset)
sapply(dataset, class)
ggplot(dataset,aes(x=job,fill=Subscribed))+
  geom_bar(position="dodge")

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of marital (categorical)

ggplot(dataset,aes(x=marital,fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ggtitle("Subscribed Users")+
ylab("Percentage") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Education (categorical)

ggplot(dataset,aes(x=education,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Default (categorical)
yesDefault <- dataset[dataset$default=="yes",]
yd <- ggplot(yesDefault,aes(" ",fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  ggtitle("Subscribers Users With\n Previous Loan Default")+
  coord_polar(theta = "y", start =1)+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

     
     
noDefault <- dataset[dataset$default=="no",]
nd <- ggplot(noDefault,aes(" ",fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  xlab(" ")+
  ggtitle("Subscribers Users Without\n Previous Loan Default")+
  coord_polar(theta = "y", start =1)+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_grid(yd,nd)





# Use ggplot() to estimate the chances of y (yes/no) from the distribution of balance (int)

ggplot(dataset,aes(x=factor(balance),fill=y))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") 

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Housing (categorical)

ggplot(dataset,aes(x=housing,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  ggtitle("Housing Loan")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Loan (categorical)

ggplot(dataset,aes(x=loan,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  ggtitle("Personal Loan")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Contact (categorical)

ggplot(dataset,aes(x=contact,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  ggtitle("Contact Type")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Use ggplot() to estimate the chances of y (yes/no) from the distribution of day (int)

ggplot(dataset,aes(x=factor(day),fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage")

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Month (categorical)

ggplot(dataset,aes(x=month,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage")

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Duration (int)

ggplot(dataset,aes(x=factor(duration),fill=y))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage")

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Compaign (int)
#cbin = cut(dataset$compaign, breaks=c(0,1,2,3,4,5,6))
ggplot(dataset,aes(x=factor(dataset$campaign),fill=factor(y)))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage")

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of pDays (int)

ggplot(dataset,aes(x=factor(pdays),fill=y))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage")

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Previous (int)

ggplot(dataset,aes(x=factor(previous),fill=y))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage")

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of pOutcome (categorical)

ggplot(dataset,aes(x=poutcome,fill=y))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage")

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of day (int) with in job (categorical)

dbin = cut(dataset$day, breaks=c(0,7,14,21,28,Inf))
ggplot(filter(dataset, is.na(y)==FALSE), aes(dbin, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~job) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Subscribed Users based on Job and Weeks") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of month (categorical) with in job (categorical)

ggplot(filter(dataset, is.na(y)==FALSE), aes(month, fill=y)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~job) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of education (categorical) with in marital (categorical)

ggplot(filter(dataset, is.na(y)==FALSE), aes(marital, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~education) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Y Rate based on Education and Marital Status") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Housing (categorical) with in Loan (categorical)

ggplot(filter(dataset, is.na(y)==FALSE), aes(loan, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~housing) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Subscribed Users based on Housing and Personal Loan") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Month (categorical) with in Contact (categorical)

ggplot(filter(dataset, is.na(y)==FALSE), aes(month, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~contact) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Subcribed Users based on Contact Type and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Contact (categorical) with in Month (categorical)

ggplot(filter(dataset, is.na(y)==FALSE), aes(contact, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~month) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Y Rate based on Contact and Month") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of pOutcome (categorical) with in Job (categorical)
neworder <- c("admin.","blue-collar", "management","technician", "entrepreneur", "housemaid", "retired", "self-employed", "services", "student", "unemployed", "unknown")
transform -> mutate
dateset2 <- arrange(transform(dataset, job=factor(job,levels=neworder)),job)
ggplot(filter(dateset2, is.na(y)==FALSE), aes(poutcome, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~job) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Job Type and Previous Campaign Outcome") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Use ggplot() to estimate the chances of y (yes/no) from the distribution of month (categorical)
janData <- dataset[dataset$month=="jan",]
febData <- dataset[dataset$month=="feb",]
marData <- dataset[dataset$month=="mar",]
aprData <- dataset[dataset$month=="apr",]
mayData <- dataset[dataset$month=="may",]
junData <- dataset[dataset$month=="jun",]
julData <- dataset[dataset$month=="jul",]
augData <- dataset[dataset$month=="aug",]
sepData <- dataset[dataset$month=="sep",]
octData <- dataset[dataset$month=="oct",]
novData <- dataset[dataset$month=="nov",]
decData <- dataset[dataset$month=="dec",]

ggplot(dataset,aes(x=job,fill=y))+
  geom_bar(position="dodge")

jn <- ggplot(filter(janData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  ylab("January") + 
  xlab("counts")+
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  coord_polar(theta = "y", start =0)

fb <- ggplot(filter(febData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  ylab("February") + 
  xlab("counts")+
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0) 

mr <- ggplot(filter(marData, is.na(y)==FALSE), aes(" " , fill=y)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  #scale_y_continuous(labels=percent)+
  ylab("March") + 
  xlab("counts")+
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)


ap <- ggplot(filter(aprData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  #scale_y_continuous(labels=percent)+
  ylab("April") + 
  xlab("counts")+ 
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)

my <- ggplot(filter(mayData, is.na(y)==FALSE), aes(" May", fill=y)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  #scale_y_continuous(labels=percent)+
  ylab("May") + 
  xlab("counts")+ 
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)

je <- ggplot(filter(junData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  #facet_wrap(~month) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  #scale_y_continuous(labels=percent)+
  ylab("June") + 
  xlab("counts")+
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)
  

jl <- ggplot(filter(julData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  #facet_wrap(~month) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  #scale_y_continuous(labels=percent)+
  ylab("July") + 
  xlab("counts")+
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)

ag <- ggplot(filter(augData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  #facet_wrap(~month) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
 # scale_y_continuous(labels=percent)+
  ylab("August") + 
  xlab("counts")+ 
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)

sp <- ggplot(filter(sepData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  #facet_wrap(~month) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
 # scale_y_continuous(labels=percent)+
  ylab("September") + 
  xlab("counts")+ 
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)
  

oc <- ggplot(filter(octData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  #facet_wrap(~month) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
 # scale_y_continuous(labels=percent)+
  ylab("October") + 
  xlab("counts")+ 
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)

nv <- ggplot(filter(novData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  #facet_wrap(~month) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
 # scale_y_continuous(labels=percent)+
  ylab("November") + 
  xlab("counts")+ 
 # ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)

dc <- ggplot(filter(decData, is.na(y)==FALSE), aes(" ", fill=y)) + 
  geom_bar() +
  #facet_wrap(~month) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
#  scale_y_continuous(labels=percent)+
  ylab("December") + 
  xlab("counts")+ 
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)

plot_grid(jn, fb, mr, ap, my, je, jl, ag, sp, oc, nv, dc, nrow=4, ncol = 3 )

# Use ggplot() to estimate the chances of y (yes) from the distribution of month (categorical)
yesData <- dataset[dataset$y=="yes",]
ggplot(filter(yesData, is.na(y)==FALSE), aes(month, fill=y)) + 
  geom_bar(aes(x= y, y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~month) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)



barplot(prop.table(table(yesData$y, yesData$month)))+


#### DATA CLEANNING ############
dataset$duration <- NULL
colSums(is.na(dataset)|dataset=='')
missmap(dataset, main="dataset Data - Missings Map",
        col=c("yellow", "black"), legend=FALSE)

#### KNOWING DATA ##############

dim(dataset)
str(dataset)
summary(dataset)


