#### Install Packages #################
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
install.packages("plotrix")
install.packages("ggrepel")

#### Load LIBRARIES #################

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
library(svglite)

#### Reading Dataset ############
setwd("/Users/Eshaal/Documents/York University/Big Data Analytics/csda1010/R")
dataset <- read.csv(file.choose(), sep=";") 

names(dataset)[names(dataset) == "y"] <- "Subscribed"

y <- "Subscribed"


#### Exploratory Data Analysis - VISUALIZATIONS #################

### Marital Status (categorical) ####
# Use ggplot() to estimate the chances of y (yes/no) from the distribution of marital (categorical)



p1 <- ggplot(filter(dataset, marital=="married"),aes(x=marital,fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ggtitle("Subscribed Users based on Marital Status")+
  ylab("Percentage") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)


p2 <- ggplot(filter(dataset, marital=="single"),aes(x=marital,fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ggtitle("Subscribed Users based on Marital Status")+
  ylab("Percentage") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)

p3 <- ggplot(filter(dataset, marital=="divorced"),aes(x=marital,fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ggtitle("Subscribed Users based on Marital Status")+
  ylab("Percentage") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0)



### Education (categorical) ####
# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Education (categorical)

ye <- ggplot(filter(dataset, Subscribed=="yes"),aes(x=education,fill=Subscribed))+
  geom_bar(position="dodge",  aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(values=c("#00BFC4", "#00BFC4", "#00BFC4"))


ne <- ggplot(filter(dataset, Subscribed=="no"),aes(x=education,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

p4 <- plot_grid(ye,ne) 

### Default (categorical) ####
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

p5 <- plot_grid(yd,nd)


### Housing Loan (categorical) ####
# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Housing (categorical)

yh <- ggplot(filter(dataset, Subscribed=="yes"),aes(x=housing,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  ggtitle("Housing Loan")+
  scale_fill_manual(values=c("#00BFC4"))+
  theme(legend.position = "none")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nh <- ggplot(filter(dataset, Subscribed=="no"),aes(x=housing,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  ggtitle("Housing Loan")+
  theme(legend.position = "none")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


p6 <- plot_grid(yh,nh)

### Personal Loan (categorical) ####
# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Loan (categorical)

ypl <- ggplot(filter(dataset, Subscribed=="yes"),aes(x=loan,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  ggtitle("Personal Loan")+
  scale_fill_manual(values=c("#00BFC4"))+
  theme(legend.position = "none")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

npl <- ggplot(filter(dataset, Subscribed=="no"),aes(x=loan,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  ggtitle("Personal Loan")+
  theme(legend.position = "none")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p7 <- plot_grid(ypl,npl)

### Contact Type (categorical) ####
# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Contact (categorical)

p8 <- ggplot(dataset,aes(x=contact,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  ggtitle("Contact Type")+
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Months (categorical) ####
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



jn <- ggplot(filter(janData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  ylab("January") + 
  xlab("counts")+
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  coord_polar(theta = "y", start =0)

fb <- ggplot(filter(febData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  ylab("February") + 
  xlab("counts")+
  #ggtitle("Y Rate based on Job and Months") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_polar(theta = "y", start =0) 

mr <- ggplot(filter(marData, is.na(y)==FALSE), aes(" " , fill=Subscribed)) + 
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


ap <- ggplot(filter(aprData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
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

my <- ggplot(filter(mayData, is.na(y)==FALSE), aes(" May", fill=Subscribed)) + 
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

je <- ggplot(filter(junData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
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


jl <- ggplot(filter(julData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
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

ag <- ggplot(filter(augData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
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

sp <- ggplot(filter(sepData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
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


oc <- ggplot(filter(octData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
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

nv <- ggplot(filter(novData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
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

dc <- ggplot(filter(decData, is.na(y)==FALSE), aes(" ", fill=Subscribed)) + 
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

p9 <- plot_grid(jn, fb, mr, ap, my, je, jl, ag, sp, oc, nv, dc, nrow=4, ncol =3)

#### Job and pOutcome (categorical) ####
# Use ggplot() to estimate the chances of y (yes/no) from the distribution of pOutcome (categorical) with in Job (categorical)
neworder <- c("admin.","blue-collar", "management","technician", "entrepreneur", "housemaid", "retired", "self-employed", "services", "student", "unemployed", "unknown")
transform -> mutate

dateset2 <- arrange(transform(dataset, job=factor(job,levels=neworder)),job)
test <- (filter(dateset2, poutcome=="success" |  poutcome == "failure"))


p10 <- ggplot (filter(test, is.na(y)==FALSE), aes(poutcome, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~job) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent) +
  ylab("Percentage") + 
  ggtitle("Job Type and Previous Campaign Outcome") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Contact Type and Months (categorical) ####
# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Month (categorical) with in Contact (categorical)


tempDS <- filter(dataset, contact=="cellular" | contact=="telephone")
ymc <- ggplot(filter(tempDS, Subscribed=="yes"), aes(month, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~contact) + 
 # scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Subcribed Users based on Contact Type and Months") +
  scale_fill_manual(values=c("#00BFC4"))+
  theme(legend.position = "none")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nmc <- ggplot(filter(tempDS, Subscribed=="no"), aes(month, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~contact) + 
  #scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") + 
  ggtitle("Subcribed Users based on Contact Type and Months") +
  theme(legend.position = "none")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p11 <- plot_grid(ymc,nmc)

### Housing and Personal Loan (categorical) ####

# Use ggplot() to estimate the chances of y (yes/no) from the distribution of Housing (categorical) with in Loan (categorical)

labels <- c("no" = "No Housing Loan", "yes" = "With Housing Loan")
p12 <- ggplot(filter(dataset, is.na(y)==FALSE), aes(loan, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  #facet_wrap(~housing, labeller="YES")+ 
  facet_grid(~housing, labeller=labeller(housing = labels))+
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  xlab("Personal Loan")+
  ggtitle("Subscribed Users based on Housing and Personal Loan") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p12

### Day (bins to weeks) and Jobs (categorical) ####
# Use ggplot() to estimate the chances of y (yes/no) from the distribution of day (int) with in job (categorical)

dbin = cut(dataset$day, breaks=c(0,7,14,21,28,Inf))
p13<- ggplot(filter(dataset, is.na(y)==FALSE), aes(dbin, fill=Subscribed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~job) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=percent)+
  scale_x_discrete(labels=c("Week1", "Week2", "Week3", "Week4", "Week5"))+
  ylab("Percentage") + 
  xlab("Weeks")+
  ggtitle("Subscribed Users based on Job and Weeks") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = , hjust = 1))



### Age (Numerical) ####
agef = cut(dataset$age, breaks=c(0,18,23,28,33,38,43,48,53,58,63,Inf))
p14 <- ggplot(dataset,aes(x=factor(agef),fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) + 
  ylab("Percentage") +
  theme_bw(base_size=25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Subscribed Users Age Distribution")

ya <- ggplot(filter(dataset, Subscribed=="yes"),aes(x=age,fill=Subscribed))+
  geom_bar(position="dodge",  aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values=c("#00BFC4", "#00BFC4", "#00BFC4"))

na <- ggplot(filter(dataset, Subscribed=="no"),aes(x=age,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


p14 <- plot_grid(ya,na)


### Month (Categorical) ####
# p13 <- ggplot(dataset,aes (month,fill=Subscribed))+
#   geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) + 
#   ylab("Percentage") +
#   theme_bw(base_size=25) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   ggtitle("Month and Subscribed Users")

### pOutcome (Categorical) ####
pdata <- dataset [dataset$poutcome =="failure" | dataset$poutcome =="success",]

yc <- ggplot(filter(pdata, Subscribed=="yes"),aes(x=poutcome,fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  xlab("Previous Campaign Outcome")+
  ggtitle("Users Subscribed") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values=c("#00BFC4", "#00BFC4", "#00BFC4"))

nc <- ggplot(filter(pdata, Subscribed=="no"),aes(x=poutcome,fill=Subscribed))+
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent)+
  ylab("Percentage") +
  xlab("Previous Campaign Outcome")+
  ggtitle("Users Did Not Subscribed") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

p15 <- plot_grid(yc,nc)

### campaign (Numerical) ####
pcontact <- c("0-1", " 1-2", "2-3", "3-4", "4-5", "5+")
cbin = cut(dataset$campaign, breaks=c(0,1,2,3,4,5,Inf))
p16 <- ggplot(dataset,aes(x=cbin,fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  ylab("Percentage") +
  ggtitle("Subscribed Users Based On \n Current Campaign") +
  theme_bw(base_size=25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Number Of Contacts") +
  scale_y_continuous(labels=percent) + scale_x_discrete(labels= pcontact)


### pContact (Categorical) ####
# dbinl <- c("week 1", "week 2", "week 3", "week 4", "week 5")
# p17 <- ggplot(dataset,aes(x=factor(dbin),fill=factor(Subscribed)))+
#   geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
#   scale_fill_brewer(palette = "Dark2", direction = -1) +
#   ylab("Percentage") +
#   ggtitle("Subscribed Users Based On \n Previous Contact") +
#   theme_bw(base_size=25) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   xlab("Number Of Contacts") +
#   scale_y_continuous(labels=percent) + scale_x_discrete(labels= dbinl)
# p17

### balance (Numerical) ####
balancef = cut(dataset$balance, breaks=c(-10000,0,2500,5000,7500,10000,12500,Inf))
balancefl <- c("<0", " 0 - 2500", "2500 - 5000", "5000 - 7500", "7500 - 10000", "10000 - 12500", ">12,500")
p17 <- ggplot(dataset,aes(x=balancef,fill=Subscribed))+
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  ylab("Percentage") +
  ggtitle("Subscribed Users Based On \n Balance") +
  theme_bw(base_size=25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Balance") +
  scale_y_continuous(labels=percent) + scale_x_discrete(labels= balancefl)

p17
plotlist = list("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","p13","p14","p15","p16","p17")

for(p in 1:17){
  ggsave(filename=paste("p",p,".pdf", sep = ""), width = 21, height = 29.7, units = "cm", scale=2, plot = get(plotlist[[p]]))
  }





