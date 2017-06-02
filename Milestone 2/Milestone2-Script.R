#Milestone 2
#Team: Clusterbusters

#NOTE: We have included the script for each method for just one question for the sake of simplicity. Just the outcome variable is different in each of the 
# 	   four questions. But results of all techniques applied to each question are mentioned in the report.

# LINEAR REGRESSION
# Question 5: Predict the number of complaints based on correlation between no of complaints and sum total of assets in the financial institutions

#Load the file : AssetsCountData.csv
countData = read.csv(file.choose())

#Remove the rows with incomplete data
countData = countData[!(countData$Include=="N"),]
#Remove the Fifth Column
countData = countData[-5]

#Rename the existing columns
colnames(countData) = c("year","quarter","count.complaints","count.assets")

#Convert comma(,) seperated number strings to numbers
countData$count.complaints = as.numeric(gsub(",", "", countData$count.complaints))
countData$count.assets = as.numeric(gsub(",", "", countData$count.assets))

#Introduce a new column so that we order the data in asending order of yearly quarters
countData$order = countData$year + (countData$quarter/10)

#Plot of Assets VS Complaints
plot(countData$count.assets,countData$count.complaints,xlab = "Assets Count",ylab="Complaints Count")

#Conduct Linear Regression on the exisitng dataset
countData.lm = lm(countData$count.complaints~countData$count.assets)
summary(countData.lm)

#Plot the regression line
abline(countData.lm)

#Check if Residuals are normally distributed : QQPlot
qqnorm(resid(countData.lm))
qqline(resid(countData.lm))

#Let us Train the model with 13 points and test with 1 point
testData = countData[1:4,]
trainData = countData[-1:-4,]
testData = as.data.frame(testData)
View(testData)
testDataFrame = data.frame(count.assets=testData$count.assets)
                           
trainData.lm = lm(count.complaints~count.assets,data=trainData)
preds = predict(trainData.lm,newdata = testData)
preds                           
pc = predict(trainData.lm,int="c",newdata = testData)
pp = predict(trainData.lm,int="p",newdata = testData)
                           
#For Confidence and Prediction Bands
#We cannot use only 1 point for test data, so we need atleast 2 for testData
newx = seq(min(countData$count.assets),max(countData$count.assets),by=1000000000)
                           
predsc = predict(trainData.lm,newdata = data.frame(count.assets=newx),int="c")
predsp = predict(trainData.lm,newdata = data.frame(count.assets=newx),int="p")
                           
plot(count.complaints~count.assets,data=trainData)
matlines(data.frame(count.assets=newx),predsc)
matlines(data.frame(count.assets=newx),predsp)

cor(preds,testData$count.complaints) #correlation
mean(trainData.lm$residuals^2)       #residual error  

#for loop
for(i in 0:13){
  k = (i+1)%%14
  j = (i+4)%%14
  ifelse(j==0,(j=i+4),j)
  ifelse(k==0,(k=i+1),k)
  testData = countData[k:j,]
  trainData = countData[-k:-j,]
  testData = as.data.frame(testData)
  trainData.lm = lm(count.complaints~count.assets,data=trainData)
  preds = predict(trainData.lm,newdata = testData)
  corValue[i] = cor(preds,testData$count.complaints)
  mse = mean(trainData.lm$residuals^2)
  #print(paste("cor: ",corValue," MSE: ",mse))
}
corValues

# LOGISTIC REGRESSION
# Question 2: Predict if a consumer would dispute the company’s feedback or not 

# choose file named "Consumer_Complaints_20170326.csv" from Datasets

d=read.csv(file.choose())
str(d)
d <- d[c(2,4,7,8,10,12,13,14)] #taking only DV's & IV's
d <- d[!(d$Consumer.disputed.==""), ] #removing blank values
levels(d$State)
d <- d[!(d$State==""), ] #removing blank states
d$State <- factor(d$State)
View(d)
levels(d$Consumer.disputed.)
d$Consumer.disputed. <- factor(d$Consumer.disputed.)
levels(d$Consumer.disputed.)
set.seed(12121)
d_rand <-d[order(runif(689922)),] #create new randomized data set

#check if both data sets have same data
summary(d$State)
summary(d_rand$State)
totalr = nrow(d_rand)
trows = 0.75*totalr  #number of rows for training data set

d_train <- d_rand[1:trows,]
d_test <- d_rand[(trows+1):totalr,]
View(d_train)
table(d_train$Consumer.disputed.)

#loading the libraries

library(aod)
library(ggplot2)

#model for Question 2
mylogit <- glm(Consumer.disputed.~Product+Issue+State+Submitted.via+Company.response.to.consumer+Timely.response.,data=d_train,family = "binomial")
mylogit 
summary(mylogit)

d_test$P <- predict(mylogit,newdata = d_test,type="response") #
View(d_test)
summary(d_test$P)


d_test$P <- predict(mylogit,newdata = d_test,type="response") #
View(d_test)
summary(d_test$P)


ggplot(d_test, aes(x=factor(Product), y=P)) + stat_summary(fun.y="mean", geom="bar")
#ggplot(d_test, aes(x=factor(Issue), y=P)) + stat_summary(fun.y="mean", geom="bar")
ggplot(d_test, aes(x=factor(Submitted.via), y=P)) + stat_summary(fun.y="mean", geom="bar")
ggplot(d_test, aes(x=factor(Company.response.to.consumer), y=P)) + stat_summary(fun.y="mean", geom="bar")
ggplot(d_test, aes(x=factor(Timely.response.), y=P)) + stat_summary(fun.y="mean", geom="bar")
#ggplot for state



# MULTINOMIAL LOGISTIC REGRESSION
# Question 3: Predict the medium through which a complaint will be received

# Read the dataset file: "Consumer_Complaints_20170326.csv" 
Consumer_Complaints <- read.csv(file.choose())

# Converting all the required variables to be used in the model into factors 
Consumer_Complaints$Company <- factor(Consumer_Complaints$Company)
Consumer_Complaints$Product <- factor(Consumer_Complaints$Product)
Consumer_Complaints$Region <- factor(Consumer_Complaints$Region)
Consumer_Complaints$Submitted.via <- factor(Consumer_Complaints$Submitted.via)
Consumer_Complaints$Timely.response. <- factor(Consumer_Complaints$Timely.response.)

# Checking the top companies with the maximum complaints. We will be using these companies only to reduce the levels for the 'Company' categorical variable. It contains more than 4000 companies and hence we are trying to take only the top one which cover more than 60 percent of the total data. 
library(sqldf)

sqldf('select Company,count(Company) from Consumer_Complaints group by Company order by count(Company) desc limit 10')


# Creating a sample containing records for the top 10 companies and keeping only the required columns that can have some effect on the outcome variable 
cc1 <- Consumer_Complaints[Consumer_Complaints$Company %in% c('Bank of America','Wells Fargo & Company','Equifax','Experian','JPMorgan Chase & Co','TransUnion Intermediate Holdings, Inc.','Citibank','Ocwen','Capital One','Navient Solutions, LLC.'),c(2,7,9,11,14)]


cc1$Company=factor(cc1$Company)

# Creating sub samples for Medium so that we can take an equal no. of rows for each medium level
cc2.web <- subset(cc1,cc1$Submitted.via=='Web')
cc2.phone <- subset(cc1,cc1$Submitted.via=='Phone')
cc2.fax <- subset(cc1,cc1$Submitted.via=='Fax')
cc2.referral <- subset(cc1,cc1$Submitted.via=='Referral')
cc2.postal <- subset(cc1,cc1$Submitted.via=='Postal mail')
cc2.email <- subset(cc1,cc1$Submitted.via=='Email')

cc2.web <- cc2.web[order(runif(5000)),]
cc2.fax <- cc2.fax[order(runif(5000)),]
cc2.phone <- cc2.phone[order(runif(5000)),]
cc2.postal <- cc2.postal[order(runif(5000)),]
cc2.referral <- cc2.referral[order(runif(5000)),]

#Merging all the subsamples into a single sample 
sample <- rbind(cc2.web,cc2.fax,cc2.phone,cc2.postal,cc2.referral,cc2.email)

# Randomizing the data
sample <- sample[order(runif(25123)),]

train_idx <- createDataPartition(y=sample$Submitted.via,p=0.75,list=FALSE)

# Creating training and testing samples
train_sample <- sample[train_idx,]
test_sample <- sample[-train_idx,]

attach(train_sample)

# Creating the model for multinomial logistic regression
library(nnet)
mlr <- multinom(Submitted.via ~ Product + Company + Region + Timely.response.,data=train_sample)
summary(mlr)

# Calculating fitted values for training data
head(pp <- fitted(mlr))

# For determinig relative risk ratios 
exp(coef(mlr))

# For calculating output values for testing sample  
predict(mlr,newdata=test_sample,"probs")



# NAIVE BAYES
# Question 1: Predicting the response to a particular consumer complaint

# Loading the libraries and the dataset

library(gmodels)	
library(e1071)
library(caret)
library(readr)
library(ROCR)

# Load the dataset: Clean_Consumer_Complaints.csv
complaints <- read_csv(file.choose())

attach(complaints)

# ensuring all categorical values as treated as factors
complaints$Issue<-factor(complaints$Issue)
complaints$Product<-factor(complaints$Product)
complaints$Company<-factor(complaints$Company)
complaints$State<-factor(complaints$State)
complaints$`Submitted via`<-factor(complaints$`Submitted via`)
complaints$`Timely response?`<-factor(complaints$`Timely response?`)
complaints$`Consumer disputed?`<-factor(complaints$`Consumer disputed?`)
complaints$`Company response to consumer`<-factor(complaints$`Company response to consumer`)

# creating training and testing datasets (75:25)
set.seed(12345)
indx<-createDataPartition(complaints$`Company response to consumer`, p=0.75, list = FALSE)
train<-complaints[indx,]
test<-complaints[-indx,]

# building the Naive Bayes model
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

# creating the confusion table
xtab<-table(nb_classifier_pred,test$`Company response to consumer`)
confusionMatrix(xtab)

#Laplace

# building the Naive Bayes model with laplace=1
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train, laplace = 1)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

xtab<-table(nb_classifier_pred,test$`Company response to consumer`)

# creating the confusion table
confusionMatrix(xtab)


# building the Naive Bayes model with laplace=2
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train, laplace = 2)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

xtab<-table(nb_classifier_pred,test$`Company response to consumer`)

# creating the confusion table
confusionMatrix(xtab)

# Sub-sampling
# Creating subsets for sampling

closed<-subset(complaints,`Company response to consumer`=="Closed")
closed_exp<-subset(complaints,`Company response to consumer`=="Closed with explanation")
closed_mon<-subset(complaints,`Company response to consumer`=="Closed with monetary relief")
closed_no_mon<-subset(complaints,`Company response to consumer`=="Closed with non-monetary relief")
closed_mon<-subset(complaints,`Company response to consumer`=="Closed with monetary relief")
closed_relief<-subset(complaints, `Company response to consumer`=="Closed with relief")
closed_no_relief<-subset(complaints, `Company response to consumer`=="Closed without relief")

set.seed(748)
n<-nrow(closed_relief)

rand<-closed[order(runif(n)),]
d<-rbind(closed_relief,rand)

rand<-closed_exp[order(runif(n)),]
d<-rbind(d,rand)

rand<-closed_mon[order(runif(n)),]
d<-rbind(d,rand)

rand<-closed_no_mon[order(runif(n)),]
d<-rbind(d,rand)

rand<-closed_no_relief[order(runif(n)),]
d<-rbind(d,rand)

# creating training and testing datasets (75:25)
set.seed((37543))
no<-nrow(d)
rand<-d[order(runif(no)),]
splt<-(no*0.75)
train<-rand[1:splt,]
test<-rand[(splt+1):no,]

# building the Naive Bayes model
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

# creating the confusion table
xtab<-table(nb_classifier_pred,test$`Company response to consumer`)
confusionMatrix(xtab)

#Laplace

# building the Naive Bayes model with laplace=1
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train, laplace = 1)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

xtab<-table(nb_classifier_pred,test$`Company response to consumer`)

# creating the confusion table
confusionMatrix(xtab)


# building the Naive Bayes model with laplace=2
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train, laplace = 2)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

xtab<-table(nb_classifier_pred,test$`Company response to consumer`)

# creating the confusion table
confusionMatrix(xtab)


# DECISION TREES AND RANDOM FORESTS

library(gmodels)	
library(e1071)
library(caret)
library(readr)
library(ROCR)
library(C50)
library(randomForest)

complaints <- read_csv("C:/Users/paldo/Desktop/MIM/Spring 2017/INST737/Project/Clean_Consumer_Complaints.csv")
complaints <-as.data.frame(complaints)
attach(complaints)

complaints$Issue<-as.factor(complaints$Issue)
complaints$Product<-as.factor(complaints$Product)
complaints$Company<-as.factor(complaints$Company)
complaints$State<-as.factor(complaints$State)
complaints$`Submitted via`<-as.factor(complaints$`Submitted via`)
complaints$`Timely response?`<-as.factor(complaints$`Timely response?`)
complaints$`Consumer disputed?`<-as.factor(complaints$`Consumer disputed?`)
complaints$`Company response to consumer`<-as.factor(complaints$`Company response to consumer`)


complaints<-complaints[,-2]
complaints<-complaints[,-3]

nrow(complaints)
complaints<-na.omit(complaints)


set.seed((4589))
no<-nrow(complaints)
rand<-complaints[order(runif(no)),]
splt<-(no*0.75)
train<-rand[1:splt,]
test<-rand[(splt+1):no,]

prop.table(table(train$`Company response to consumer`))
prop.table(table(test$`Company response to consumer`))

model<-C50::C5.0(train[-4],train$`Company response to consumer`)
model
summary(model)
pred<-predict(model,test)

xtab<-table(pred,test$`Company response to consumer`)
a<-confusionMatrix(xtab)
a$overall['Accuracy']

#boosting
model_boost<-C50::C5.0(train[-4],train$`Company response to consumer`,trials=10)
model_boost
summary(model_boost)
pred_boost<-predict(model_boost,test)

xtab_boost<-table(pred_boost,test$`Company response to consumer`)
a<-confusionMatrix(xtab_boost)
a$overall['Accuracy']

# boosting
# model_bag<-randomForest(train$`Company response to consumer`~.,data=train, mtry=13, importance=TRUE)


