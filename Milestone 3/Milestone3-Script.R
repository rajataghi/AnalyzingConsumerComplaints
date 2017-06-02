#Milestone 2
#Team: Clusterbusters

#NOTE: We have included the script for each method for just one question for the sake of simplicity. Just the outcome variable is different in each of the 
# 	   four questions. But results of all techniques applied to each question are mentioned in the report.

# SVM
# Question 1: Predicting the response to a particular consumer complaint

# Loading the libraries and the dataset
library(kernlab)
library(caret)
library(readr)

# Load the dataset: Consumer_Complaints_Milestone3.csv
Consumer_Complaints <- read_csv(file.choose())

# Preprocessing tasks
Consumer_Complaints <- na.omit (Consumer_Complaints)
str(Consumer_Complaints)

Consumer_Complaints$Issue<-factor(Consumer_Complaints$Issue)
Consumer_Complaints$Product<-factor(Consumer_Complaints$Product)
Consumer_Complaints$Company<-factor(Consumer_Complaints$Company)
Consumer_Complaints$Region<-factor(Consumer_Complaints$Region)
Consumer_Complaints$Submitted.via<-factor(Consumer_Complaints$Submitted.via)
Consumer_Complaints$Timely.response.<-factor(Consumer_Complaints$Timely.response.)
Consumer_Complaints$Consumer.disputed.<-factor(Consumer_Complaints$Consumer.disputed.)
Consumer_Complaints$Company.response.to.consumer<-factor(Consumer_Complaints$Company.response.to.consumer)

attach(Consumer_Complaints)

# creating training and testing datasets (75:25) after randomly choosing 30% of the records so that RStudio can handle it
set.seed(12345)
index<-createDataPartition(Consumer_Complaints$Company.response.to.consumer, p=0.3, list = FALSE)

new_data <- Consumer_Complaints[index,]
index<-createDataPartition(new_data$Company.response.to.consumer, p=0.75, list = FALSE)
training<-new_data[index,]
testing<-new_data[-index,]
sink("results.txt")

# Kernel: Linear (Vanilladot)
# building the model
classifier_vanilla<-ksvm(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed.,data=training, kernel= "vanilladot")

# Testing the model on testing data
pred_vanilla<-predict(classifier_vanilla,testing)

# creating the confusion table
ptab <- table(pred_vanilla,testing$Company.response.to.consumer)
results_vanilla <- confusionMatrix(ptab)
results_vanilla
sink()

# Kernel: Non-Linear (Gaussian)
# building the model
classifier_rbf<-ksvm(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed.,data=training, kernel= "rbfdot")

# Testing the model on testing data
pred_rbf<-predict(classifier_rbf,testing)

# creating the confusion table
ptab <- table(pred_rbf,testing$Company.response.to.consumer)
results_rbf <- confusionMatrix(ptab)
results_rbf
sink()


# Question 5: Predict the number of complaints based on correlation between number of complaints and sum total of assets in the financial institutions

# Load the dataset: assets.csv
assets <- read_csv(choose.file())
attach(assets)

assets$Quarter <- as.factor(assets$Quarter)
assets$Year <- as.factor(assets$Year)


index<-createDataPartition(assets$Complaint_Count, p=0.55, list = FALSE)
training<-assets[index,]
testing<-assets[-index,]

# Kernel: Linear (Vanilladot)
classifier_vanilla5<-ksvm(Complaint_Count~.,data=training, kernel= "vanilladot", type="nu-svr")
pred_vanilla5<-predict(classifier_vanilla5,testing)

# Checking model
ptab <- table(sort(pred_vanilla5),sort(testing$Complaint_Count))
ptab
cor.test(pred_vanilla5,testing$Complaint_Count)

# Kernel: Non-Linear (Gaussian)
classifier_rbf5<-ksvm(Complaint_Count~.,data=training, kernel= "rbfdot", type="nu-svr")
pred_rbf5<-predict(classifier_rbf5,testing)

# Checking model
ptab <- table(sort(pred_rbf5),sort(testing$Complaint_Count))
ptab
cor.test(pred_rbf5,testing$Complaint_Count)


# Neural Networks
# Question : Predict the geographical region from where the complaint originated

# Load the dataset: Consumer_Complaints_Milestone3.csv
d=read.csv(choose.file())
View(d)
table(d$Consumer.disputed.) #check for blank rows
d2 <- subset(d,Consumer.disputed.=='Yes' | Consumer.disputed. == 'No')

set.seed(12121)
d2_rand <-d2[order(runif(695379)),] #create new randomized data set

d2_rand[d2_rand==""] <- NA
d2_rand <- na.omit(d2_rand)
levels(d2_rand$Company.response.to.consumer)
View(d2_rand)
cc1 <- d2_rand[d2_rand$Company %in% c('Bank of America','Wells Fargo & Company','Equifax','Experian','JPMorgan Chase & Co','TransUnion Intermediate Holdings, Inc.','Citibank','Ocwen','Capital One','Nationstar Mortgage'),]
cc1$Company <- factor(cc1$Company)
levels(cc1$Company)
View(cc1)
totalr = 1000
cc1$Factorregion = factor(cc1$Region, labels=c("1", "2", "3", "4","5","6","7")) #code output variable
trows = 0.75*totalr
trows
#split data into training and testing data
d2_train <- cc1[1:trows,]
d2_test <- cc1[(trows+1):totalr,]
d2_train[d2_train==""] <- NA
d2_train <- na.omit(d2_train)
levels(cc1$Company)
View(d2_train)
as.factor(d2_train$Submitted.via)
as.factor(d2_train$Product)
as.factor(d2_train$Company)
contrasts(d2_train$Submitted.via) <- contr.sum
contrasts(d2_train$Product) <- contr.sum
contrasts(d2_train$Company) <- contr.sum

mq4 <- model.matrix(
  ~ Product+ Company+ Submitted.via, 
  data = d2_train
)
nrow(d2_train)
nrow(mq4)

d2_ttq4 <-mq4[,-1]
nrow(d2_ttq4)
nrow(mq4)
d2_ttq4 <- cbind(d2_ttq4,d2_train$Factorregion)
View(d2_ttq4)

nrow(d2_ttq4)

#Model with 1 Layer
nn_modelq4 <- neuralnet(formula=V26~ Product1+	Product2+	Product3+	Product4+	Product5+	Product6+	Product7+	Product8+	Product9+	Product10+	Product11 + Company1+	Company2+	Company3+	Company4+	Company5+	Company6+	Company7+	Company8+	Submitted.via1+	Submitted.via2+	Submitted.via3+	Submitted.via4+	Submitted.via5+	Submitted.via6
                        ,data = d2_ttq4,stepmax = 1e6)

plot(nn_modelq4)

#Model with 3 Layer
nn_modelq43 <- neuralnet(formula=V26~ Product1+	Product2+	Product3+	Product4+	Product5+	Product6+	Product7+	Product8+	Product9+	Product10+	Product11 + Company1+	Company2+	Company3+	Company4+	Company5+	Company6+	Company7+	Company8+	Submitted.via1+	Submitted.via2+	Submitted.via3+	Submitted.via4+	Submitted.via5+	Submitted.via6
                         ,data = d2_ttq4,hidden = 3,stepmax = 1e6)

plot(nn_modelq43)

#Model with 5 Layer
nn_modelq45 <- neuralnet(formula=V26~ Product1+	Product2+	Product3+	Product4+	Product5+	Product6+	Product7+	Product8+	Product9+	Product10+	Product11 + Company1+	Company2+	Company3+	Company4+	Company5+	Company6+	Company7+	Company8+	Submitted.via1+	Submitted.via2+	Submitted.via3+	Submitted.via4+	Submitted.via5+	Submitted.via6
                         ,data = d2_ttq4,hidden = 5,stepmax = 1e6)

plot(nn_modelq45)

as.factor(d2_test$Company)
as.factor(d2_test$Product)
as.factor(d2_test$Submitted.via)
contrasts(d2_test$Company) <- contr.sum
contrasts(d2_test$Product) <- contr.sum
contrasts(d2_test$Submitted.via) <- contr.sum

mq4_testing <- model.matrix( 
  ~  Product + Company+ Submitted.via, 
  data = d2_test 
)
d2_testingsetq4 <-mq4_testing[,-1]
d2_testingsetq4 <- cbind(d2_testingsetq4,d2_test$Factormedium)

####Testing the model(s) created above

#Model with 1 layer
View(d2_testingsetq4)
model_resultsq4 <- compute(nn_modelq4,d2_testingsetq4[,1:25])
predicted_strengthq4 <- model_resultsq4$net.result

cor(predicted_strengthq4,d2_testingsetq4[,26]) #1layer ; Correlation= 0.33

#Model with 3 layer
View(d2_testingsetq4)
model_resultsq43 <- compute(nn_modelq43,d2_testingsetq4[,1:25])
predicted_strengthq43 <- model_resultsq43$net.result

cor(predicted_strengthq43,d2_testingsetq4[,26]) #3layer ; Correlation= 0.097

#Model with 5 layer
View(d2_testingsetq4)
model_resultsq45 <- compute(nn_modelq45,d2_testingsetq4[,1:25])
predicted_strengthq45 <- model_resultsq45$net.result

cor(predicted_strengthq45,d2_testingsetq4[,26]) #5layer ; Correlation= 0.086


# Clustering
# For all research questions

#Package for running K Mode
install.packages("klar")
library(klaR)

#Package for K-mdeoids
install.packages("cluster")
library(cluster)

#Package for Spectral Clustering 
install.packages("kernlab")
library(kernlab)

#Package for Mixture Clustering 
install.packages("mclust")
library(mclust)

#Load the data file Consumer_Complaints_Milestone3_Data_Values.csv
data = read.csv(file.choose()) 

#Subset the data to random 10k rows
data_subset_10k = data[sample(nrow(data),10000),]
------------------------------------------------------------------------------------------
#Research Question 1: Predicting the response to a particular consumer complaint

#Filter the required columns
data_q1 = data_subset_10k[,c("Product","Issue","Company.response.to.consumer")]

#Run the clustering model
cl <- kmodes(data_q1, 7)

#Data formatting for plotting
data_q1$Product = as.numeric(data_q1$Product)
data_q1$Issue = as.numeric(data_q1$Issue)
data_q1$Company.response.to.consumer = as.numeric(data_q1$Company.response.to.consumer)
data_m = data.matrix(data_q1)

#Plot the input data, color coded on basis of IV
plot(jitter(data_m),col=data_q1$Company.response.to.consumer)
#Distribuion of IV
hist(data_q1$Company.response.to.consumer)

#Plot the input data, color coded on basis of cluster
plot(jitter(data_m), col = cl$cluster)
#Distribution of cluster
hist(cl$cluster)
------------------------------------------------------------------------------------------
#Research Question 2: Predict if a consumer would dispute the company’s feedback or not 

#Filter the required columns
data_q2 = data_subset_10k[,c("Product","Region","Submitted.via","Consumer.disputed.")]

#Run the clustering model
cl <- kmodes(data_q2, 7)

#Data formatting for plotting
data_q2$Product = as.numeric(data_q2$Product)
data_q2$Region = as.numeric(data_q2$Region)
data_q2$Submitted.via = as.numeric(data_q2$Submitted.via)
data_m = data.matrix(data_q2)

#Plot the input data, color coded on basis of IV
plot(jitter(data_m),col=cl$cluster)
#Distribuion of IV
hist(cl$cluster)

#Plot the input data, color coded on basis of cluster
plot(jitter(data_m),col=data_q2$Consumer.disputed.)
#Distribution of cluster
hist(data_q2$Consumer.disputed.)
------------------------------------------------------------------------------------------
#Research Question 3: Predict the medium through which a complaint will be received

#Filter the required columns
data_q3 = data_subset_10k[,c("Product","Region","Submitted.via")]

#Run the clustering model
cl <- kmodes(data_q3, 7)

#Data formatting for plotting
data_q3$Product = as.numeric(data_q3$Product)
data_q3$Region = as.numeric(data_q3$Region)
data_m = data.matrix(data_q3)

#Plot the input data, color coded on basis of IV
plot(jitter(data_m),col=cl$cluster)
#Distribuion of IV
hist(cl$cluster)

#Plot the input data, color coded on basis of cluster
plot(jitter(data_m),col=data_q3$Submitted.via)
#Distribution of cluster
hist(data_q4$Submitted.via)
------------------------------------------------------------------------------------------
#Research Question 4: Predict the geographical region in United States where the complaint originated

#Filter the required columns
data_q4 = data_subset_10k[,c("Product","Issue","Region")]
#Filter out rows with region N/A
data_q4=data_q4[!(data_q4$Region=="#N/A"),]

#Run the clustering model
cl <- kmodes(data_q4,6)

#Data formatting for plotting
data_q4$Product = as.numeric(data_q4$Product)
data_q4$Issue = as.numeric(data_q4$Issue)
data_m = data.matrix(data_q4)

#Plot the input data, color coded on basis of IV
plot(jitter(data_m),col=cl$cluster)
#Distribuion of IV
hist(cl$cluster)

#Plot the input data, color coded on basis of cluster
plot(jitter(data_m),col=data_q4$Region)
#Distribuion of IV
hist(data_q4$Region)
------------------------------------------------------------------------------------------
#Research Question 5: Predict the number of complaints based on correlation between number 
#of complaints and sum total of assets in the financial institutions

#Load the datafile : assets_institutions_cleaned_data.csv
data = read.csv(file.choose())

#Filter the data to include only complete rows
data = data[(data$Include=="Y"),]
data = data[,c("Count.of.Compaints","Sum.of.Assets...Institutions")]
#Filter the required columns and convert to numeric format
colnames(data) = c("complaints","assets")
data$complaints = as.numeric(as.character(data$complaints))
data$assets = as.numeric(as.character(data$assets))

#Technique 1: k-means
cl <- kmeans(as.matrix(data),2)

plot(data,col=cl$cluster,pch=16)
points(cl$centers,col=1:2,pch=8,cex=2)

#Technique 1: k-medoids
pm <- pam(data,2)

summary(pm)
plot(pm)

#Technique 3: spectral clustering
spcc <- specc(as.matrix(data),2)

plot(data,col=spcc,pch=16)
points(centers(spcc),col=1:2,pch=8,cex=2)

#Technique 4: m-clust
mcl <- Mclust(as.matrix(data))

summary(mcl)
summary(mcl,parameters=TRUE)

plot(mcl) #option2
------------------------------------------------------------------------------------------

# Read the dataset file: Consumer_Complaints_Milestone3.csv
Consumer_Complaints <- read.csv(file.choose())

# Converting all the required variables to be used in the model into factors 
Consumer_Complaints$Company <- factor(Consumer_Complaints$Company)
Consumer_Complaints$Product <- factor(Consumer_Complaints$Product)
Consumer_Complaints$Issue = factor(Consumer_Complaints$Issue)
Consumer_Complaints$Region <- factor(Consumer_Complaints$Region)
Consumer_Complaints$Submitted.via <- factor(Consumer_Complaints$Submitted.via)
Consumer_Complaints$Company.response.to.consumer=factor(Consumer_Complaints$Company.response.to.consumer)
Consumer_Complaints$Timely.response. <- factor(Consumer_Complaints$Timely.response.)
Consumer_Complaints$Consumer.disputed.=factor(Consumer_Complaints$Consumer.disputed.)

# Checking the top companies with the maximum complaints. We will be using these companies only to reduce the levels for the 'Company' categorical variable. It contains more than 4000 companies and hence we are trying to take only the top 10 which cover more than 50 percent of the total data. 
library(sqldf)

sqldf('select Company,count(Company) from Consumer_Complaints group by Company order by count(Company) desc limit 10')

data <- Consumer_Complaints[Consumer_Complaints$Company %in% c('Bank of America','Wells Fargo & Company','Equifax','Experian','JPMorgan Chase & Co','TransUnion Intermediate Holdings, Inc.','Citibank','Ocwen','Capital One','Nationstar Mortgage'),]

# Clustering technique - Hierarchical Clustering
# Question 1 - Predict the company response to consumer
 
cc1=data[,c(1,2,3,6)]

install.packages("ClustOVar")
library(ClustOVar)

# Generating the hierarchical cluster model
tree <- hclustvar(X.quali = cc1)
plot(tree)

# Cutting the tree to generate two clusters
part=cutreevar(tree,2)
summary(part)

 # Comparative Analysis
 # Question 1: Predict the company's response to a consumer complaint

 # Comparative Analysis

library(caret)
# Load the dataset: Consumer_Complaints_Milestone3.csv
Consumer_Complaints <- read_csv(file.choose())

# Preprocessing tasks
Consumer_Complaints <- na.omit (Consumer_Complaints)
str(Consumer_Complaints)

Consumer_Complaints$Issue<-factor(Consumer_Complaints$Issue)
Consumer_Complaints$Product<-factor(Consumer_Complaints$Product)
Consumer_Complaints$Company<-factor(Consumer_Complaints$Company)
Consumer_Complaints$Region<-factor(Consumer_Complaints$Region)
Consumer_Complaints$Submitted.via<-factor(Consumer_Complaints$Submitted.via)
Consumer_Complaints$Timely.response.<-factor(Consumer_Complaints$Timely.response.)
Consumer_Complaints$Consumer.disputed.<-factor(Consumer_Complaints$Consumer.disputed.)
Consumer_Complaints$Company.response.to.consumer<-factor(Consumer_Complaints$Company.response.to.consumer)

Consumer_Complaints <- subset(Consumer_Complaints, Consumer_Complaints$Company.response.to.consumer!="Untimely response")

set.seed(5)
index<-createDataPartition(Consumer_Complaints$Company.response.to.consumer, p=0.05, list = FALSE)

new_data <- Consumer_Complaints[index,]

# Method: K-Fold cross validation
# control method function
train_control<- trainControl(method = "cv", number=10)

# Model: SVM
model_svm<-train(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed., data = new_data, trControl=train_control, method = "svmRadial")

# Model: Neural Networks
model_nn<-train(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed., data = new_data, trControl=train_control, method = "nnet")

# Model: Random Forests
model_rf<-train(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed., data = new_data, trControl=train_control, method = "rf")

results	<-	resamples(list(NN=modelnn,	RF=modelrf,	SVM=modelsvm))	

summary(results)

bwplot(results)
dotplot(results)

# Method: Repeated K-Fold cross validation (3 repeats)
# control method function
train_control1<- trainControl(method = "repeatedcv", number=10, repeats = 3)

# Model: SVM
model_svm1<-train(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed., data = new_data, trControl=train_control1, method = "svmRadial")

# Model: Neural Networks
model_nn1<-train(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed., data = new_data, trControl=train_control1, method = "nnet", verbose = FALSE)

# Model: Random Forests
model_rf1<-train(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed., data = new_data, trControl=train_control1, method = "rf")

results1	<-	resamples(list(NN=model_nn1,	RF=model_rf1,	SVM=model_svm1))	

summary(results1)

bwplot(results1)

# Method: Repeated Bootstrap
# control method function
train_control2<- trainControl(method = "boot", number=10)

# Model: SVM
model_svm2<-train(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed., data = new_data, trControl=train_control2, method = "svmRadial")

# Model: Neural Networks
model_nn2<-train(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed., data = new_data, trControl=train_control2, method = "nnet", verbose = FALSE)

# Model: Random Forests
model_rf2<-train(Company.response.to.consumer~Product+Issue+Submitted.via+Region+Timely.response.+Consumer.disputed., data = new_data, trControl=train_control2, method = "rf")

results2	<-	resamples(list(NN=model_nn2,	RF=model_rf2,	SVM=model_svm2))	

summary(results2)

bwplot(results2)

# Question 5: Predict the number of complaints for a company based on it's assets

# Comparative Analysis

library(readr)
library(caret)

# Open the dataset: assets.csv
assets <- read_csv(choose.file())
assets <- na.omit (assets)

attach(assets)


set.seed(5)

# Method: K-Fold cross validation
# control method function
train_control<- trainControl(method = "cv", number=10)

# Model: SVM
model_svm<-train(Complaint_Count~., data = assets, trControl=train_control, method = "svmRadial")

 # Model: Neural Networks
model_nn<-train(Complaint_Count~., data = assets, trControl=train_control, method = "nnet", verbose = FALSE)

# Model: Random Forests
model_rf<-train(Complaint_Count~., data = assets, trControl=train_control, method = "lm")

results	<-	resamples(list(NN=model_nn,	LR=model_rf,	SVM=model_svm))	

summary(results)

bwplot(results)

# Method: Repeated K-Fold cross validation
# control method function
train_control1<- trainControl(method = "repeatedcv", number=10, repeats = 3)

# Model: SVM
model_svm1<-train(Complaint_Count~., data = assets, trControl=train_control1, method = "svmRadial")

# Model: Neural Networks
model_nn1<-train(Complaint_Count~., data = assets, trControl=train_control1, method = "nnet", verbose = FALSE)

# Model: Random Forests
model_rf1<-train(Complaint_Count~., data = assets, trControl=train_control1, method = "lm")

results1	<-	resamples(list(NN=model_nn1,	LR=model_rf1,	SVM=model_svm1))	

summary(results1)

bwplot(results1)

# Method: LOOCV
# control method function
train_control2<- trainControl(method = "LOOCV", number=10)

# Model: SVM
model_svm2<-train(Complaint_Count~., data = assets, trControl=train_control2, method = "svmLinear")

# Model: Neural Networks
model_nn2<-train(Complaint_Count~., data = assets, trControl=train_control2, method = "nnet", verbose = FALSE)

# Model: Random Forests
model_rf2<-train(Complaint_Count~., data = assets, trControl=train_control2, method = "lm")

results2	<-	resamples(list(NN=model_nn2,	LM=model_rf2,	SVM=model_svm2))	

summary(results2)

bwplot(results2)

# Method: Bootstrap
# control method function
train_control2<- trainControl(method = "boot", number=10)

# Model: SVM
model_svm2<-train(Complaint_Count~., data = assets, trControl=train_control2, method = "svmLinear")

# Model: Neural Networks
model_nn2<-train(Complaint_Count~., data = assets, trControl=train_control2, method = "nnet", verbose = FALSE)

# Model: Random Forests
model_rf2<-train(Complaint_Count~., data = assets, trControl=train_control2, method = "lm")

results2	<-	resamples(list(NN=model_nn2,	LM=model_rf2,	SVM=model_svm2))	

summary(results2)
