#Milestone 1
#Team: Clusterbusters

#Phase 1: Data Cleaning

library(readr)
filename <- file.choose()
Consumer_Complaints <- read.csv(filename)
View(Consumer_Complaints)

##removing the consumer public response, tags and consumer consent columns from dataframe
Consumer_Complaints<-Consumer_Complaints[,-c(7,11,12)]
head(Consumer_Complaints)

##updating the names of the columns; substituted the blanks with periods
colnames(Consumer_Complaints)<-make.names(names(Consumer_Complaints))

##checking for duplicate entries in the dataset
ifelse (length(unique(Consumer_Complaints$Complaint.ID))==nrow(Consumer_Complaints), "No duplicate complaints in the dataset","Duplicates found")

##converting String Date to date object and segregate in new columns

chardates <- as.character(Consumer_Complaints$Date.received) #convert date to character
chardates <- gsub("/", "-", chardates) #change all records to same format
z <- as.Date(chardates,"%m-%d-%Y") #convert to date format
Consumer_Complaints$ComplaintYear <- format(z, "%Y") #extract year from date
Consumer_Complaints$ComplaintMonth <- format(z,"%m") #extract month from date
Consumer_Complaints$ComplaintDay <- format(z,"%d") #extract day from date

#Phase 2: Data Analysis

library(ggplot2)

##State wise distribution of complaints(expressed as a %)

cc = Consumer_Complaints
cc_state = aggregate(Complaint.ID~State,data=cc,FUN = length)
ggplot(cc_state,aes(x=State,,width=0.5)) + geom_bar(stat="identity",aes(y=Complaint.ID/sum(Complaint.ID)*100)) + geom_text(aes(label=paste(round(Complaint.ID/sum(Complaint.ID)*100,2),"%"),y=Complaint.ID/sum(Complaint.ID)*100),hjust=-0.2,check_overlap = TRUE,size=3) + ylim(0,round(max(cc_state$Complaint.ID)/sum(cc_state$Complaint.ID),2)*100) + coord_flip() + xlab(label="States")+ ylab(label="Percentage of complaints")

##Product-wise consumer complaint distribution

cc$Product = as.character(cc$Product)
cc_product = aggregate(Complaint.ID~Product,data=cc,FUN=length)
ggplot(cc_product,aes(x=Product,width=0.5)) + geom_bar(stat="identity",aes(y=Complaint.ID/sum(Complaint.ID)*100)) 
+ geom_text(aes(label=paste(round(Complaint.ID/sum(Complaint.ID)*100,2),"%"),y=Complaint.ID/sum(Complaint.ID)*100),hjust=-0.2,check_overlap = TRUE,size=3) + ylim(0,round(max(cc_product$Complaint.ID)/sum(cc_product$Complaint.ID),2)*100) + coord_flip() + xlab(label="Products") + ylab(label="Percentage of Complaints")

##Responsewise consumer complaint distribution

cc$Company.response.to.consumer = as.character(cc$Company.response.to.consumer)
cc_response = aggregate(Complaint.ID~Company.response.to.consumer,data=cc,FUN=length)
ggplot(cc_response,aes(x=Company.response.to.consumer,width=0.5)) + geom_bar(stat="identity",aes(y=Complaint.ID/sum(Complaint.ID)*100))
 + geom_text(aes(label=paste(round(Complaint.ID/sum(Complaint.ID)*100,2),"%"),y=Complaint.ID/sum(Complaint.ID)*100),vjust=-0.5,check_overlap = TRUE,size=3) + xlab(label="Company response") + ylab(label="Percentage of complaints") + theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust = 0.5)

## Plotting the percentage distribution of the consumer disputed attrbute

cc$Consumer.disputed = as.character(cc$Consumer.disputed.)
cc_dispute = aggregate(Complaint.ID~Consumer.disputed,data=cc,FUN=length)
ggplot(cc_dispute,aes(x=Consumer.disputed,,width=0.5)) + geom_bar(stat="identity",aes(y=Complaint.ID/sum(Complaint.ID)*100)) + geom_text(aes(label=paste(round(Complaint.ID/sum(Complaint.ID)*100,2),"%"),y=Complaint.ID/sum(Complaint.ID)*100),vjust=-0.5,check_overlap = TRUE,size=3) + xlab(label="Is Consumer disputed") + ylab(label="Percentage of complaints")

##Year Wise Complaints breakdown

ggplot(cc,aes(ComplaintYear)) + geom_bar(position = "dodge") + xlab(label="Complaint Year") + ylab(labe="Number of Complaints")

##Year/Company.response distribution

ggplot(cc,aes(ComplaintYear,fill=Company.response.to.consumer)) + geom_bar(position = "dodge") + xlab(label="Complaint Year") + ylab(label="Company response count")

##Year/Product distribution

ggplot(cc,aes(ComplaintYear,fill=Product)) + geom_bar(position = "dodge") + xlab(label="Complaint Year") + ylab(label="Complaint Count")

##Communication medium-wise complaint distribution

cc$Submitted.via = as.character(cc$Submitted.via)
cc_medium = aggregate(Complaint.ID~Submitted.via,data=cc,FUN=length)
ggplot(cc_medium,aes(x=Submitted.via,,width=0.5)) + geom_bar(stat="identity",aes(y=Complaint.ID/sum(Complaint.ID)*100)) + geom_text(aes(label=paste(round(Complaint.ID/sum(Complaint.ID)*100,2),"%"),y=Complaint.ID/sum(Complaint.ID)*100),vjust=-0.5,check_overlap = TRUE,size=3) +  ylab(label="Percentage of complaints") + xlab(label="Submitted via medium")

