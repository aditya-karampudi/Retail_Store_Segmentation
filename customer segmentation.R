rm(list = ls(all.names = TRUE))


setwd("C://Users//mohad//OneDrive//Desktop//New_Github//Retail_Store_Segmentation")
getwd()


dataretail<-read.csv("DataRetail.csv", header = T, sep=",")

library(lubridate)
############ extracting date ######## 
dataretail$Transaction.Time<-dmy_hm(dataretail$Transaction.Time)
dataretail$Transaction.Time<-as.Date( as.POSIXlt(dataretail$Transaction.Time,tz= "UTC"))

#######
#k=aggregate(dataretail,by = list(dataretail$TransactionID),FUN = sum)

head(dataretail$Transaction.Time)
##MOnetary(total amount spend by each customer)
library(dplyr)

data_rt<- dataretail %>%
  group_by(CustomerID) %>%
  summarise(sumcost=sum(Cost), no.ofproducts = length(ProductID), mins=min(Cost),maxs=max(Cost)) %>%
  as.data.frame

DT = unique(dataretail[c("CustomerID", "TransactionID")])
data_rt$Frequency=aggregate(TransactionID ~ CustomerID, DT, function(x) length(unique(x)))$TransactionID
rm(DT)

##first and last arrival date of each customer
first_last_date<-dataretail %>% group_by(CustomerID) %>% summarise(FirstDate = min(Transaction.Time), LastDate = max(Transaction.Time)  ) %>%
  as.data.frame
first_last_date$CustomerID<-NULL
data_rt$CustomerID<- NULL
##No. of days in-between first arrival &last arrival of each customer
y<- as.Date("31/12/2016", format="%d/%m/%Y")
recency<- as.data.frame(y-first_last_date$LastDate) #subracting last date visitdate(each cust)
tenure<-as.data.frame(first_last_date$LastDate-first_last_date$FirstDate);

data_rt<- cbind(data_rt,first_last_date, recency,tenure)  #check data_set

##removing unwanted 
first_last_date<-NULL
data_rt$FirstDate<- NULL
data_rt$LastDate<- NULL
recency<- NULL

##final dataset
names(data_rt) <- c("Revenue","Quantity","Minimum Cost","Maximum Cost", "Frequency", "Recency","Tenure") # And change names

str(data_rt)
data_rt$Recency=as.integer(data_rt$Recency)
data_rt$Tenure=as.integer(data_rt$Tenure)


##PROGRAM##

data_rt1=as.data.frame(data_rt)
####standardizing the data for clustering###############
library(vegan)
data_std<-decostand(data_rt1,"standardize",MARGIN = 2)
sum((is.na(data_std)))
data_std_hclust<-data_std## copy of data for hclust
data_std_hclust1<-data_std

######Applying Hierarchial CLustering #####
distmatrix <- dist(data_std_hclust,method = "euclidean") # distance matrix
retail_hclust <- hclust(distmatrix, method="ward.D2")
plot(retail_hclust) # display dendogram
groups <- cutree(retail_hclust, k=6)
groups
rect.hclust(retail_hclust, k=6, border="red") 
data_std_hclust$cluster <- groups
head(data_std_hclust)

####To check NO of ouliers###
a=summary(data_rt$Revenue)[5]-summary(data_rt$Revenue)[2]
b=(1.5*a) +summary(data_rt$Revenue)[5]
c= - (1.5*a) +summary(data_rt$Revenue)[2]
d=nrow(subset(data_rt, Revenue>b))
e=nrow(subset(data_rt, Revenue<c))
### to display the total data points in each cluster

library(dplyr)

data_std_hclust %>%
  group_by(cluster) %>%
  summarise(No_of_obs = n())

summary(retail_hclust)


####Applying K-means Algorithm######
# Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(data_std,centers=i)$withinss)
}

#Scree Plot
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

#############fit6################
fit6<-kmeans(data_std,6)
#With-in sum of squares in each cluster 
fit6$withinss 
sum(fit6$withinss)  ##suming withinss
summary(fit6)
###Insight
fit6$centers
fit6$size


#To check cluster number of each row in data
Cluster_No.<-fit6$cluster

#Cluster Centers 
Cluster_Centres<-fit6$centers 

# get cluster means
aggregate(data_std,by=list(fit6$cluster),FUN=mean)

# append cluster label to the actual data frame
fit6_withclusterNo <- data.frame(data_rt1,fit6$cluster) 
head(fit6_withclusterNo)

fit6_withclusterNo$fit6.cluster<-as.factor(fit6_withclusterNo$fit6.cluster)
fit6_withclusterNo$Quantity<-as.numeric(fit6_withclusterNo$Quantity)
str(fit6_withclusterNo)

#####


### to display the total data points in each cluster

library(dplyr)

fit6_withclusterNo %>%
  group_by(fit6.cluster) %>%
  summarise(No_of_obs = n())

comb= cbind(data_rt1,CustomerID= unique(dataretail$CustomerID))
score_data=as.data.frame(comb)
str(score_data)
score_data$Tenure = ifelse(score_data$Tenure==0,0.1,score_data$Tenure)
score_data$Recency = ifelse(score_data$Recency==0,1,score_data$Recency)
score_data$Quantity<-as.numeric(score_data$Quantity)
score_data$Score=(score_data$Revenue)*sqrt(score_data$Frequency)*(score_data$Quantity)/sqrt(score_data$Recency)
k = quantile(score_data$Score,0.9)
score_data$Score = scale(score_data$Score, center = FALSE, scale = k/100)
Sub = subset(score_data, Revenue > 500)
score_data$Score=ifelse(score_data$Score>100,100,score_data$Score)
summary(score_data$Score)
table(round(score_data$Score))





#####BOX PLOT of clusters################
fit6$centers
Boxplot<-boxplot(fit6$centers[1:6])
Boxplot<-boxplot(fit6$centers[37:42])


#####dividind into train and test for classification##############
rows<-seq(1,nrow(fit6_withclusterNo),1)
set.seed(1234)
trainrows<-sample(rows,0.7*nrow(fit6_withclusterNo))
Train<-fit6_withclusterNo[trainrows,]
Test<-fit6_withclusterNo[-trainrows,]


fit6_withclusterNo$Quantity<-as.numeric(fit6_withclusterNo$Quantity)

#######Building a decision tree with C5.0####

library(C50)
Model_C50 <-C5.0(Train[,-8],Train[,8])
Model_C50
summary(Model_C50)

library(caret)
#Predicting on Train
P1_train=predict(Model_C50,Train);P1_train
table(Train[,8],Predicted=P1_train)

confusionMatrix(P1_train,Train$fit6.cluster)

#Predicting on Test
P1_test = predict(Model_C50,Test);P1_test
table(Test[,8],Predicted=P1_test)
predict(Model_C50,Test,rules=T)
confusionMatrix(P1_test,Test$fit6.cluster)
plot(Model_C50, subtree = 10)

####GGPLOT################

library(ggplot2)
set.seed(20)

#dataretailCluster$cluster <- as.factor(dataretailCluster$cluster)
#p1<-ggplot(fit6_withclusterNo)+aes(fit6_withclusterNo$Recency,new$Monetary,color = factor(fit6$cluster)) + geom_point(size=3)
#p1

#summary(dataretail)




######building Rpart for 6 clusters###################

library(rpart)
library(rpart.plot)
library(party)

Model_rpart= rpart(fit6.cluster~.,data=Train, method="class")
plot(Model_rpart,main="Classifcation Tree for Amount",
     margin=0.15,uniform=TRUE)
text(Model_rpart,use.n=T)


##Predicting on Train
P1_train=predict(Model_rpart,Train,type = "class")
tr=P1_train
table(Train[,8],Predicted=P1_train)

confusionMatrix(P1_train,Train$fit6.cluster)

#Predicting on Test
P1_test = predict(Model_rpart,Test,type = "class")
te=P1_test
table(Test[,8],Predicted=P1_test)

confusionMatrix(P1_test,Test$fit6.cluster)

library(rpart.plot)
rpart.plot(Model_rpart)
require(fpc)
require(cluster)
plotcluster(data_std,fit6$cluster,clnum = 3,method = "dc")

k=cbind(score_data,Cluster= fit6_withclusterNo$fit6.cluster)
k= k[,c("CustomerID","Cluster")]
main=as.data.frame(dataretail) 
main=merge(main,k,by='CustomerID')

#####Apriori for k=6#####
library('arules')

# Rules for cluster 6,5,1 since they are our prioirty customers in that order
for (i in 1:6)
{
  if(i==1||i==5||i==6)
  {
    sub_data = main[main$Cluster == i, c("TransactionID","ProductID")] 
    sub_data$ProductID = as.factor(sub_data$ProductID)
    sub_data$TransactionID = as.factor(sub_data$TransactionID)
    sub_data = droplevels(sub_data)
    trans = as(split(sub_data[,2], sub_data[,1]), "transactions")
    inspect(trans)
    rules = apriori(trans,parameter = list(sup = 0.001, conf = 0.7,target="rules"))
    summary(rules)
    inspect(rules)
    p= as(rules, "data.frame")
    write.csv(p,file = paste("cluster",i,".csv") )
  }
}


######Applying Hieratchial CLustering #####
distmatrix1 <- dist(data_std_hclust1,method = "euclidean") # distance matrix
retail_hclust1 <- hclust(distmatrix1, method="ward.D2")
plot(retail_hclust1) # display dendogram
groups1 <- cutree(retail_hclust1, k=5)
groups1
rect.hclust(retail_hclust1, k=5, border="red") 
data_std_hclust1$cluster <- groups1
head(data_std_hclust1)

### to display the total data points in each cluster

library(dplyr)

data_std_hclust1 %>%
  group_by(cluster) %>%
  summarise(No_of_obs = n())



##################with k=5 fit5#######################

fit5<-kmeans(data_std,5)
#With-in sum of squares in each cluster 
fit5$withinss 
sum(fit5$withinss)  ##suming withinss

fit5$centers
fit5$size

# append cluster label to the actual data frame fit 3
fit5_withclusterNo <- data.frame(data_rt1,fit5$cluster) 
head(fit5_withclusterNo)

fit5_withclusterNo$fit5.cluster<-as.factor(fit5_withclusterNo$fit5.cluster)
fit5_withclusterNo$Quantity<-as.numeric(fit5_withclusterNo$Quantity)
str(fit5_withclusterNo)



#To check cluster number of each row in data
Cluster_No.5<-fit5$cluster5

#Cluster Centers 
Cluster_Centres5<-fit5$centers5 

# get cluster means
aggregate(data_std,by=list(fit5$cluster),FUN=mean)

### to display the total data points in each cluster

library(dplyr)

fit5_withclusterNo %>%
  group_by(fit5.cluster) %>%
  summarise(No_of_obs = n())
as.factor(fit5$cluster)

#####dividind into train and test##############
rows5<-seq(1,nrow(fit5_withclusterNo),1)
set.seed(1234)
trainrows5<-sample(rows5,0.7*nrow(fit5_withclusterNo))
Train5<-fit5_withclusterNo[trainrows5,]
Test5<-fit5_withclusterNo[-trainrows5,]

fit5_withclusterNo$Quantity<-as.numeric(fit5_withclusterNo$Quantity)
str(Train5)

####Building C5.0 tree for k=5###
library(C50)
Model_C505 <-C5.0(Train5[,-c(8)],factor(Train5$fit5.cluster),rules=F)
plot(Model_C505)
Model_C505
summary(Model_C505)


##############rpart for k=5###########
Model_rpart5= rpart(Train5$fit5.cluster~.,data=Train5, method="class")
plot(Model_rpart5,main="Classifcation Tree for Amount",
     margin=0.15,uniform=TRUE)
text(Model_rpart5,use.n=T)

rpart.plot(Model_rpart5)

#Predicting on Train
P1_train5=predict(Model_rpart5,Train5,type = "class")
tr5=P1_train5
table(Train5[,8],Predicted=P1_train5)

confusionMatrix(P1_train5,Train5$fit5.cluster)

#Predicting on Test
P1_test5 = predict(Model_rpart5,Test5,type = "class")
te5=P1_test5
table(Test5[,8],Predicted=P1_test5)

confusionMatrix(P1_test5,Test5$fit5.cluster)




require(fpc)
require(cluster)
plotcluster(data_rt1,fit6$cluster,clnum = 6,method = "dc")











