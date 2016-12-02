#####CHURN PREDICTION- RETAIL########

remove(list = ls())

####Read file
Online.Retail <- read.csv("~/Desktop/Retail models/Online Retail.csv")
setwd("~/Desktop/Retail models")

###Load data wrangling libraries
library(dplyr)
library(stringr)
library(tidyr)
library(caret)

###Lets analyse the structure to see which data type formats need to be changed
str(Online.Retail)
summary(Online.Retail)


factor(Online.Retail$Country)
factor(Online.Retail$InvoiceNo)
factor(Online.Retail$StockCode)
factor(Online.Retail$CustomerID)

####Need to create monetary variable by multiplying unit price by quantity, we do this using dplyr package
####This gives us total amount generated from particular items in a transaction
Online.Retail <- Online.Retail %>% mutate(itemamount = Quantity * UnitPrice)



####Lets see how many NA's we have in each column

sum(is.na(Online.Retail$CustomerID))
#Thats 135080 Na's out of 541909

sum(is.na(data3$Transactiondate))
#That's 308950 NA's

##Lets try complete cases to see how much data we lose
Online.Retail2 <- na.omit(Online.Retail)




##Lets clean the date time component column to contain only dates to enable 
##easy calculation of recency and frequency

Online.Retail2$InvoiceDate <- strptime(Online.Retail2$InvoiceDate, format='%m/%d/%Y')




####Now for the aggregations, lets aggregate per invoice first

#First we aggregte total amount for each transaction
aggdata1 <-aggregate(x = Online.Retail2$itemamount, by = list(Online.Retail2$InvoiceNo), FUN = sum)
colnames (aggdata1) <- c("InvoiceNo", "totalamount")
summary(aggdata1)


#Lets now aggregate the number of unique products ordered per transaction
aggdata2 <-aggregate(x = Online.Retail2$StockCode, by = list(Online.Retail2$InvoiceNo), FUN = length)
colnames (aggdata2) <- c("InvoiceNo", "Nouniqueproducts")
summary(aggdata2)

#We also need to aggregate the date for each transaction
aggdata3 <-aggregate(x = Online.Retail2$InvoiceDate, by = list(Online.Retail2$InvoiceNo), FUN = mean)
colnames (aggdata3) <- c("InvoiceNo", "Transactiondate")
summary(aggdata3)

#Aggregation by max for customer ID
aggdata4 <-aggregate(x = Online.Retail2$CustomerID, by = list(Online.Retail2$InvoiceNo), FUN = max)
colnames (aggdata4) <- c("InvoiceNo", "customerID")

str(aggdata3)
str(aggdata4)

#Let's combine the aggdata
data <- merge(aggdata1,aggdata2, by="InvoiceNo")
data2 <- merge(data,aggdata3, by="InvoiceNo")
data3 <- merge(data2,aggdata4, by="InvoiceNo")
str(data3)

data3 <- na.omit(data3)



####Frequency calc function- called from different script, churn definition- 6 months
####For frequency we just count the number of different transaction id's per customer using aggregation

aggdata5 <-aggregate(x = data3$InvoiceNo, by = list(data3$customerID), FUN = length)
colnames (aggdata5) <- c("customerID", "Frequency")
str(aggdata5)
#(This shows we have complete cases for all transactions for only 3100 customers)


# Now lets get the last date data for each customer
aggdata6 <-aggregate(x = data3$Transactiondate, by = list(data3$customerID), FUN = max)
colnames (aggdata6) <- c("customerID", "Lasttransactiondate")
str(aggdata6)

# Now we get the average amount and total amount for each customer's transactions
aggdata7 <-aggregate(x = data3$totalamount, by = list(data3$customerID), FUN = mean)
colnames (aggdata7) <- c("customerID", "Avgamount")
str(aggdata7)

aggdata8 <-aggregate(x = data3$totalamount, by = list(data3$customerID), FUN = sum)
colnames (aggdata8) <- c("customerID", "totalamount")
str(aggdata8)

data4 <- merge(aggdata5,aggdata6, by="customerID")
data5 <- merge(data4, aggdata7, by="customerID")
data6 <- merge(data5, aggdata8, by="customerID")

str(data6)
summary(data6)


####Now lets get to the churn variable creation, lets say churn is defined as not buying a product in the last 5 months
#### Note our data runs from 12-01-2011 to 12-09-2012

data6$churn <- ifelse(data6$Lasttransactiondate > '0011-11-10',"0", "1")
str(data6)
summary(data6)

#####Now all thats left to do is change variables types to their appropriate class then we can rune machine learning algorithms

#Churn to factor
data6$churn <- as.character(extract_numeric(data6$churn))
data6$churn <- as.factor(extract_numeric(data6$churn))

#Customerid to factor
data6$customerID <- as.character(extract_numeric(data6$customerID))
data6$customerID <- as.factor(extract_numeric(data6$customerID))






#####TRAIN TEST SPLIT- 80 percent training split
set.seed(999)
trainIndex <- createDataPartition(data6$churn, p = .8,list = FALSE, times = 1)
head(trainIndex)

dataTrain <- data6[ trainIndex,]
dataTest  <- data6[-trainIndex,]


#lets try a simple random forest with 5 fold cross validation in training
modelrf <- train(churn ~ Frequency + Lasttransactiondate + totalamount + Avgamount, data = dataTrain, method = "rf", trControl = trainControl(method = "cv", number = 10))
modelrf

confusionMatrix(modelrf)


#SVM next
modelsvm <- train(churn ~ Frequency + Lasttransactiondate + totalamount + Avgamount, data = dataTrain, method = "svmRadialWeights", trControl = trainControl(method = "cv", number = 10))
modelsvm

confusionMatrix(modelsvm)


#Lets try XGB

modelxgb <- train(churn ~ Frequency + Lasttransactiondate + totalamount + Avgamount, data = dataTrain, method = "xgbTree", trControl = trainControl(method = "cv", number = 10))
modelxgb

confusionMatrix(modelxgb)

#Now naive bayes


modelnb <- train(churn ~Frequency + Lasttransactiondate + totalamount + Avgamount, data = dataTrain, method = "nb", trControl = trainControl(method = "cv", number = 10))
modelnb

confusionMatrix(modelnb)

###Lets look at the results on training data from the cross validation
results <- resamples(list(RF=modelrf, XGB=modelxgb, SVM=modelsvm, NB=modelnb))
summary(results)
dotplot(results)

#RF
testPred <- predict(modelrf, dataTest)
postResample(testPred, dataTest$churn)
sensitivity(testPred, dataTest$churn)
confusionMatrix(testPred, dataTest$churn)

#SVM
testPred <- predict(modelsvm, dataTest$churn)
postResample(testPred, dataTest$churn)
sensitivity(testPred, dataTest$churn)
confusionMatrix(testPred, dataTest$churn)

#XGB
testPred <- predict(modelxgb, dataTest$churn)
postResample(testPred, dataTest$churn)
sensitivity(testPred, dataTest$churn)
confusionMatrix(testPred, dataTest$churn)

#NB
testPred <- predict(modelnb, dataTest$churn)
postResample(testPred, dataTest$churn)
sensitivity(testPred, dataTest$churn)
confusionMatrix(testPred, dataTest$churn)









