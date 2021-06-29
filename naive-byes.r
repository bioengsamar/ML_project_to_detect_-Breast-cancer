data<- read.csv("C:/Users/user1/Desktop/prototype/data-set.csv", sep=";")
data<-data
str(data)
x<- is.na(data)#check if there's missing value


for (i in x){
  if(i ==TRUE){
    na.omit(x)
  }
}

data <- data[,-1] #remove 1st column(id number)
data[,1] <- factor(data[,1], levels=c('M' , 'B'), labels=c("malignant", "benign"))

#feature selection
library(Boruta)
library(ggplot2)
set.seed(1234)
bor<-Boruta(diagnosis~.,data = data,doTrace=1)
print(bor)
plot(bor,las=2)
borr<-TentativeRoughFix(bor)
print(borr)
attStats(borr)
ggplot(data, aes(area_worst, concave.points_worst, col=diagnosis) ) + 
  geom_point(size = 3, aes(pch = diagnosis))

#print(summary(data))

library(caret)
k<- 10
folds <- cut(seq(1,nrow(data)), breaks = k,labels = F)
folds
library(e1071)
trainAcc <-0
testAcc <-0
SN_train<-0
SP_train<-0
SN_test<-0
SP_test<-0
for(i in 1:k){
  test<- data[folds==i,]
  train <- data[folds !=i,]
  
  model1<-naiveBayes(diagnosis~. ,data=train , type = c("class"))
  
  trainTable=table(predict(  model1,newdata = train, type = "class"),train$diagnosis)
  testTable=table(test$diagnosis,predict(model1, newdata=test, type="class"))
  trainAcc[i] <-sum(diag(trainTable))/sum(trainTable)
  SN_train[i]<-trainTable[1,1]/sum(trainTable[,1])
  SP_train[i]<-tab[2,2]/sum(tab[,2])
  testAcc[i]<-sum(diag(testTable))/sum(testTable)
  SN_test[i]<-testTable[1,1]/sum(testTable[,1])
  SP_test[i]<-testTable[2,2]/sum(testTable[,2])

  
}

#get average
mean( trainAcc)
mean(SN_train)
mean(SP_train)
mean(testAcc)
mean(SN_test)
mean(SP_test)




