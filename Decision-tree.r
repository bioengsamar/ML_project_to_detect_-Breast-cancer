data.set <- read.csv("C:/Users/user1/Desktop/prototype/data-set.csv", sep=";")
#prepare data
data<-data.set

x<- is.na(data)#check if there's missing value
for (i in x){
  if(i ==TRUE){
    na.omit(x)
  }
}

data <- data[,-1] #remove 1st column(id number)

#categorical encoding
data[,1] <- factor(data[,1], levels=c('M' , 'B'), labels=c("malignant", "benign"))
print(summary(data))

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


# Decision tree with k-fold CV :
library(rpart)
k<- 10
folds <- cut(seq(1,nrow(data)), breaks = k,labels = F)
folds
accu_train<-0
SN_train<-0
SP_train<-0
accu_test<-0
SN_test<-0
SP_test<-0
for(i in 1:k){
test<- data[folds==i,]
train <- data[folds!=i,]
model<- rpart(diagnosis~.,data=train, method = "class")

tab<- table(predict(model,type = "class"),train$diagnosis)
print(tab)
accu_train[i]<-(sum(diag(tab))/sum(tab))
SN_train[i]<-(sum(tab[1,1])/sum(tab[,1]))
SP_train[i]<-(sum(tab[2,2])/sum(tab[,2]))

tested<- predict(model,type = "class",newdata=test)
tab_test<-table(tested,test$diagnosis)
print(tab_test)
accu_test[i]<-(sum(diag(tab_test))/sum(tab_test))
SN_test[i]<-(tab_test[1,1]/sum(tab_test[,1]))
SP_test[i]<-(tab_test[2,2]/sum(tab_test[,2]))


}

#get average
mean(accu_train)
mean(SN_train)
mean(SP_train)
mean(accu_test)
mean(SN_test)
mean(SP_test)



