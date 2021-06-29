#importing library
library(readxl)
library(caTools)
library(class)
#importing data 
data <-  read_excel("C:/Users/user1/Desktop/prototype/wbcd.xlsx")
str(data)
data<-data[,-1] #remove ID col
#missing data 
#no missing data
#categrical data 
data$diagnosis<- factor(x=data$diagnosis,levels =c("B","M"),labels = c(0,1))

#feature selection
library(FSelector)
library(ggplot2)
f<-information.gain(diagnosis~., data)
print(f)
subset <- cutoff.k(f, 2)
subset
ggplot(data, aes( area_worst,perimeter_worst, col=diagnosis) ) + 
  geom_point(size = 3, aes(pch = diagnosis))

#scaling data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_nor<-as.data.frame(lapply(data[2:31], normalize))
data<-cbind(data[,1],data_nor)
#data[,2:31]<-scale(data[,2:31])

#spliting data 
set.seed(123)   #so that same sample can be reproduced in future
sample<- sample.split(data,SplitRatio = 0.8)   
data_train<-subset(data,sample==TRUE)        #select 80% of data as a train sample
data_test<-subset(data,sample==FALSE)#select 20% of data as a test sample
#knn
y_pred <- knn(train = data_train[,-1],test = data_test[,-1],cl=data_train$diagnosis,k=23)
#confusion matrix
cm<-table(data_test$diagnosis,y_pred)
#evaluation 
ACC<-sum(diag(cm))/sum(cm)
SP<-cm[1,1]/sum(cm[,1])
SN<-cm[2,2]/sum(cm[,2])
#k-fold cross validation
folds<-cut(seq(1,nrow(data)),breaks=10,labels = FALSE) #split the data into 10 sections (k=10)
CV_ACC<-0
CV_SP<-0
CV_SN<-0
for(i in 1:10){
  CV_test_data<-data[folds==i,]
  CV_train_data<-data[folds!=i,]
  CV_pred<-knn(train =CV_train_data[,-1] ,test =CV_test_data[,-1],cl=CV_train_data$diagnosis,k=23)
  CV_cm<-table(CV_test_data$diagnosis,CV_pred)
  print(CV_cm)
  CV_ACC[i]<-sum(diag(CV_cm))/sum(CV_cm)
  CV_SP[i]<-cm[1,1]/sum(cm[,1])
  CV_SN[i]<-cm[2,2]/sum(cm[,2])
}
mean(CV_ACC)
mean(CV_SP)
mean(CV_SN)
