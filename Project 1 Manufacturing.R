
getwd()
setwd("D:/Practice R/Project working directory")

data=read.csv('product_train.csv')

data=read.csv('product_train.csv',stringsAsFactors = T)
str(data)

#Missing value check
sapply(data, function(x) sum(is.na(x)))

#As data is large so checking distrubution of target variables
data_yes=data[data$went_on_backorder=="Yes",]
data_no=data[data$went_on_backorder=="No",]
nrow(data_yes) #1615
nrow(data_no)  #248463

#As we can see this is imbalanced data so need to balance them
#implementing down sampling to avoid imbalance data
set.seed(100)
library(dplyr)
data_no_sam=sample_n(data_no, size = nrow(data_yes), replace = T)
dim(data_no_sam)

data_2=rbind(data_yes,data_no_sam)
dim(data_2)
str(data_2)

#converting target value in 0 and 1
data_2$went_on_backorder<-factor(data_2$went_on_backorder,label=c(0,1))

#creating train and test data
set.seed(10)
sample_index=sample(2,nrow(data_2),replace = T,prob = c(0.7,0.3))
train_data=data_2[sample_index==1,]
test_data=data_2[sample_index==2,]
colnames(train_data)  
summary(train_data) 

#contrasts(train_data$went_on_backorder) # returns the coding that R have used to create the dummy variables


glm_model=glm(went_on_backorder~.,train_data,family="binomial")
summary(glm_model)

pred=predict(glm_model,newdata=test_data,type="response")
pred=ifelse(pred>0.5,1,0)
confusion_matrix=table(test_data$went_on_backorder,pred)
confusion_matrix
accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

library(randomForest)
#train_data
rf.model=randomForest(went_on_backorder~.,data=train_data,ntree = 1000,importance = T)
rf.model
plot(rf.model)

varImpPlot(rf.model,n.var = 5)

pred.rf=predict(rf.model,newdata = test_data,n.trees = 1000,type="response")
#pred.rf
confusion_matrix=table(test_data$went_on_backorder,pred.rf)
(acc=sum(diag(confusion_matrix))/sum(confusion_matrix)) # 0.86

# GBM
library(gbm)

gbm.model=gbm((unclass(went_on_backorder)-1)~.,data = train_data,n.trees = 1000,distribution = "bernoulli")
summary(gbm.model)

pred.gbm=predict(gbm.model,newdata = test_data,n.trees = 1000,type="response")
pred.gbm=ifelse(pred.gbm>0.5,1,0)
confusion_matrix=table((unclass(test_data$went_on_backorder)-1),pred.gbm)
acc=sum(diag(confusion_matrix))/sum(confusion_matrix)
acc
