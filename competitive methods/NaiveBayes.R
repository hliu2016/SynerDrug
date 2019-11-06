library(plyr)  #same in knn.R, 5_fold
CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]    #将数据分成K份，并生成的完成数据集n
  temp <- sample(n,datasize)   #把n打乱
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x])  #dataseq中随机生成k个随机有序数据列
  return(cvlist)
}

#benchmark <- read.csv("/root/zwhtest/GBT/t1class.csv", header= TRUE)
benchmark <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/rwr results/t_m_0.9.csv", header= TRUE)
benchmarkfeature<-benchmark[,-1] #delete interg
k <- 10
datasize <- nrow(benchmarkfeature)
cvlist <- CVgroup(k = k,datasize = datasize,seed = 1111)
data <- benchmarkfeature
pred <- data.frame()

library(e1071)
library(ROCR)
MCC_all<-vector(mode="numeric",length=0)
precision_all<-vector(mode="numeric",length=0)
recall_all<-vector(mode="numeric",length=0)
F1_all<-vector(mode="numeric",length=0)
AUC_all<-vector(mode="numeric",length=0)
for (i in 1:k){
  train <- benchmarkfeature[-cvlist[[i]],]  #刚才通过cvgroup生成的函数
  test <- benchmarkfeature[cvlist[[i]],]
  train$class=factor(train$class)
  test$class=factor(test$class)
  nb.model <- naiveBayes(class~.,data = train, probability = TRUE)
  prediction <- predict(nb.model,test[,!names(test) %in% c("class")],probability=TRUE)
  pred <- prediction(as.numeric(prediction),as.numeric(test$class))
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,'auc')
  auc = slot(auc,"y.values")
  table(test$class, prediction,dnn=c("true","pred"))
  TP<-as.numeric(table(test$class, prediction,dnn=c("true","pred"))[2,2])
  TN<-as.numeric(table(test$class, prediction,dnn=c("true","pred"))[1,1])
  FP<-as.numeric(table(test$class, prediction,dnn=c("true","pred"))[1,2])
  FN<-as.numeric(table(test$class, prediction,dnn=c("true","pred"))[2,1])
  MCC<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  MCC_all[i]<-MCC #预测
  precision<-TP/(TP+FP)
  precision_all[i]<-precision
  recall<-TP/(TP+FN)
  recall_all[i]<-recall
  F1<-(2*precision*recall)/(precision+recall)
  F1_all[i]<-F1
  AUC_all[i]<-auc[[1]]
}
print(paste(mean(precision_all),mean(recall_all),mean(F1_all),mean(MCC_all),mean(AUC_all)))
result <- c(mean(precision_all),mean(recall_all),mean(F1_all),mean(MCC_all),mean(AUC_all))
#write.table(t(as.matrix(result)), file="/root/zwhtest/GBT/RFresult.csv",col.names=F,row.names=F,sep=',',append=T) 
write.table(t(as.matrix(result)), file="E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/comparative methods/NBresult.csv",col.names=F,row.names=F,sep=',',append=T) 