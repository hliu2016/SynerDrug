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
benchmark <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/rwr results/t1class.csv", header= TRUE)
benchmarkfeature<-benchmark[,-1] #delete interg
k <- 10
datasize <- nrow(benchmarkfeature)
cvlist <- CVgroup(k = k,datasize = datasize,seed = 1111)
data <- benchmarkfeature
pred <- data.frame()
library(caTools)
library(pROC)
mnum <- seq(5,100,by = 5)
for(j in mnum){
    MCC_all<-vector(mode="numeric",length=0)
    precision_all<-vector(mode="numeric",length=0)
    recall_all<-vector(mode="numeric",length=0)
    F1_all<-vector(mode="numeric",length=0)
    AUC_all<-vector(mode="numeric",length=0)
    for (i in 1:k){
      train <- benchmarkfeature[-cvlist[[i]],]  #刚才通过cvgroup生成的函数
      test <- benchmarkfeature[cvlist[[i]],]
      train$class=as.factor(train$class)
      test$class=as.factor(test$class)
      logitboostmodel <- LogitBoost(train[,-6075],train[,6075],nIter =j)
      prediction <- predict(logitboostmodel,test[,-6065])
      prob<-predict(logitboostmodel,test[,-6065],type="raw")
      table(test$class,prediction,dnn=c("真实值","预测值"))
      roc <- roc(test$class,as.numeric(prediction))
      auc <- roc[["auc"]][1]
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
      AUC_all[i]<-auc
    }
    print(paste(j,mean(precision_all),mean(recall_all),mean(F1_all),mean(MCC_all),mean(AUC_all)))
    result <- c(j,mean(precision_all),mean(recall_all),mean(F1_all),mean(MCC_all),mean(AUC_all))
    #write.table(t(as.matrix(result)), file="/root/zwhtest/GBT/Adaresult.csv",col.names=F,row.names=F,sep=',',append=T) 
    write.table(t(as.matrix(result)), file="E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/comparative methods/LogitBoostresult.csv",col.names=F,row.names=F,sep=',',append=T)
}