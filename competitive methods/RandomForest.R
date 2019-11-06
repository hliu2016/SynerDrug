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
benchmark <- read.csv("/root/zwhtest/GBT/t_m_0.9.csv", header= TRUE)
benchmarkfeature<-benchmark[,-1] #delete interg
k <- 5
datasize <- nrow(benchmarkfeature)
cvlist <- CVgroup(k = k,datasize = datasize,seed = 1111)
data <- benchmarkfeature
pred <- data.frame() 
library(plyr)
library(randomForest)
library(ROCR)
m <- seq(60,500,by = 20)  #m是RF重要超参数，必须tuning
for(j in m){   
  # progress.bar <- create_progress_bar("text")  #plyr包中的create_progress_bar函数创建一个进度条，
  # progress.bar$init(k)   #设置上面的任务数，几折就是几个任务
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
    model <-randomForest(class~.,data = train,ntree = j)   #建模，ntree=j 指的树数
    prediction <- predict(model,test[,!names(test) %in% c("class")],type = 'class' )
    pred <- prediction(as.numeric(prediction),as.numeric(test$class))
    perf <- performance(pred,"tpr","fpr")
    auc <- performance(pred,'auc')
    auc = slot(auc,"y.values")
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
    # randomtree <- rep(j,length(prediction))   #随机森林树的数量
    # kcross <- rep(i,length(prediction))   #i是第几次循环交叉，共K次
    # temp <- data.frame(cbind(subset(test,select = class),prediction,randomtree,kcross))
    # pred <- rbind(pred,temp)   #查看结果
    # print(paste("RF：",j))  #循环至树数j的随机森林模型
    # progress.bar$step() #输出进度条。可删除
  }
  print(paste(j,mean(precision_all),mean(recall_all),mean(F1_all),mean(MCC_all),mean(AUC_all)))
  result <- c(j,mean(precision_all),mean(recall_all),mean(F1_all),mean(MCC_all),mean(AUC_all))
  write.table(t(as.matrix(result)), file="/root/zwhtest/GBT/RFresult.csv",col.names=F,row.names=F,sep=',',append=T) 
 # write.csv(t(as.matrix(result)),file="E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/rwr results/RFresult.csv")
 # print(paste("RF：",j,"AUC: ",mean(AUC_all)))
}


