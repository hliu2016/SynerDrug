data1 <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/ccfinalnormalized.csv",header = TRUE);
data2 <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/ppfinalnormalized.csv",header = TRUE);
data3 <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/STITCHcpmatrixnormalized.csv",header = TRUE);
data4 <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/STITCHpcmatrixnormalized.csv",header = TRUE);

rownames(data1) = data1$X  #drug nodes
data1 = data1[,-1]
cc = as.matrix(data1)  #drug similarity network

rownames(data2) = data2$X #protein nodes
data2 = data2[,-1]
pp = as.matrix(data2) #protein similarity network

rownames(data3) = data3$X
data3 = data3[,-1]
cp = as.matrix(data3)  #drug-protein association network

rownames(data4) = data4$X
data4 = data4[,-1]
pc = as.matrix(data4)


####construct original transition matrix
lambda = 0.9
##the probability of staying in one-type network, 1-lamda represents the probability of jumping to heterotype network
cp_sum = apply(cp, 1, sum)
for(i in 1:dim(cp)[1]){
  if(cp_sum[i] !=0){
    cc[i,] = lambda*cc[i,]
    cp[i,] = (1-lambda)*cp[i,]
  }
}

pc_sum = apply(pc, 1, sum)
for(i in 1:dim(pc)[1]){
  if(pc_sum[i] != 0){
    pp[i,] = lambda*pp[i,]
    pc[i,] = (1-lambda)*pc[i,]
  }
}

cc_cp = cbind(cc,cp)
pc_pp = cbind(pc, pp)

M = rbind(cc_cp, pc_pp)
write.csv(M,file="E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/M_0.9.csv")
###original transition matrix
####original probability matrix, see SynergyddrugMaster.py




###when replicating the reastrat processing, start from here, original transition matrix and probability vector are provided
###parallel R. refer to parallel multiply.R 
#loading transfer matrix
data5 <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/M.csv",header = TRUE);
rownames(data5) = data5$X
data5 = data5[,-1]
M = as.matrix(data5) 
M1=apply(M,1,as.numeric)

#loading initial probability matrix
data6 <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/m.0.csv",header = TRUE);
rownames(data6) = data6$X
data6 = data6[,-1]
m0 = as.matrix(data6) 
m01=apply(m0,1,as.numeric)

##rwr single core
restart.ratio <- 0.2
LIMIT_TOR <- 1e-10

L1_norm <- function(v){
  return(abs(v))
}

start.transfer.matrix <- function(M, m.0) {
  turn <- 0
  m.t <- m.0
  while (TRUE) {
    turn <- turn + 1
    m.t1 <- (1 - restart.ratio) * (M %*% m.t) + restart.ratio * m.0
    L1 <- L1_norm(m.t1 - m.t)
    if (L1 < LIMIT_TOR) {
      break
    }
    m.t <- m.t1
  }
  return(m.t1)
}

t1=start.transfer.matrix(M1,m01)
write.csv(t(t1),file="E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/t2.csv")#your path

###########################################another 
data5 <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/Mnonehead.csv",header = FALSE);
M = as.matrix(data5) 
M1=apply(M,1,as.numeric)
data6 <- read.csv("E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/m.0nonehead.csv",header = FALSE);
m0 = as.matrix(data6) 
m01=apply(m0,1,as.numeric)

###parallel multiply
# library(parallel)
# parallel.matrix.multiply <- function(A, B) {
#   if (ncol(A) != nrow(B)) {
#     stop("Matrix A and B, not confirm.")
#   }
#   nr <- detectCores() - 6 #线程-2
#   cl <- makeCluster(rep("localhost", nr)) #并行计算 
#   t0 <- proc.time() #程序执行时间
#   idx <- splitIndices(nrow(A), length(cl)) #将A按行分成cl份，list
#   #对idx每个list的元素遍历
#   Alist <- lapply(idx, function(ii) {
#     return(A[ii,,drop=F])
#   })
#   #返回length=核数的list,list[i]=(nrow(A)/cl)*nrow
#   ans <- clusterApply(cl, Alist,
#                       function(aa, BB) {
#                         return(aa %*% BB)  
#                       }, B
#   )#对每个cl进行矩阵乘积
#   t1 <- proc.time()
#   stopCluster(cl)
#   return(do.call(rbind, ans))#合并
# }
# 
# 
# restart.ratio <- 0.2
# LIMIT_TOR <- 1e-10
# 
# L1_norm <- function(v){
#   return(abs(v))
# }
# 
# start.transfer.matrix <- function(M, m.0) {
#   turn <- 0
#   m.t <- m.0
#   while (TRUE) {
#     turn <- turn + 1
#     m.t1 <- (1 - restart.ratio) * parallel.matrix.multiply(M,m.t) + restart.ratio * m.0
#     L1 <- L1_norm(m.t1 - m.t)
#     if (L1 < LIMIT_TOR) {
#       break
#     }
#     m.t <- m.t1
#   }
#   return(m.t1)
# }
# 
# t1=start.transfer.matrix(M1,m01)
# write.csv(t(t1),file="E:/zwh/education/graduated/1导师布置/小论文5（GTB）/code/code2/t2.csv")
