
####loading transfer matrix
data5 <- read.csv("/root/zwhtest/GBT/Mnonehead.csv",header = FALSE);
M = as.matrix(data5) 
M1=apply(M,1,as.numeric)

#loading initial probability matrix
data6 <- read.csv("/root/zwhtest/GBT/m.0nonehead.csv",header = FALSE);
m0 = as.matrix(data6) 
m01=apply(m0,1,as.numeric)




#############rwr
###parallel multiply
library(parallel)
parallel.matrix.multiply <- function(A, B) {
  if (ncol(A) != nrow(B)) {
    stop("Matrix A and B, not confirm.")
  }
  nr <- detectCores() - 4 #线程-4
  cl <- makeCluster(rep("localhost", nr)) #并行计算 
  t0 <- proc.time() #程序执行时间
  idx <- splitIndices(nrow(A), length(cl)) #将A按行分成cl份，list
  #对idx每个list的元素遍历
  Alist <- lapply(idx, function(ii) {
    return(A[ii,,drop=F])
  })
  #返回length=核数的list,list[i]=(nrow(A)/cl)*nrow
  ans <- clusterApply(cl, Alist,
                      function(aa, BB) {
                        return(aa %*% BB)  
                      }, B
  )#对每个cl进行矩阵乘积
  t1 <- proc.time()
  stopCluster(cl)
  return(do.call(rbind, ans))#合并
}


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
    m.t1 <- (1 - restart.ratio) * parallel.matrix.multiply(M,m.t) + restart.ratio * m.0
    L1 <- L1_norm(m.t1 - m.t)
    if (L1 < LIMIT_TOR) {
      break
    }
    m.t <- m.t1
  }
  return(m.t1)
}

t1=start.transfer.matrix(M1,m01)
write.csv(t(t1),file="/root/zwhtest/GBT/t2.csv")
