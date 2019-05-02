

data1 <- read.csv("DCDBccguiihua.csv",header = TRUE);
data2 <- read.csv("DCDBppguiihua.csv",head = TRUE);
data3 <- read.csv("drugsimilirityguiyihua.csv",head = TRUE);
data4 <- read.csv("proteinsimilarityguiyihua.csv",head = TRUE);
data5 <- read.csv("cpguiihua.csv",head = TRUE);
data6 <- read.csv("pcguiihua.csv",head = TRUE);


rownames(data1) = data1$X
data1 = data1[,-1]
cc_1 = as.matrix(data1)


rownames(data2) = data2$X
data2 = data2[,-1]
pp_1 = as.matrix(data2)

rownames(data3) = data3$X
data3 = data3[,-1]
cc_2 = as.matrix(data3)


rownames(data4) = data4$X
data4 = data4[,-1]
pp_2 = as.matrix(data4)


rownames(data5) = data5$X
data5 = data5[,-1]
pc = as.matrix(data5)

rownames(data6) = data6$X
data6 = data6[,-1]
cp = as.matrix(data6)





deta = 0.7
cc = matrix(nrow=dim(cc_1)[1], ncol=dim(cc_1)[2])
sum_cc1 = apply(cc_1, 1, sum)
for(i in 1:dim(cc_1)[1]){
  if(sum_cc1[i] > 0){
    cc[i,] = deta*cc_1[i,] + (1-deta)*cc_2[i,]
  }
  else{
    cc[i,] = cc_2[i,]
  }
}

pp = matrix(nrow=dim(pp_1)[1], ncol=dim(pp_1)[2])
sum_pp1 = apply(pp_1, 1, sum)
for(i in 1:dim(pp_1)[1]){
  if(sum_pp1[i] > 0 ){
    pp[i,] = deta*pp_1[i,] + (1-deta)*pp_2[i,]
  }
  else{
    pp[i,] = pp_2[i,]
  }
    
}


lambda = 0.5
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

write.csv(M,file="hebing.csv")



m0 <- read.csv("D:/Documents/m.0.csv",header =  FALSE); 
m1 <- read.csv("D:/Documents/m.1.csv",header =  FALSE);

M <- as.matrix(M)
m0 <- as.matrix(m0)
m1 <- as.matrix(m1)
is.matrix (M)
is.matrix (m1)
is.matrix (m0)


restart.ratio <- 0.2
LIMIT_TOR <- 1e-10

L1_norm <- function(v){
  return(abs(v))
}



start.transfer.vector <- function(M, p.0) {
  turn <- 0
  p.t <- p.0
  while (TRUE) {
    turn <- turn + 1
    p.t1 <- (1 - restart.ratio)* (t(M)%*%p.t) + restart.ratio * p.0
    L1 <- L1_norm(p.t1 - p.t)
    if (L1 < LIMIT_TOR) {
      break
    }
    p.t <- p.t1
  }
  return(p.t1)
}



start.transfer.matrix <- function(M, m.0) {
  turn <- 0
  m.t <- m.0
  while (TRUE) {
    turn <- turn + 1
    m.t1 <- (1 - restart.ratio) * (t(M) %*% m.t) + restart.ratio * m.0
    L1 <- L1_norm(m.t1 - m.t)
    if (L1 < LIMIT_TOR) {
      break
    }
    m.t <- m.t1
  }
  return(m.t1)
}


t1=start.transfer.matrix(M,t(m1))
t0=start.transfer.matrix(M,t(m0))

write.csv(t1,file="t1.csv")
write.csv(t0,file="t0.csv")

trainingset = rbind(t1,t0)

write.csv(trainingset,file="trainingset.csv")








