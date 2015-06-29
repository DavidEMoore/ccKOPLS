ccSVM.demo <- function(X,y,L,kfold,t){

#setting up to select parameter lambda and C
LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
CRange <- c(2^-8,2^-4,2^-2,2^0,2^2,2^4,2^8)

#kfold CV
library(permute)
size <- round(nrow(X)/kfold)
test.inxs <- list()
for(i in 1:kfold){
  start <- 1 + size*(i-1)
  end <- min(nrow(X),size + size*(i-1))
  test.inxs[[i]] <- t[start:end]
}

cckcauc <- matrix(0, nrow=1,ncol=kfold)
kcauc <- matrix(0, nrow=1, ncol=kfold)
ccm <- list()
ccr <- list()
cclabels <- list()
m <- list()
r <- list()
labels <- list()

for(i in 1:length(test.inxs)){
  train.X <- X[-test.inxs[[i]],-test.inxs[[i]]]
  test.X <- X[test.inxs[[i]],test.inxs[[i]]]
  train.y <- y[-test.inxs[[i]]]
  test.y <- y[test.inxs[[i]]]
  train.L <- L[-test.inxs[[i]],-test.inxs[[i]]]
  test.L <- L[test.inxs[[i]],test.inxs[[i]]]
  
  #parameter selection by kfold cross validation based only on training data
  C <- parameter.setting(train.X,train.y,train.L,CRange,2)
  lambda <- lambda.setting(train.X,train.y,train.L,LambdaRange,C,2)
  
  #to do prediction on test data using ccSVM
  ccSVM.predict <- ccSVM(X,y,L,lambda,C,2)
  cckcauc[1,i] <- ccSVM.predict[[1]]
  ccm[[i]] <- ccSVM.predict[[2]]
  cclabels[[i]] <- ccSVM.predict[[3]]
  ccr[[i]] <- ccSVM.predict[[4]]
  
  #to do prediction on test data using standard SVM, setting lambda as 0
  cauc <- ccSVM(X,y,L,0,C,2)
  kcauc[1,i] <- cauc[[1]]
  m[[i]] <- cauc[[2]]
  labels[[i]] <- cauc[[3]]
  r[[i]] <- cauc[[4]]
}

return (list(cckcauc,kcauc,ccm,m,cclabels,labels,ccr,r))
#bye bye



# setwd('~/Downloads')
# df <- read.csv('breast-cancer-wisconsin.data.txt', header=FALSE, col.names=c('Sample', 
#                'Thickness', 'CellSize', 'CellShape', 'Adhesion', 'EpiCellSize', 'Nuclei', 
#                'Chromatin', 'Nucleoli', 'Mitoses', 'Class'), na.strings = '?')
# 
# idx <- round(runif(round(.6*nrow(df)), 1, nrow(df)))
# df.train <- df[idx,]
# df.test <- df[-idx,]


#ccSVM Demo
# setwd('~/R_work')
# X <- read.csv('X.csv',header=FALSE)
# X = t(X)
# y <- read.csv('y.csv',header=FALSE)
# L <- read.csv('L.csv',header=FALSE)
# L <- as.matrix(L)

#TO choose training and test dataset
# n <- dim(X)[1]
# m <- dim(X)[2]

# idxy <- round(runif(round(.6*nrow(y)), 1, nrow(y)))
# idxL <- round(runif(round(.6*nrow(L)), 1, nrow(L)))
# idx <- 1:149
# idx <- 1:10

# CVO = cvpartition(m,'k',5)
# test = find(CVO.test(1))
# train = find(CVO.training(1))

# kfold <- 2

# y = factor(y[,1])

# Predict.label 
# dec
# accuracy
# ccauc
# w

}