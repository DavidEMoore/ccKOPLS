ccSVM.demo <- function(X,y,L,kfold){

#setting up to select parameter lambda and C
LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
CRange <- c(2^-8,2^-4,2^-2,2^0,2^2,2^4,2^8)

#kfold CV
library(permute)
t <- shuffle(nrow(X))
size <- round(nrow(X)/kfold)
test.inxs <- list()
for(i in 1:kfold){
  start <- 1 + size*(i-1)
  end <- min(nrow(X),size + size*(i-1))
  test.inxs[[i]] <- t[start:end]
}

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
  
  #to do prediction on test data using standard SVM, setting lambda as 0
  cauc <- ccSVM(X,y,L,0,C) 
}

return (list(ccSVM.predict[[1]],cauc[[1]],ccSVM.predict[[2]],cauc[[2]]))
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