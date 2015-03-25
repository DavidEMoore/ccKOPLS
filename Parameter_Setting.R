parameter.setting <- function(X,y,L,LambdaRange,CRange,kfold,Class){

library(kernlab)

n <- dim(X)[1]
m <- dim(X)[2]

idx <- round(runif(round(.6*nrow(X)), 1, nrow(X)))
X.train <- X[idx,]
X.test <- X[-idx,]

#CVO = cvpartition(m,'k',kfold);


# choose best C

K <- t(X)*X
kcauc <- matrix(0, nrow = length(CRange), ncol = kfold)

for (i in 1:length(CRange)){
  c <- CRange[i]
  for (j in 1:kfold){
    test <- which(X.test[j] != 0)
    train <- which(X.train[j] != 0)
    print (ksvm(X.train,C=c,cross=kfold))
    kcauc[i,j] <- ksvm(X.train,C=c,cross=kfold)
  }
}


a <- max(mean(kcauc,2))

C <- CRange[a]

#Choose best lambda

for (i in 1:length(LambdaRange)){
  lam <- LambdaRange(i)

  rescaled <- Rescaling(X,L,lam)
  X.new <- rescaled[1]
  K.new <- rescaled[2]
  l <- rescaled[3]

  for (j in 1:kfold){
    test = find(CVO.test(j))
    train = find(CVO.training(j))
    kauc(i,j) = svm(y,train,test,K.new,C)
  }
  }


a <- max(mean(kcauc,2))

C <- CRange[a]

#Choose best lambda

for (i in 1:length(LambdaRange)){
  lam <- LambdaRange(i)

  rescaled <- Rescaling(X,L,lam)
  X.new <- rescaled[1]
  K.new <- rescaled[2]
  l <- rescaled[3]

  for (j in 1:kfold){
    test = find(CVO.test(j))
    train = find(CVO.training(j))
    kauc(i,j) = svm(y,train,test,K.new,C)
  }
}


a= max(mean(kauc,2))
lambda = LambdaRange(a)

return (c(lambda,C))
}

  
