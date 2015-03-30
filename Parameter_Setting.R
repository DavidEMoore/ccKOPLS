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
kcauc <- matrix(0,nrow=CRange,ncol=kfold)

for (i in 1:length(CRange)){
  c <- CRange[i]  
  for (j in 1:kfold){
    test <- which(X.test[j] != 0)
    train <- which(X.train[j] != 0)
    ksvm.obj <- ksvm(Class~.,data=X[train],C=c,cross=kfold)
    labels <- predict(ksvm.obj,test,type="response")
    kcauc[i,j] <- auc(accuracy(X[test]$Class,response))
  }
}

max <- -100

for(i in 1:nrow(kcauc)){
  a <- max(mean(kcauc[i,]),max)
}

C <- CRange[a]

#Choose best lambda

for (i in 1:length(LambdaRange)){
  lam <- LambdaRange(i)
  
  rescaled <- Rescaling(X,L,lam)
  X.new <- rescaled[1]
  K.new <- rescaled[2]
  l <- rescaled[3]
  
  idx <- round(runif(round(.6*nrow(X.new)), 1, nrow(X.new)))
  X.train <- X.new[idx,]
  X.test <- X.new[-idx,]
  
  for (j in 1:kfold){
    test <- which(X.test[j] != 0)
    train <- which(X.train[j] != 0)
    print (ksvm(X.train,C=c,cross=kfold))
    ksvm.obj <- ksvm(Class~.,data=K.new,C=c,cross=kfold)
    labels <- predict(ksvm.obj,X.test,type="response")
    kcauc[i,j] <- auc(accuracy(X.test$Class,response))
    
    kauc(i,j) = svm(y,train,test,K.new,C)
  }   
}


max <- -100

for(i in 1:nrow(kcauc)){
  a <- max(mean(kcauc[i,]),max)
}

lambda = LambdaRange(a)

return (c(lambda,C))
}