

library(kopls)
library(kernlab)
library(AUC)
library(modeest)
library(permute)

cc.auc <- function(X,y,L,kfold,method='cckopls',kfoldopt=2){
  t <- shuffle(nrow(X))
  
  kcauc <- matrix(0, nrow=1,ncol=kfold)
  ytr <- matrix(0,nrow=length(y),2)
  ytr[y==1,1] <- 1
  ytr[y==2,2] <- 1
  
  size <- round(nrow(X)/kfold)
  test.inxs <- list()
  for(i in 1:kfold){
    start <- 1 + size*(i-1)
    end <- min(nrow(X),size + size*(i-1))
    test.inxs[[i]] <- t[start:end]
  }
  
  # X is all the data
  # y is all the labels
  # test.inxs[[i]]
  
  # Optimization of parameters if any necessary  
  n <- c()
  l <- c()
  C = c()
  for (i in 1:length(test.inxs)) {
    train.X <- X[-test.inxs[[i]],]
    test.X <- X[test.inxs[[i]],]
    train.L <- L[-test.inxs[[i]],-test.inxs[[i]]]
    train.y <- y[-test.inxs[[i]]]
    train.ytr <- ytr[-test.inxs[[i]],]
    if (method == 'cckopls') {
      noxRange <- 0:5
      LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
      results = optimize.cckopls(train.X,train.ytr,train.L,noxRange,LambdaRange,kfoldopt)
      l[i] = results[1]
      n[i] = results[2]
    } else if (method == 'kopls') {
      noxRange <- 0:5
      results <- optimize.cckopls(train.X,train.ytr,noxRange,c(0),kfoldopt)
      l[i] = results[1]
      n[i] = results[2]
    } else if (method == 'ccsvm') {
      LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
      CRange <- c(2^-8,2^-4,2^-2,2^0,2^2,2^4,2^8)
      # TODO finish optim. 
    }
  }
  
  m <- list()
  r <- list()
  labels <- list()
  
  for (i in 1:length(test.inxs)) {
    train.X <- X[-test.inxs[[i]],]
    test.X <- X[test.inxs[[i]],]
    train.L <- L[-test.inxs[[i]],-test.inxs[[i]]]
    train.y <- y[-test.inxs[[i]]]
    train.ytr <- ytr[-test.inxs[[i]],]
    test.y <- y[test.inxs[[i]]]
    test.ytr <- ytr[test.inxs[[i]],]
    
    if (method == 'cckopls' || method == 'kopls') {
      test.L <- L[test.inxs[[i]],test.inxs[[i]]]
      rescaled <- Rescaling(X,L,l[i])
      X.new <- rescaled[[1]]
      K.new <- rescaled[[2]]
      modelOrg <- koplsModel(K.new[-test.inxs[[i]],-test.inxs[[i]]],ytr[-test.inxs[[i]],],1,n[i],'mc','mc')
      modelOrgPred<-koplsPredict(K.new[test.inxs[[i]],-test.inxs[[i]]],K.new[test.inxs[[i]],test.inxs[[i]]],K.new[-test.inxs[[i]],-test.inxs[[i]]],modelOrg,rescaleY=TRUE)
      roc.curve <- roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]])
      m[[i]] <- modelOrgPred$Yhat[,2]
    } else if (method == 'ccsvm') {
      test.L <- L[test.inxs[[i]],test.inxs[[i]]]
      rescaled <- Rescaling(X,L,l[i])
      X.new <- rescaled[[1]]
      K.new <- rescaled[[2]]
      
      ok = F
      while(ok == F) {
        tryCatch({
          ksvm.obj <- ksvm(K.new[-test.ixs[[i]],-test.ixs[[i]]],ytr[-test.inxs[[i]],],C=C[i],kernel='matrix',prob.model=T,type='nu-svc')
          
          Ktest.new1 = K.new[test.inxs[[i]],test.inxs[[i]]]
          Ktest.new2 <- as.kernelMatrix(crossprod(t(X.new[test.ixs[[i]],]),t(X.new[SVindex(ksvm.obj), ])))  
          # TODO: Compare the above
          
          # predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
          predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
          # labels <- y
          roc.curve <- roc(predictions,y[test.inxs[[i]]])
          m[[i]] <- predictions
          ok <- T
        },
        error = function(e) {
          print('retrying ksvm')
          print('run')
          print(e)
          ok <- F
        })
      } 
      
      
      #modelOrg <- koplsModel(K.new[-test.inxs[[i]],-test.inxs[[i]]],ytr[-test.inxs[[i]],],1,n[i],'mc','mc')
      #modelOrgPred<-koplsPredict(K.new[test.inxs[[i]],-test.inxs[[i]]],K.new[test.inxs[[i]],test.inxs[[i]]],K.new[-test.inxs[[i]],-test.inxs[[i]]],modelOrg,rescaleY=TRUE)
      #roc.curve <- roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]])
    }
    
    print(auc(roc.curve))
    kcauc[1,i] <- auc(roc.curve)
    
    #plot(r,main='ROC Curve for ccKOPLS')
    labels[[i]] <- y[test.inxs[[i]]]
    r[[i]] <- roc.curve
  }
  
  return (list(kcauc,m,labels,r))
}


#load the data
X <- read.csv('X.csv',header=FALSE)
X <- t(X)
y <- read.csv('y.csv',header=FALSE)
L <- read.csv('L.csv',header=FALSE)
y <- as.matrix(y)
y <- factor(y[,1])
L <- as.matrix(L)

set.seed(0, kind = NULL, normal.kind = NULL)
for () {
  method = 'cckopls'
  kfold = 5
  cc.auc(X,y,L,kfold,method=method)
}

set.seed(0, kind = NULL, normal.kind = NULL)
for () {
  method = 'kopls'
  kfold = 5
  cc.auc(X,y,L,kfold,method=method)
}

set.seed(0, kind = NULL, normal.kind = NULL)
for () {
  method = 'ccsvm'
  kfold = 5
  cc.auc(X,y,L,kfold,method=method)
}

set.seed(0, kind = NULL, normal.kind = NULL)
for () {
  method = 'svm'
  kfold = 5
  cc.auc(X,y,L,kfold,method=method)
}

set.seed(0, kind = NULL, normal.kind = NULL)
for () {
  method = 'koplsnox0'
  kfold = 5
  cc.auc(X,y,L,kfold,method=method)
}
