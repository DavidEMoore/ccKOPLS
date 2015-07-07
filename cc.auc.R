library(kopls)
library(kernlab)
library(AUC)
library(modeest)
library(permute)

#' Optimize parameters for selected method
#'
#' Optimizes parameters for the selected `method', calculates 5-fold AUC,
#'  and yields scores, labels, ROC curve statistics, and AUC
#'
#' @param X - input data
#' @param y - labels
#' @param kfold - number of folds for kfold CV
#' @param method - one of 'cckopls', 'kopls', 'nox0', 'ccnox0', 'ccsvm', 'svm'
#'
#' @return kcauc, m, labels, r
#'
#' @export
cc.auc <- function(X,y,L,kfold,method){ #compute auc of method selected - currently kopls/SVM
  t <- shuffle(nrow(X))

  kcauc <- matrix(0, nrow=1,ncol=kfold)
  ytr <- matrix(0,nrow=length(y),2)
  #ytr[y==1,1] <- 1
  #ytr[y==2,2] <- 1
  ytr[y==0,1] <- 1
  ytr[y==1,2] <- 1
  
  size <- round(nrow(X)/kfold)
  test.inxs <- list()
  for(i in 1:kfold){
    start <- 1 + size*(i-1)
    end <- min(nrow(X),size + size*(i-1))
    test.inxs[[i]] <- t[start:end]
  }

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
      print('optimizing nox and lambda...')
      results = optimize.cckopls(train.X,train.ytr,train.L,noxRange,LambdaRange,kfold)
      print('optimized')
      l[i] = results[1]
      n[i] = results[2]
      #print(n[i])
    } else if (method == 'kopls') {
      noxRange <- 0:5
      results <- optimize.cckopls(train.X,train.ytr,train.L,noxRange,c(0),kfold)
      l[i] = results[1]
      n[i] = results[2]
    } else if (method == 'nox0'){
      noxRange <- 0:5
      results <- optimize.cckopls(train.X,train.ytr,train.L,c(0),c(0),kfold)
      l[i] = results[1]
      n[i] = results[2]
    } else if (method == 'ccnox0'){
      noxRange <- 0:5
      LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
      results <- optimize.cckopls(train.X,train.ytr,train.L,c(0),LambdaRange,kfold)
      l[i] = results[1]
      n[i] = results[2]
    } else if (method == 'ccsvm') {
      LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
      CRange <- c(2^-8,2^-4,2^-2,2^0,2^2,2^4,2^8)
      results <- optimize.ccSVM(train.X,train.y,train.L,CRange,LambdaRange,kfold)
      l[i] <- results[1]
      C[i] <- results[2]
    } else if (method == 'svm'){
      CRange <- c(2^-8,2^-4,2^-2,2^0,2^2,2^4,2^8)
      results <- optimize.ccSVM(train.X,train.y,train.L,CRange,c(0),kfold)
      C[i] <- results[2]
    }
  }
  
  m <- list() #models
  r <- list() #roc curves
  labels <- list()
  
  for (i in 1:length(test.inxs)) {
    train.X <- X[-test.inxs[[i]],]
    test.X <- X[test.inxs[[i]],]
    train.L <- L[-test.inxs[[i]],-test.inxs[[i]]]
    train.y <- y[-test.inxs[[i]]]
    train.ytr <- ytr[-test.inxs[[i]],]
    test.y <- y[test.inxs[[i]]]
    test.ytr <- ytr[test.inxs[[i]],]
    
    if (method == 'cckopls' || method == 'kopls' || method == 'ccnox0' || method == 'nox0') {
      print('finding auc...')
      test.L <- L[test.inxs[[i]],test.inxs[[i]]]
      rescaled <- Rescaling(X,L,l[i])
      X.new <- rescaled[[1]]
      K.new <- rescaled[[2]]
      modelOrg <- koplsModel(K.new[-test.inxs[[i]],-test.inxs[[i]]],ytr[-test.inxs[[i]],],1,n[i],'mc','mc')
      modelOrgPred<-koplsPredict(K.new[test.inxs[[i]],-test.inxs[[i]]],K.new[test.inxs[[i]],test.inxs[[i]]],K.new[-test.inxs[[i]],-test.inxs[[i]]],modelOrg,rescaleY=TRUE)
      roc.curve <- roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]])
      m[[i]] <- modelOrgPred$Yhat[,2]
      #print(auc(roc.curve))
    } else if (method == 'ccsvm' || method == 'svm') {
      test.L <- L[test.inxs[[i]],test.inxs[[i]]]
      rescaled <- Rescaling(X,L,l[i])
      X.new <- rescaled[[1]]
      K.new <- rescaled[[2]]
      
      ok = F
      while(ok == F) {
        tryCatch({
          ksvm.obj <- ksvm(K.new[-test.inxs[[i]],-test.inxs[[i]]],y[-test.inxs[[i]]],C=C[i],kernel='matrix',prob.model=T,type='nu-svc')
          
          #Ktest.new1 = K.new[test.inxs[[i]],test.inxs[[i]]]
          Ktest.new2 <- as.kernelMatrix(crossprod(t(X.new[test.inxs[[i]],]),t(X.new[SVindex(ksvm.obj), ])))  
          # TODO: Compare the above - looks like Ktest.new1 dim = 75,75 Ktest.new2 dim = 75, 42...using Ktest.new2
          predictions <- predict(ksvm.obj,Ktest.new2,type='probabilities')[,2]
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
    } #end of ccSVM
    
    #print(auc(roc.curve))
    kcauc[1,i] <- auc(roc.curve)
    #plot(r,main='ROC Curve for ccKOPLS')
    labels[[i]] <- y[test.inxs[[i]]]
    r[[i]] <- roc.curve
  }
  
  return (list(kcauc,m,labels,r))
}

