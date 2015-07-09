library(CCPredict)

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
cc.auc <- function(X,y,L,kfold,opt.kfold,test.inxs,method,cluster.size=8){ #compute auc of method selected - currently kopls/SVM
  kcauc <- matrix(0, nrow=1,ncol=kfold)

  # Optimization of parameters if any necessary  
  n <- vector(length=length(test.inxs),mode='numeric')
  l <- vector(length=length(test.inxs),mode='numeric')
  C = vector(length=length(test.inxs),mode='numeric')
  for(i in 1:length(test.inxs)) {
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
      results <- optimize.ccSVM(train.X,train.y,train.L,CRange,LambdaRange,kfold=opt.kfold)
      l[i] <- results[1]
      C[i] <- results[2]
    } else if (method == 'svm'){
      CRange <- c(2^-8,2^-4,2^-2,2^0,2^2,2^4,2^8)
      results <- optimize.ccSVM(train.X,train.y,train.L,CRange,c(0),kfold=opt.kfold,cluster.size=8)
      C[i] <- results[2]
    }
  }
  
  m <- vector('list',length=length(test.inxs)) #models
  r <- vector('list',length=length(test.inxs)) #roc curves
  labels <- vector('list',length=length(test.inxs))
  
  cl<-makeCluster(cluster.size)
  registerDoParallel(cl)
  
  #for (i in 1:length(test.inxs)) { # left here for testing purposes
  all.results <- foreach(i = 1:length(test.inxs),.packages=c("CCPredict")) %dopar% {
    if (method == 'cckopls' || method == 'kopls' || method == 'ccnox0' || method == 'nox0') {
<<<<<<< HEAD
      results <- predict.cckopls(X,y,L,test.inxs[[i]],l[i],n[i])
=======
      print('finding auc...')
      test.L <- L[test.inxs[[i]],test.inxs[[i]]]
      rescaled <- Rescaling(X,L,l[i])
      X.new <- rescaled[[1]]
      K.new <- rescaled[[2]]
      modelOrg <- koplsModel(K.new[-test.inxs[[i]],-test.inxs[[i]]],ytr[-test.inxs[[i]],],1,n[i],'mc','mc')
      modelOrgPred<-koplsPredict(K.new[test.inxs[[i]],-test.inxs[[i]]],K.new[test.inxs[[i]],test.inxs[[i]]],K.new[-test.inxs[[i]],-test.inxs[[i]]],modelOrg,rescaleY=TRUE)
      print(y)
      print(y[test.inxs[[i]]])
      roc.curve <- roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]])
      m[[i]] <- modelOrgPred$Yhat[,2]
      #print(auc(roc.curve))
>>>>>>> 32c96ce436c19dd6966f45ad9a5253cbe4051563
    } else if (method == 'ccsvm' || method == 'svm') {
      results <- predict.ccsvm(X,y,L,test.inxs[[i]],l[i],C[i])
    } #end of ccSVM
    results  
  }
  
  stopCluster(cl)
  
  for (i in 1:length(all.results)) {
    m[[i]] <- all.results[[i]]$predicted.labels
    kcauc[1,i] <- all.results[[i]]$auc
    r[[i]] <- all.results[[i]]$roc.curve
    labels[[i]] <- all.results[[i]]$labels
  }
  return (list(kcauc,m,labels,r))
}

