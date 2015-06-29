optimize.nox <- function(X,ytr,noxRange,kfold){
  
  kcauc <- matrix(0, nrow=length(noxRange),ncol=kfold)
  
  t <- shuffle(nrow(X))
  size <- round(nrow(X)/kfold)
  test.inxs <- list()
  for(i in 1:kfold){
    start <- 1 + size*(i-1)
    end <- min(nrow(X),size + size*(i-1))
    test.inxs[[i]] <- t[start:end]
  }
  
  for (i in 1:length(noxRange)){
    n <- noxRange[i]
    for (j in 1:kfold){
      test <- test.inxs[[j]]
      #     K <- as.kernelMatrix(crossprod(t(X[-test,])))
      K <- as.kernelMatrix(crossprod(t(X)))
      #modelCV <- koplsCV(K,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
      modelOrg <- koplsModel(K[-test,-test],ytr[-test,],1,n,preProcK='mc',preProcY='mc')
      #     modelOrg <- koplsModel(K,ytr,1,n,'mc','mc')
      modelOrgPred<-koplsPredict(K[test,-test],K[test,test],K[-test,-test],modelOrg,n,rescaleY=TRUE)
      #     modelOrgPred<-koplsPredict(K,K,K,modelOrg,rescaleY=TRUE)
      kcauc[i,j] <- auc(roc(modelOrgPred$Yhat[,2],factor(ytr[test,2])))
    }
  }
  
  a <- max(rowMeans(kcauc))
  b <- which(rowMeans(kcauc) == a)
  
  nox <- noxRange[b[1]]
  
  return(nox)
}