optimize.cckopls <- function(X,ytr,L,noxRange,LambdaRange,kfold=2){
  
  kcauc <- vector(length=length(LambdaRange)*length(noxRange))
  size <- round(nrow(X)/kfold)
  test.inxs <- list()
  for(i in 1:kfold){
    start <- 1 + size*(i-1)
    end <- min(nrow(X),size + size*(i-1))
    test.inxs[[i]] <- start:end
  }
  
  c = 1
  iz = matrix(0,nrow=length(LambdaRange)*length(noxRange),ncol=2)
  for (z in 1:length(noxRange)) {
    for (i in 1:length(LambdaRange)){
      lambda <- LambdaRange[i]
      kcauc_total = 0
      for (j in 1:kfold){
        rescaled <- Rescaling(X,L,lambda)
        X.new <- rescaled[[1]]
        K.new <- rescaled[[2]]
        # n.list <- rescaled[[3]]
        test <- test.inxs[[j]]
        #modelCV <- koplsCV(K.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
        modelOrg <- koplsModel(K.new[-test,-test],ytr[-test,],1,noxRange[z],'mc','mc')
        modelOrgPred<-koplsPredict(K.new[test,-test],K.new[test,test],K.new[-test,-test],modelOrg,rescaleY=TRUE)
        kcauc_total <- kcauc_total + auc(roc(modelOrgPred$Yhat[,2],y[test]))
      }
      kcauc_total = kcauc_total/kfold
      kcauc[c] = kcauc_total
      iz[c,1] = i
      iz[c,2] = z
      c = c + 1
    }
  }
  
  ix = which.max(kcauc)    
  i = iz[ix,1]
  z = iz[ix,2]
  lambda <- LambdaRange[i]
  nox <- noxRange[z]
  
  return(c(lambda,nox))
  
}

