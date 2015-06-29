optimize.lambda <- function(X,ytr,L,nox,LambdaRange,kfold){

LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
kcauc <- matrix(0, nrow=length(LambdaRange),ncol=2)
size <- round(nrow(X)/kfold)
test.inxs <- list()
for(i in 1:kfold){
  start <- 1 + size*(i-1)
  end <- min(nrow(X),size + size*(i-1))
  test.inxs[[i]] <- start:end
}

for (i in 1:length(LambdaRange)){
  lambda <- LambdaRange[i]
  for (j in 1:kfold){
    rescaled <- Rescaling(X,L,lambda)
    X.new <- rescaled[[1]]
    K.new <- rescaled[[2]]
    # n.list <- rescaled[[3]]
    test <- test.inxs[[j]]
    #modelCV <- koplsCV(K.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
    modelOrg <- koplsModel(K.new[-test,-test],ytr[-test,],1,nox,'mc','mc')
    modelOrgPred<-koplsPredict(K.new[test,-test],K.new[test,test],K.new[-test,-test],modelOrg,rescaleY=TRUE)
    kcauc[i,j] <- auc(roc(modelOrgPred$Yhat[,2],y[test]))
  }
}

a <- max(rowMeans(kcauc))
b <- which(rowMeans(kcauc) == a)

lambda <- LambdaRange[b[1]]

return(lambda)
   
}