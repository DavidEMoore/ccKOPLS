kopls.demo <- function(X,y,L,kfold,idx){

library(kopls)
library(kernlab)
library(AUC)

#optimize nox
noxRange <- 1:5
kcauc <- matrix(0, nrow=length(noxRange),ncol=2)
ytr <- matrix(0,nrow=length(y),2)
ytr[y==1,1] <- 1
ytr[y==2,2] <- 1

size <- round(nrow(X)/kfold)
test.inxs <- list()
for(i in 1:kfold){
  start <- 1 + size*(i-1)
  end <- min(nrow(X),size + size*(i-1))
  test.inxs[[i]] <- start:end
}

for (i in 1:length(noxRange)){
  n <- noxRange[i]  
  for (j in 1:kfold){
    test <- test.inxs[[j]]
#     K <- as.kernelMatrix(crossprod(t(X[-test,])))
    K <- as.kernelMatrix(crossprod(t(X)))
    modelCV <- koplsCV(K,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
    modelOrg <- koplsModel(K[-test,-test],ytr[-test,],1,n,'mc','mc')
#     modelOrg <- koplsModel(K,ytr,1,n,'mc','mc')
    modelOrgPred<-koplsPredict(K[test,-test],K[test,test],K[-test,-test],modelOrg,rescaleY=TRUE)
#     modelOrgPred<-koplsPredict(K,K,K,modelOrg,rescaleY=TRUE)
    kcauc[i,j] <- auc(roc(modelOrgPred$Yhat[,2],y[test]))
  }
}

a <- max(rowMeans(kcauc))
b <- which(rowMeans(kcauc) == a)

nox <- noxRange[b[1]]

#optimize lambda
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
  rescaled <- Rescaling(X,L,lambda)
  X.new <- rescaled[[1]]
  K.new <- rescaled[[2]]
  n.list <- rescaled[[3]]
  for (j in 1:kfold){
    test <- test.inxs[[j]]
    modelCV <- koplsCV(K.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
    modelOrg <- koplsModel(K.new[-test,-test],ytr[-test,],1,nox,'mc','mc')
    modelOrgPred<-koplsPredict(K.new[test,-test],K.new[test,test],K.new[-test,-test],modelOrg,rescaleY=TRUE)
    kcauc[i,j] <- auc(roc(modelOrgPred$Yhat[,2],y[test]))
  }
}

a <- max(rowMeans(kcauc))
b <- which(rowMeans(kcauc) == a)

lambda <- LambdaRange[b[1]]

# koplsPlotCVDiagnostics(modelCV)
# title("Statistics from K-OPLS cross-validation of original data")
# 
# data(koplsExample)
# 
# Ktr<-koplsKernel(Xtr,NA,'g',25)
# KteTr<-koplsKernel(Xte[-1,],Xtr,'g',sigma)
# KteTe<-koplsKernel(Xte[-1,],NA,'g',sigma)

rescaled <- Rescaling(X,L,lambda)
X.new <- rescaled[[1]]
K.new <- rescaled[[2]]
n.list <- rescaled[[3]]
train.idx <- round(runif(round(.7*nrow(K.new)), 1, nrow(K.new)))
#modelCV <- koplsCV(K.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
modelOrg <- koplsModel(K.new[train.idx,train.idx],ytr[train.idx,],1,nox,'mc','mc')
modelOrgPred<-koplsPredict(K.new[-train.idx,train.idx],K.new[-train.idx,-train.idx],K.new[train.idx,train.idx],modelOrg,rescaleY=TRUE)
kcauc <- auc(roc(modelOrgPred$Yhat[,2],y[-train.idx]))

rescaled <- Rescaling(X,L,0)
X.reg.new <- rescaled[[1]]
K.reg.new <- rescaled[[2]]
n.reg.list <- rescaled[[3]]
train.idx <- round(runif(round(.7*nrow(K.reg.new)), 1, nrow(K.reg.new)))
#modelCV <- koplsCV(K.reg.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
modelOrg <- koplsModel(K.reg.new[train.idx,train.idx],ytr[train.idx,],1,nox,'mc','mc')
modelOrgPred.reg<-koplsPredict(K.reg.new[-train.idx,train.idx],K.reg.new[-train.idx,-train.idx],K.reg.new[train.idx,train.idx],modelOrg,rescaleY=TRUE)
kcauc.reg <- auc(roc(modelOrgPred.reg$Yhat[,2],y[-train.idx]))


return (list(kcauc,kcauc.reg,modelOrgPred$Yhat[,2],modelOrgPred.reg$Yhat[,2]))

}