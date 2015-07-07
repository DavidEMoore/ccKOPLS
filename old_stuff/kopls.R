cckopls.demo <- function(X,y,L,kfold){

library(kopls)
library(kernlab)
library(AUC)
library(modeest)
library(permute)

#optimize nox
noxRange <- 1:5
kcauc <- matrix(0, nrow=1,ncol=kfold)
ytr <- matrix(0,nrow=length(y),2)
ytr[y==1,1] <- 1
ytr[y==2,2] <- 1

t <- shuffle(nrow(X))
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

n <- c()
l <- c()
m <- list()
r <- list()
labels <- list()

for (i in 1:length(test.inxs)) {
  train.X <- X[-test.inxs[[i]],]
  test.X <- X[test.inxs[[i]],]
  train.L <- L[-test.inxs[[i]],-test.inxs[[i]]]
  train.y <- y[-test.inxs[[i]]]
  test.y <- y[test.inxs[[i]]]
  train.ytr <- ytr[-test.inxs[[i]],]
  test.ytr <- ytr[test.inxs[[i]],]
  test.L <- L[test.inxs[[i]],test.inxs[[i]]]
    
  n[i] <- optimize.nox(train.X,train.ytr,noxRange,2)
  l[i] = optimize.lambda(train.X,train.ytr,train.L,n[i],LambdaRange,2)

  rescaled <- Rescaling(X,L,l[i])
  X.new <- rescaled[[1]]
  K.new <- rescaled[[2]]
  #train.idx <- round(runif(round(.7*nrow(K.new)), 1, nrow(K.new)))
  #modelCV <- koplsCV(K.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
  modelOrg <- koplsModel(K.new[-test.inxs[[i]],-test.inxs[[i]]],ytr[-test.inxs[[i]],],1,n[i],'mc','mc')
  modelOrgPred<-koplsPredict(K.new[test.inxs[[i]],-test.inxs[[i]]],K.new[test.inxs[[i]],test.inxs[[i]]],K.new[-test.inxs[[i]],-test.inxs[[i]]],modelOrg,rescaleY=TRUE)
  #kcauc <- auc(roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]]))
  roc.curve <- roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]])
  print(auc(roc.curve))
  kcauc[1,i] <- auc(roc.curve)
  #plot(r,main='ROC Curve for ccKOPLS')
  m[[i]] <- modelOrgPred$Yhat[,2]
  labels[[i]] <- y[test.inxs[[i]]]
  r[[i]] <- roc.curve
  
}

#nox <- mfv(n)[1]
#lambda <- mfv(l)[1]

# for (i in 1:length(noxRange)){
#   n <- noxRange[i]
#   for (j in 1:kfold){
#     test <- test.inxs[[j]]
# #     K <- as.kernelMatrix(crossprod(t(X[-test,])))
#     K <- as.kernelMatrix(crossprod(t(X)))
#     modelCV <- koplsCV(K,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
#     modelOrg <- koplsModel(K[-test,-test],ytr[-test,],1,n,'mc','mc')
# #     modelOrg <- koplsModel(K,ytr,1,n,'mc','mc')
#     modelOrgPred<-koplsPredict(K[test,-test],K[test,test],K[-test,-test],modelOrg,rescaleY=TRUE)
# #     modelOrgPred<-koplsPredict(K,K,K,modelOrg,rescaleY=TRUE)
#     kcauc[i,j] <- auc(roc(modelOrgPred$Yhat[,2],y[test]))
#   }
# }
# 
# a <- max(rowMeans(kcauc))
# b <- which(rowMeans(kcauc) == a)
# 
# nox <- noxRange[b[1]]

#optimize lambda
# LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
# kcauc <- matrix(0, nrow=length(LambdaRange),ncol=2)
# size <- round(nrow(X)/kfold)
# test.inxs <- list()
# for(i in 1:kfold){
#   start <- 1 + size*(i-1)
#   end <- min(nrow(X),size + size*(i-1))
#   test.inxs[[i]] <- start:end
# }
# 
# for (i in 1:length(LambdaRange)){
#   lambda <- LambdaRange[i]
#   for (j in 1:kfold){
#     rescaled <- Rescaling(X,L,lambda)
#     X.new <- rescaled[[1]]
#     K.new <- rescaled[[2]]
#     n.list <- rescaled[[3]]
#     test <- test.inxs[[j]]
#     modelCV <- koplsCV(K.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
#     modelOrg <- koplsModel(K.new[-test,-test],ytr[-test,],1,nox,'mc','mc')
#     modelOrgPred<-koplsPredict(K.new[test,-test],K.new[test,test],K.new[-test,-test],modelOrg,rescaleY=TRUE)
#     kcauc[i,j] <- auc(roc(modelOrgPred$Yhat[,2],y[test]))
#   }
# }
# 
# a <- max(rowMeans(kcauc))
# b <- which(rowMeans(kcauc) == a)
# # print('kcauc = ')
# # print(kcauc)
# # print('a = ')
# # print(a)
# 
# lambda <- LambdaRange[b[1]]
# print('lambda = ')
# print(lambda)
# koplsPlotCVDiagnostics(modelCV)
# title("Statistics from K-OPLS cross-validation of original data")
# 
# data(koplsExample)
# 
# Ktr<-koplsKernel(Xtr,NA,'g',25)
# KteTr<-koplsKernel(Xte[-1,],Xtr,'g',sigma)
# KteTe<-koplsKernel(Xte[-1,],NA,'g',sigma)

# rescaled <- Rescaling(X,L,lambda)
# X.new <- rescaled[[1]]
# K.new <- rescaled[[2]]
# n.list <- rescaled[[3]]
# #train.idx <- round(runif(round(.7*nrow(K.new)), 1, nrow(K.new)))
# #modelCV <- koplsCV(K.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
# modelOrg <- koplsModel(K.new[train.idx,train.idx],ytr[train.idx,],1,nox,'mc','mc')
# modelOrgPred<-koplsPredict(K.new[-train.idx,train.idx],K.new[-train.idx,-train.idx],K.new[train.idx,train.idx],modelOrg,rescaleY=TRUE)
# kcauc <- auc(roc(modelOrgPred$Yhat[,2],y[-train.idx]))
# r <- roc(modelOrgPred$Yhat[,2],y[-train.idx])
# plot(r,main='ROC Curve for ccKOPLS')


# rescaled <- Rescaling(X,L,lambda)
# X.reg.new <- rescaled[[1]]
# K.reg.new <- rescaled[[2]]
# n.reg.list <- rescaled[[3]]
# train.idx <- round(runif(round(.7*nrow(K.reg.new)), 1, nrow(K.reg.new)))
# #modelCV <- koplsCV(K.reg.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
# modelOrg <- koplsModel(K.reg.new[train.idx,train.idx],ytr[train.idx,],1,nox,'mc','mc')
# modelOrgPred.reg<-koplsPredict(K.reg.new[-train.idx,train.idx],K.reg.new[-train.idx,-train.idx],K.reg.new[train.idx,train.idx],modelOrg,rescaleY=TRUE)
# kcauc.reg <- auc(roc(modelOrgPred.reg$Yhat[,2],y[-train.idx]))


return (list(kcauc,m,labels,r,t))

}