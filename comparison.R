# 
# 
# library(kopls)
# library(kernlab)
# library(AUC)
# library(modeest)
# library(permute)
# 
# cc.auc <- function(X,y,L,kfold,method='cckopls',kfoldopt=2){
#   t <- shuffle(nrow(X))
#   
#   kcauc <- matrix(0, nrow=1,ncol=kfold)
#   ytr <- matrix(0,nrow=length(y),2)
#   ytr[y==1,1] <- 1
#   ytr[y==2,2] <- 1
#   
#   size <- round(nrow(X)/kfold)
#   test.inxs <- list()
#   for(i in 1:kfold){
#     start <- 1 + size*(i-1)
#     end <- min(nrow(X),size + size*(i-1))
#     test.inxs[[i]] <- t[start:end]
#   }
#   
#   # X is all the data
#   # y is all the labels
#   # test.inxs[[i]]
#   
#   # Optimization of parameters if any necessary  
#   n <- c()
#   l <- c()
#   C = c()
#   for (i in 1:length(test.inxs)) {
#     train.X <- X[-test.inxs[[i]],]
#     test.X <- X[test.inxs[[i]],]
#     train.L <- L[-test.inxs[[i]],-test.inxs[[i]]]
#     train.y <- y[-test.inxs[[i]]]
#     train.ytr <- ytr[-test.inxs[[i]],]
#     if (method == 'cckopls') {
#       noxRange <- 0:5
#       LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
#       results = optimize.cckopls(train.X,train.ytr,train.L,noxRange,LambdaRange,kfoldopt)
#       l[i] = results[1]
#       n[i] = results[2]
#     } else if (method == 'kopls') {
#       noxRange <- 0:5
#       results <- optimize.cckopls(train.X,train.ytr,noxRange,c(0),kfoldopt)
#       l[i] = results[1]
#       n[i] = results[2]
#     } else if (method == 'ccsvm') {
#       LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
#       CRange <- c(2^-8,2^-4,2^-2,2^0,2^2,2^4,2^8)
#       C <- parameter.setting(train.X,train.y,train.L,CRange,2)
#       lambda <- lambda.setting(train.X,train.y,train.L,LambdaRange,C,2)
#       # TODO finish optim. 
#     }
#   }
#   
#   m <- list()
#   r <- list()
#   labels <- list()
#   
#   for (i in 1:length(test.inxs)) {
#     train.X <- X[-test.inxs[[i]],]
#     test.X <- X[test.inxs[[i]],]
#     train.L <- L[-test.inxs[[i]],-test.inxs[[i]]]
#     train.y <- y[-test.inxs[[i]]]
#     train.ytr <- ytr[-test.inxs[[i]],]
#     test.y <- y[test.inxs[[i]]]
#     test.ytr <- ytr[test.inxs[[i]],]
#     
#     if (method == 'cckopls' || method == 'kopls') {
#       test.L <- L[test.inxs[[i]],test.inxs[[i]]]
#       rescaled <- Rescaling(X,L,l[i])
#       X.new <- rescaled[[1]]
#       K.new <- rescaled[[2]]
#       modelOrg <- koplsModel(K.new[-test.inxs[[i]],-test.inxs[[i]]],ytr[-test.inxs[[i]],],1,n[i],'mc','mc')
#       modelOrgPred<-koplsPredict(K.new[test.inxs[[i]],-test.inxs[[i]]],K.new[test.inxs[[i]],test.inxs[[i]]],K.new[-test.inxs[[i]],-test.inxs[[i]]],modelOrg,rescaleY=TRUE)
#       roc.curve <- roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]])
#       m[[i]] <- modelOrgPred$Yhat[,2]
#     } else if (method == 'ccsvm') {
#       test.L <- L[test.inxs[[i]],test.inxs[[i]]]
#       rescaled <- Rescaling(X,L,l[i])
#       X.new <- rescaled[[1]]
#       K.new <- rescaled[[2]]
#       
#       ok = F
#       while(ok == F) {
#         tryCatch({
#           ksvm.obj <- ksvm(K.new[-test.ixs[[i]],-test.ixs[[i]]],ytr[-test.inxs[[i]],],C=C[i],kernel='matrix',prob.model=T,type='nu-svc')
#           
#           Ktest.new1 = K.new[test.inxs[[i]],test.inxs[[i]]]
#           Ktest.new2 <- as.kernelMatrix(crossprod(t(X.new[test.ixs[[i]],]),t(X.new[SVindex(ksvm.obj), ])))  
#           # TODO: Compare the above
#           
#           # predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
#           predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
#           # labels <- y
#           roc.curve <- roc(predictions,y[test.inxs[[i]]])
#           m[[i]] <- predictions
#           ok <- T
#         },
#         error = function(e) {
#           print('retrying ksvm')
#           print('run')
#           print(e)
#           ok <- F
#         })
#       } 
#       
#       
#       #modelOrg <- koplsModel(K.new[-test.inxs[[i]],-test.inxs[[i]]],ytr[-test.inxs[[i]],],1,n[i],'mc','mc')
#       #modelOrgPred<-koplsPredict(K.new[test.inxs[[i]],-test.inxs[[i]]],K.new[test.inxs[[i]],test.inxs[[i]]],K.new[-test.inxs[[i]],-test.inxs[[i]]],modelOrg,rescaleY=TRUE)
#       #roc.curve <- roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]])
#     }
#     
#     print(auc(roc.curve))
#     kcauc[1,i] <- auc(roc.curve)
#     
#     #plot(r,main='ROC Curve for ccKOPLS')
#     labels[[i]] <- y[test.inxs[[i]]]
#     r[[i]] <- roc.curve
#   }
#   
#   return (list(kcauc,m,labels,r))
# }


#load the data
# X is all the data
# y is all the labels
# L is the side matrix
setwd('~/ccSVM/ccSVM/')
X <- read.csv('X.csv',header=FALSE)
X <- t(X)
y <- read.csv('y.csv',header=FALSE)
L <- read.csv('L.csv',header=FALSE)
y <- as.matrix(y)
y <- factor(y[,1])
L <- as.matrix(L)

cckoplsauc1 <- data.frame(ccKOPLS=0)
cckopls.scores1 <- list() #cckopls scores
cckopls.roc1 <- list()    #roc curves
cckopls.predict1 <- list()

koplsauc <- data.frame(KOPLS=0)
kopls.scores <- list()   #kopls scores
kopls.roc <- list()
kopls.predict <- list()

ccSVMauc <- data.frame(ccSVM=0)
ccSVM.scores <- list()   #ccSVM scores 
ccSVM.roc <- list()
ccSVM.predict <- list()

SVM.scores <- list()     #SVM scores
SVMauc <- data.frame(SVM=0)
SVM.roc <- list()
SVM.predict <- list()

ccnox0auc <- data.frame(ccnox0=0)
ccnox0.scores <- list()
ccnox0.roc <- list()
ccnox0.predict <- list()

nox0.scores <- list()
nox0auc <- data.frame(nox0=0)
nox0.roc <- list()
nox0.predict <- list()

#cckopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter = 0
for (i in 1:50) {
  method <- 'cckopls'
  kfold <- 5
  cckoplsauc1[1,i] <- 1
  cckopls.predict1 <- cc.auc(X,y,L,kfold,method=method)
  for (j in 1:ncol(cckopls.predict1[[1]])){
    cckoplsauc1[[j,i]] <- cckopls.predict1[[1]][1,j] 
  }
  cckopls.scores1[[i]] <- cckopls.predict1[[2]]
  cckopls.roc1[[i]] <- cckopls.predict1[[4]]
  counter = counter + 1
  print("cckopls iteration = ")
  print(counter)
}

#kopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter = 0
for (i in 1:50) {
  method <- 'kopls'
  kfold <- 5
  koplsauc[1,i] <- 1
  kopls.predict <- cc.auc(X,y,L,kfold,method=method)
  for (j in 1:ncol(kopls.predict[[1]])){
    koplsauc[[j,i]] <- kopls.predict[[1]][1,j] 
  }
  kopls.scores[[i]] <- kopls.predict[[2]]
  kopls.roc[[i]] <- kopls.predict[[4]]
  counter = counter + 1
  print("kopls iteration = ")
  print(counter)
}

#ccSVM
set.seed(0, kind = NULL, normal.kind = NULL)
counter = 0
for (i in 1:50) {
  method <- 'ccsvm'
  kfold <- 5
  ccSVMauc[1,i] <- 1 
  ccSVM.predict <- cc.auc(X,y,L,kfold,method=method)
  for (j in 1:ncol(ccSVM.predict[[1]])){
    ccSVMauc[[j,i]] <- ccSVM.predict[[1]][1,j] 
  }
  ccSVM.scores[[i]] <- ccSVM.predict[[2]]
  ccSVM.roc[[i]] <- ccSVM.predict[[4]]
  counter = counter + 1
  print("ccSVM iteration = ")
  print(counter)
}

#SVM: debug
set.seed(0, kind = NULL, normal.kind = NULL)
counter = 0
for (i in 1:50) {
  method = 'svm'
  kfold = 5
  SVMauc[1,i] <- 1
  SVM.predict <- cc.auc(X,y,L,kfold,method=method)
  for (j in 1:ncol(SVM.predict[[1]])){
    SVMauc[[j,i]] <- SVM.predict[[1]][1,j] 
  }
  SVM.scores[[i]] <- SVM.predict[[2]]
  SVM.roc[[i]] <- SVM.predict[[4]]
  counter = counter + 1
  print("SVM iteration = ")
  print(counter)
}

#ccnox0
set.seed(0, kind = NULL, normal.kind = NULL)
counter = 0
for (i in 1:50) {
  method = 'ccnox0'
  kfold = 5
  ccnox0auc[1,i] <- 1
  ccnox0.predict <- cc.auc(X,y,L,kfold,method=method)
  for (j in 1:ncol(ccnox0.predict[[1]])){
    ccnox0auc[[j,i]] <- ccnox0.predict[[1]][1,j] 
  }
  ccnox0.scores[[i]] <- ccnox0.predict[[2]]
  ccnox0.roc[[i]] <- ccnox0.predict[[4]]
  counter = counter + 1
  print("ccnox0 iteration = ")
  print(counter)
}

#nox0
set.seed(0, kind = NULL, normal.kind = NULL)
counter = 0
for (i in 1:50) {
  method = 'nox0'
  kfold = 5
  nox0auc[1,i] <- 1
  nox0.predict <- cc.auc(X,y,L,kfold,method=method)
  for (j in 1:ncol(nox0.predict[[1]])){
    nox0auc[[j,i]] <- nox0.predict[[1]][1,j] 
  }
  nox0.scores[[i]] <- nox0.predict[[2]]
  nox0.roc[[i]] <- nox0.predict[[4]]
  counter = counter + 1
  print("nox0 iteration = ")
  print(counter)
}

ccconf1 <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0,ccnox0=0,nox0=0)
ccconf1[1:3,] <- 0
rownames(ccconf1) <- c('auc','left','right')

#cckopls CI
ci <- compute.auc.ci(cckoplsauc1)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf1[2,3] <- left
ccconf1[3,3] <- right
ccconf1[1,3] <- mean_value

#kopls CI
ci <- compute.auc.ci(koplsauc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf1[2,4] <- left
ccconf1[3,4] <- right
ccconf1[1,4] <- mean_value

#ccSVM CI
ci <- compute.auc.ci(ccSVMauc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf1[2,1] <- left
ccconf1[3,1] <- right
ccconf1[1,1] <- mean_value

#SVM CI
ci <- compute.auc.ci(SVMauc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf1[2,2] <- left
ccconf1[3,2] <- right
ccconf1[1,2] <- mean_value

#ccnox0 CI
ci <- compute.auc.ci(ccnox0auc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf1[2,5] <- left
ccconf1[3,5] <- right
ccconf1[1,5] <- mean_value

#nox0 CI
ci <- compute.auc.ci(nox0auc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf1[2,6] <- left
ccconf1[3,6] <- right
ccconf1[1,6] <- mean_value

# #Calculate CI of ccOPLS
# s <- sd(as.matrix(cckoplsauc1[-1]))
# m <- mean(as.matrix(cckoplsauc1[-1]))
# ccconf1[1,3] <- m
# n <- ncol(cckoplsauc1[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf1[2,3] <- left
# right <- m + 1.645*error
# ccconf1[3,3] <- right
# 
# #Calculate CI of O-PLS
# s <- sd(as.matrix(koplsauc))
# m <- mean(as.matrix(koplsauc))
# ccconf1[1,4] <- m
# n <- ncol(koplsauc)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf1[2,4] <- left
# right <- m + 1.645*error
# ccconf1[3,4] <- right
# 
# #Calculate CI of ccSVM
# s <- sd(as.matrix(ccSVMauc[-1]))
# m <- mean(as.matrix(ccSVMauc[-1]))
# ccconf1[1,1] <- m
# n <- ncol(ccSVMauc[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf1[2,1] <- left
# right <- m + 1.645*error
# ccconf1[3,1] <- right
# 
# #Calculate CI of SVM
# s <- sd(as.matrix(SVMauc))
# m <- mean(as.matrix(SVMauc))
# ccconf1[1,2] <- m
# n <- ncol(SVMauc)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf1[2,2] <- left
# right <- m + 1.645*error
# ccconf1[3,2] <- right
# 
# #Calculate CI of ccnox0
# s <- sd(as.matrix(ccnox0auc[-1]))
# m <- mean(as.matrix(ccnox0auc[-1]))
# ccconf1[1,5] <- m
# n <- ncol(ccnox0auc[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf1[2,5] <- left
# right <- m + 1.645*error
# ccconf1[3,5] <- right
# 
# #Calculate CI of nox0
# s <- sd(as.matrix(nox0auc))
# m <- mean(as.matrix(nox0auc))
# ccconf1[1,6] <- m
# n <- ncol(nox0auc)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf1[2,6] <- left
# right <- m + 1.645*error
# ccconf1[3,6] <- right
