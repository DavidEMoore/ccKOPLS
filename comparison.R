
###########################################################
# This only needs to be done if you want the recent version
library(devtools)
#dependencies - git2r, rversions, xml2, curl
#apt-get install libs for git2r, xml2, curl
install_github('Anderson-Lab/CCPredict')
###########################################################

library(CCPredict)

X <- read.csv('~/dev/CCPredictStudy/data_sets/X.csv',header=FALSE)
X <- t(X)

#not for microarray data set
#X = scale(X,center=T,scale=T) # Scale the X data so it has a mean of 0 and a stdev of 1. Pretty standard

y <- read.csv('~/dev/CCPredictStudy/data_sets/y.csv',header=FALSE)
L <- read.csv('~/dev/CCPredictStudy/data_sets/L.csv',header=FALSE)
y <- as.matrix(y)
y <- factor(y)
L <- as.matrix(L)

# Common parameters
kfold <- 5      #computing auc
opt.kfold <- 2  #optimizing params
n.iter = 50     #iterations

cckoplsauc <- matrix(0,nrow=kfold,ncol=n.iter)
cckopls.scores <- list() #scores
cckopls.roc <- list()    #roc curves
cckopls.predict <- list() #labels

koplsauc <- matrix(0,nrow=kfold,ncol=n.iter)
kopls.scores <- list()
kopls.roc <- list()
kopls.predict <- list()

ccSVMauc <- matrix(0,nrow=kfold,ncol=n.iter)
ccSVM.scores <- list()
ccSVM.roc <- list()
ccSVM.predict <- list()

ccnox0auc <- matrix(0,nrow=kfold,ncol=n.iter)
ccnox0.scores <- list()
ccnox0.roc <- list()
ccnox0.predict <- list()

nox0.scores <- list()
nox0auc <- matrix(0,nrow=kfold,ncol=n.iter)
nox0.roc <- list()
nox0.predict <- list()

SVM.scores <- list()
SVMauc <- matrix(0,nrow=kfold,ncol=n.iter)
SVM.roc <- list()
SVM.predict <- list()

#cckopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X),kfold)
  method <- 'cckopls'
  cckopls.predict.test <- cc.auc(X,y,L,kfold,opt.kfold,test.inxs,method=method,cluster.size=4)
  for (j in 1:ncol(cckopls.predict.test[[1]])){
    cckoplsauc.test[[j,i]] <- cckopls.predict.test[[1]][1,j] 
  }
  cckopls.scores.test[[i]] <- cckopls.predict.test[[2]]
  cckopls.roc.test[[i]] <- cckopls.predict.test[[4]]
  print("cckopls iteration = ")
  counter <- counter + 1
  print(counter)
}


#kopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X),kfold)
  method <- 'kopls'
  kopls.predict.test <- cc.auc(X,y,L,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(kopls.predict.test[[1]])){
    koplsauc.test[[j,i]] <- kopls.predict.test[[1]][1,j] 
  }
  kopls.scores.test[[i]] <- kopls.predict.test[[2]]
  kopls.roc.test[[i]] <- kopls.predict.test[[4]]
  print("kopls iteration = ")
  counter <- counter + 1
  print(counter)
}

#ccSVM
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:50) {
  test.inxs = generate.test.inxs(nrow(X),kfold)
  method <- 'ccsvm'
  ccSVM.predict <- cc.auc(X,y,L,kfold,opt.kfold,test.inxs,method=method,cluster.size=8)
  for (j in 1:ncol(ccSVM.predict[[1]])){
    ccSVMauc[[j,i]] <- ccSVM.predict[[1]][1,j] 
  }
  ccSVM.scores[[i]] <- ccSVM.predict[[2]]
  ccSVM.roc[[i]] <- ccSVM.predict[[4]]
  print("ccSVM iteration = ")
  counter <- counter + 1
  print(counter)
}

#SVM
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X),kfold)
  method = 'svm'
  SVM.predict <- cc.auc(X,y,L,kfold,opt.kfold,test.inxs,method=method,cluster.size=8)
  for (j in 1:ncol(SVM.predict[[1]])){
    SVMauc[[j,i]] <- SVM.predict[[1]][1,j] 
  }
  SVM.scores[[i]] <- SVM.predict[[2]]
  SVM.roc[[i]] <- SVM.predict[[4]]
  print("SVM iteration = ")
  counter <- counter + 1
  print(counter)
}

#ccnox0
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X),kfold)
  method = 'ccnox0'
  ccnox0.predict <- cc.auc(X,y,L,kfold,opt.kfold,test.inxs,method=method,cluster.size=8)
  for (j in 1:ncol(ccnox0.predict[[1]])){
    ccnox0auc[[j,i]] <- ccnox0.predict[[1]][1,j] 
  }
  ccnox0.scores[[i]] <- ccnox0.predict[[2]]
  ccnox0.roc[[i]] <- ccnox0.predict[[4]]
  print("ccnox0 iteration = ")
  counter <- counter + 1
  print(counter)
}

#nox0
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X),kfold)
  method = 'nox0'
  nox0.predict <- cc.auc(X,y,L,kfold,opt.kfold,test.inxs,method=method,cluster.size=8)
  for (j in 1:ncol(nox0.predict[[1]])){
    nox0auc[[j,i]] <- nox0.predict[[1]][1,j] 
  }
  nox0.scores[[i]] <- nox0.predict[[2]]
  nox0.roc[[i]] <- nox0.predict[[4]]
  print("nox0 iteration = ")
  counter <- counter + 1
  print(counter)
}

ccconf <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0,ccnox0=0,nox0=0)
ccconf[1:3,] <- 0
rownames(ccconf) <- c('auc','left','right')

#Calculate CI of ccOPLS
ci <- compute.auc.ci(cckoplsauc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf[2,3] <- left
ccconf[3,3] <- right
ccconf[1,3] <- mean_value

#Calculate CI of O-PLS
ci <- compute.auc.ci(koplsauc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf[2,4] <- left
ccconf[3,4] <- right
ccconf[1,4] <- mean_value

#Calculate CI of ccSVM
ci <- compute.auc.ci(ccSVMauc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf[2,1] <- left
ccconf[3,1] <- right
ccconf[1,1] <- mean_value

#Calculate CI of SVM
ci <- compute.auc.ci(SVMauc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf[2,2] <- left
ccconf[3,2] <- right
ccconf[1,2] <- mean_value

#Calculate CI of ccnox0
ci <- compute.auc.ci(ccnox0auc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf[2,5] <- left
ccconf[3,5] <- right
ccconf[1,5] <- mean_value

#Calculate CI of nox0
ci <- compute.auc.ci(nox0auc)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf[2,6] <- left
ccconf[3,6] <- right
ccconf[1,6] <- mean_value
