
###########################################################
# This only needs to be done if you want the recent version
library(devtools)
install_github('Anderson-Lab/CCPredict')
###########################################################

library(CCPredict)

X <- read.csv('data_sets/X.csv',header=FALSE)
X <- t(X)
X = scale(X,center=T,scale=T) # Scale the X data so it has a mean of 0 and a stdev of 1. Pretty standard
y <- read.csv('data_sets/y.csv',header=FALSE)
L <- read.csv('data_sets/L.csv',header=FALSE)
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


ccnox0auc <- data.frame(ccnox0=0)
ccnox0.scores <- list()
ccnox0.roc <- list()
ccnox0.predict <- list()

nox0.scores <- list()
nox0auc <- data.frame(nox0=0)
nox0.roc <- list()
nox0.predict <- list()

# Common parameters
kfold <- 5
opt.kfold <- 2
n.iter = 50

# paul - this is the one I've gotten working. The others need to be updated
#SVM: debug
SVM.scores <- list()     #SVM scores
SVMauc <- matrix(0,nrow=kfold,ncol=n.iter) # changed this from a dataframe. not sure why it was
SVM.roc <- list()
SVM.predict <- list()

set.seed(0, kind = NULL, normal.kind = NULL)
counter = 0


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
  print(n.iter)
}


run()

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
  test.inxs = generate.test.inxs(nrow(X),kfold)
  method <- 'ccsvm'
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

#Calculate CI of ccOPLS
s <- sd(as.matrix(cckoplsauc1[-1]))
m <- mean(as.matrix(cckoplsauc1[-1]))
ccconf1[1,3] <- m
n <- ncol(cckoplsauc1[-1])
error <- s/sqrt(n)
left <- m - 1.645*error
ccconf1[2,3] <- left
right <- m + 1.645*error
ccconf1[3,3] <- right

#Calculate CI of O-PLS
s <- sd(as.matrix(koplsauc))
m <- mean(as.matrix(koplsauc))
ccconf1[1,4] <- m
n <- ncol(koplsauc)
error <- s/sqrt(n)
left <- m - 1.645*error
ccconf1[2,4] <- left
right <- m + 1.645*error
ccconf1[3,4] <- right

#Calculate CI of ccSVM
s <- sd(as.matrix(ccSVMauc[-1]))
m <- mean(as.matrix(ccSVMauc[-1]))
ccconf1[1,1] <- m
n <- ncol(ccSVMauc[-1])
error <- s/sqrt(n)
left <- m - 1.645*error
ccconf1[2,1] <- left
right <- m + 1.645*error
ccconf1[3,1] <- right

#Calculate CI of SVM
s <- sd(as.matrix(SVMauc))
m <- mean(as.matrix(SVMauc))
ccconf1[1,2] <- m
n <- ncol(SVMauc)
error <- s/sqrt(n)
left <- m - 1.645*error
ccconf1[2,2] <- left
right <- m + 1.645*error
ccconf1[3,2] <- right

#Calculate CI of ccnox0
s <- sd(as.matrix(ccnox0auc[-1]))
m <- mean(as.matrix(ccnox0auc[-1]))
ccconf1[1,5] <- m
n <- ncol(ccnox0auc[-1])
error <- s/sqrt(n)
left <- m - 1.645*error
ccconf1[2,5] <- left
right <- m + 1.645*error
ccconf1[3,5] <- right

#Calculate CI of nox0
s <- sd(as.matrix(nox0auc))
m <- mean(as.matrix(nox0auc))
ccconf1[1,6] <- m
n <- ncol(nox0auc)
error <- s/sqrt(n)
left <- m - 1.645*error
ccconf1[2,6] <- left
right <- m + 1.645*error
ccconf1[3,6] <- right
