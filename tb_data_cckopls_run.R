#processing TB data

setwd('~/')
df.tb <- read.csv('TBdata_better_edited.csv')
df.tb <- na.omit(df.tb)
A.tb <- which(grepl('Active',df.tb[,1]))
a.tb <- which(grepl('active',df.tb[,1]))
c.tb <- which(grepl('control',df.tb[,1]))
df.tb <- df.tb[sort(c(A.tb,a.tb,c.tb)),]

y.tb <- df.tb[1]
y.tb <- as.matrix(y.tb)
g.tb <- df.tb[2] #confounders
df.tb <- df.tb[,-c(1,2)]

X.tb <- as.matrix(df.tb)

y.tb[which(grepl('control',y.tb))] <- 0
y.tb[which(grepl('active',y.tb))] <- 1
y.tb[which(grepl('Active',y.tb))] <- 1
y.tb <- as.numeric(y.tb)
y.tb <- as.matrix(y.tb)
y.tb <- factor(y.tb[,1])
y.tb <- sample(y.tb)

g.tb <- as.matrix(g.tb)
g.tb[which(grepl('Male',g.tb))] <- 'Male'
g.tb[which(grepl('Female',g.tb))] <- 'Female'
g.tb <- as.factor(g.tb)

L.tb <- matrix(0,nrow(X.tb),nrow(X.tb))
#make the L matrix to run data
for (i in 1:nrow(L.tb)){
  for (j in 1:ncol(L.tb)){
    if (i == j){
      L.tb[i,j] <- 1
    } else if (g.tb[i] == g.tb[j]){
      L.tb[i,j] <- 1
    } else{
      L.tb[i,j] <- 0
    }
  }
}

cckoplsauctb <- data.frame(ccKOPLS=0)
cckopls.scorestb <- list() #cckopls scores
cckopls.roctb <- list()    #roc curves
cckopls.predicttb <- list()

koplsauctb <- data.frame(KOPLS=0)
kopls.scorestb <- list()   #kopls scores
kopls.roctb <- list()
kopls.predicttb <- list()

ccSVMauctb <- data.frame(ccSVM=0)
ccSVM.scorestb <- list()   #ccSVM scores 
ccSVM.roctb <- list()
ccSVM.predicttb <- list()

SVM.scorestb <- list()     #SVM scores
SVMauctb <- data.frame(SVM=0)
SVM.roctb <- list()
SVM.predicttb <- list()

ccnox0auctb <- data.frame(ccnox0=0)
ccnox0.scorestb <- list()
ccnox0.roctb <- list()
ccnox0.predicttb <- list()

nox0.scorestb <- list()
nox0auctb <- data.frame(nox0=0)
nox0.roctb <- list()
nox0.predicttb <- list()

#cckopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter = 0
for (i in 1:50) {
  method <- 'cckopls'
  kfold <- 5
  cckoplsauctb[1,i] <- 1
  cckopls.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,method=method)
  for (j in 1:ncol(cckopls.predicttb[[1]])){
    cckoplsauctb[[j,i]] <- cckopls.predicttb[[1]][1,j] 
  }
  cckopls.scorestb[[i]] <- cckopls.predicttb[[2]]
  cckopls.roctb[[i]] <- cckopls.predicttb[[4]]
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
  koplsauctb[1,i] <- 1
  kopls.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,method=method)
  for (j in 1:ncol(kopls.predicttb[[1]])){
    koplsauctb[[j,i]] <- kopls.predicttb[[1]][1,j] 
  }
  kopls.scorestb[[i]] <- kopls.predicttb[[2]]
  kopls.roctb[[i]] <- kopls.predicttb[[4]]
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
  ccSVMauctb[1,i] <- 1 
  ccSVM.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,method=method)
  for (j in 1:ncol(ccSVM.predicttb[[1]])){
    ccSVMauctb[[j,i]] <- ccSVM.predicttb[[1]][1,j] 
  }
  ccSVM.scorestb[[i]] <- ccSVM.predicttb[[2]]
  ccSVM.roctb[[i]] <- ccSVM.predicttb[[4]]
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
  SVMauctb[1,i] <- 1
  SVM.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,method=method)
  for (j in 1:ncol(SVM.predicttb[[1]])){
    SVMauctb[[j,i]] <- SVM.predicttb[[1]][1,j] 
  }
  SVM.scorestb[[i]] <- SVM.predicttb[[2]]
  SVM.roctb[[i]] <- SVM.predicttb[[4]]
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
  ccnox0auctb[1,i] <- 1
  ccnox0.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,method=method)
  for (j in 1:ncol(ccnox0.predicttb[[1]])){
    ccnox0auctb[[j,i]] <- ccnox0.predicttb[[1]][1,j] 
  }
  ccnox0.scorestb[[i]] <- ccnox0.predicttb[[2]]
  ccnox0.roctb[[i]] <- ccnox0.predicttb[[4]]
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
  nox0auctb[1,i] <- 1
  nox0.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,method=method)
  for (j in 1:ncol(nox0.predicttb[[1]])){
    nox0auctb[[j,i]] <- nox0.predicttb[[1]][1,j] 
  }
  nox0.scorestb[[i]] <- nox0.predicttb[[2]]
  nox0.roctb[[i]] <- nox0.predicttb[[4]]
  counter = counter + 1
  print("nox0 iteration = ")
  print(counter)
}

ccconftb <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0,ccnox0=0,nox0=0)
ccconftb[1:3,] <- 0
rownames(ccconftb) <- c('auc','left','right')

#Calculate CI of ccOPLS
s <- sd(as.matrix(cckoplsauctb[-1]))
m <- mean(as.matrix(cckoplsauctb[-1]))
ccconftb[1,3] <- m
n <- ncol(cckoplsauctb[-1])
error <- s/sqrt(n)
left <- m - 1.645*error
ccconftb[2,3] <- left
right <- m + 1.645*error
ccconftb[3,3] <- right

#Calculate CI of O-PLS
s <- sd(as.matrix(koplsauctb))
m <- mean(as.matrix(koplsauctb))
ccconftb[1,4] <- m
n <- ncol(koplsauctb)
error <- s/sqrt(n)
left <- m - 1.645*error
ccconftb[2,4] <- left
right <- m + 1.645*error
ccconftb[3,4] <- right

#Calculate CI of ccSVM
s <- sd(as.matrix(ccSVMauctb[-1]))
m <- mean(as.matrix(ccSVMauctb[-1]))
ccconftb[1,1] <- m
n <- ncol(ccSVMauctb[-1])
error <- s/sqrt(n)
left <- m - 1.645*error
ccconftb[2,1] <- left
right <- m + 1.645*error
ccconftb[3,1] <- right

#Calculate CI of SVM
s <- sd(as.matrix(SVMauctb))
m <- mean(as.matrix(SVMauctb))
ccconftb[1,2] <- m
n <- ncol(SVMauctb)
error <- s/sqrt(n)
left <- m - 1.645*error
ccconftb[2,2] <- left
right <- m + 1.645*error
ccconftb[3,2] <- right

#Calculate CI of ccnox0
s <- sd(as.matrix(ccnox0auctb[-1]))
m <- mean(as.matrix(ccnox0auctb[-1]))
ccconftb[1,5] <- m
n <- ncol(ccnox0auctb[-1])
error <- s/sqrt(n)
left <- m - 1.645*error
ccconftb[2,5] <- left
right <- m + 1.645*error
ccconftb[3,5] <- right

#Calculate CI of nox0
s <- sd(as.matrix(nox0auctb))
m <- mean(as.matrix(nox0auctb))
ccconftb[1,6] <- m
n <- ncol(nox0auctb)
error <- s/sqrt(n)
left <- m - 1.645*error
ccconftb[2,6] <- left
right <- m + 1.645*error
ccconftb[3,6] <- right
