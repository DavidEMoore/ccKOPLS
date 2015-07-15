#processing TB data

setwd('~/')
df.tb <- read.csv('~/ccSVM/ccSVM/data_sets/TBdata_better_edited.csv')
df.tb <- na.omit(df.tb)
A.tb <- which(grepl('Active',df.tb[,1]))
a.tb <- which(grepl('active',df.tb[,1]))
c.tb <- which(grepl('control',df.tb[,1]))
df.tb <- df.tb[sort(c(A.tb,a.tb,c.tb)),]

y.tb <- df.tb[1]
y.tb <- as.matrix(y.tb)
g.tb <- df.tb[2] #confounders
df.tb <- df.tb[,-c(1,2,3,4)]

X.tb <- as.matrix(df.tb)

y.tb[which(grepl('control',y.tb))] <- 0
y.tb[which(grepl('active',y.tb))] <- 1
y.tb[which(grepl('Active',y.tb))] <- 1
y.tb <- as.numeric(y.tb)
y.tb <- as.matrix(y.tb)
y.tb <- factor(y.tb[,1])
set.seed(0, kind=NULL, normal.kind=NULL)
samp <- sample(c(1:nrow(X.tb)))
y.tb <- y.tb[samp]
X.tb <- X.tb[samp,]

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


# Common parameters
kfold <- 5      #computing auc
opt.kfold <- 2  #optimizing params
n.iter = 50     #iterations

cckoplsauctb <- matrix(0,nrow=kfold,ncol=n.iter)
cckopls.scorestb <- list() #cckopls scores
cckopls.roctb <- list()    #roc curves
cckopls.predicttb <- list()

koplsauctb <- matrix(0,nrow=kfold,ncol=n.iter)
kopls.scorestb <- list()   #kopls scores
kopls.roctb <- list()
kopls.predicttb <- list()

ccSVMauctb <- matrix(0,nrow=kfold,ncol=n.iter)
ccSVM.scorestb <- list()   #ccSVM scores 
ccSVM.roctb <- list()
ccSVM.predicttb <- list()

SVM.scorestb <- list()     #SVM scores
SVMauctb <- matrix(0,nrow=kfold,ncol=n.iter)
SVM.roctb <- list()
SVM.predicttb <- list()

ccnox0auctb <- matrix(0,nrow=kfold,ncol=n.iter)
ccnox0.scorestb <- list()
ccnox0.roctb <- list()
ccnox0.predicttb <- list()

nox0.scorestb <- list()
nox0auctb <- matrix(0,nrow=kfold,ncol=n.iter)
nox0.roctb <- list()
nox0.predicttb <- list()

#cckopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs <- generate.test.inxs(nrow(X.tb),kfold)
  method <- 'cckopls'
  cckopls.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(cckopls.predicttb[[1]])){
    cckoplsauctb[[j,i]] <- cckopls.predicttb[[1]][1,j] 
  }
  cckopls.scorestb[[i]] <- cckopls.predicttb[[2]]
  cckopls.roctb[[i]] <- cckopls.predicttb[[4]]
  counter <- counter + 1
  print("cckopls iteration = ")
  print(counter)
}

#kopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs <- generate.test.inxs(nrow(X.tb),kfold)
  method <- 'kopls'
  kopls.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(kopls.predicttb[[1]])){
    koplsauctb[[j,i]] <- kopls.predicttb[[1]][1,j] 
  }
  kopls.scorestb[[i]] <- kopls.predicttb[[2]]
  kopls.roctb[[i]] <- kopls.predicttb[[4]]
  counter <- counter + 1
  print("kopls iteration = ")
  print(counter)
}

#ccSVM
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs <- generate.test.inxs(nrow(X.tb),kfold)
  method <- 'ccsvm'
  ccSVM.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(ccSVM.predicttb[[1]])){
    ccSVMauctb[[j,i]] <- ccSVM.predicttb[[1]][1,j] 
  }
  ccSVM.scorestb[[i]] <- ccSVM.predicttb[[2]]
  ccSVM.roctb[[i]] <- ccSVM.predicttb[[4]]
  counter <- counter + 1
  print("ccSVM iteration = ")
  print(counter)
}

#SVM: debug
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs <- generate.test.inxs(nrow(X.tb),kfold)
  method <- 'svm'
  SVM.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(SVM.predicttb[[1]])){
    SVMauctb[[j,i]] <- SVM.predicttb[[1]][1,j] 
  }
  SVM.scorestb[[i]] <- SVM.predicttb[[2]]
  SVM.roctb[[i]] <- SVM.predicttb[[4]]
  counter <- counter + 1
  print("SVM iteration = ")
  print(counter)
}

#ccnox0
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs <- generate.test.inxs(nrow(X.tb),kfold)
  method <- 'ccnox0'
  ccnox0.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(ccnox0.predicttb[[1]])){
    ccnox0auctb[[j,i]] <- ccnox0.predicttb[[1]][1,j] 
  }
  ccnox0.scorestb[[i]] <- ccnox0.predicttb[[2]]
  ccnox0.roctb[[i]] <- ccnox0.predicttb[[4]]
  counter <- counter + 1
  print("ccnox0 iteration = ")
  print(counter)
}

#nox0
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs <- generate.test.inxs(nrow(X.tb),kfold)
  method <- 'nox0'
  nox0.predicttb <- cc.auc(X.tb,y.tb,L.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(nox0.predicttb[[1]])){
    nox0auctb[[j,i]] <- nox0.predicttb[[1]][1,j] 
  }
  nox0.scorestb[[i]] <- nox0.predicttb[[2]]
  nox0.roctb[[i]] <- nox0.predicttb[[4]]
  counter <- counter + 1
  print("nox0 iteration = ")
  print(counter)
}

#GENDER
ccconftb <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0,ccnox0=0,nox0=0)
ccconftb[1:3,] <- 0
rownames(ccconftb) <- c('auc','left','right')

#cckopls CI
ci <- compute.auc.ci(cckoplsauctb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,3] <- left
ccconf.a.tb[3,3] <- right
ccconf.a.tb[1,3] <- mean_value

#kopls CI
ci <- compute.auc.ci(koplsauctb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,4] <- left
ccconf.a.tb[3,4] <- right
ccconf.a.tb[1,4] <- mean_value

#ccSVM CI
ci <- compute.auc.ci(ccSVMauctb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,1] <- left
ccconf.a.tb[3,1] <- right
ccconf.a.tb[1,1] <- mean_value

#SVM CI
ci <- compute.auc.ci(SVMauctb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,2] <- left
ccconf.a.tb[3,2] <- right
ccconf.a.tb[1,2] <- mean_value

#ccnox0 CI
ci <- compute.auc.ci(ccnox0auctb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,5] <- left
ccconf.a.tb[3,5] <- right
ccconf.a.tb[1,5] <- mean_value

#nox0 CI
ci <- compute.auc.ci(nox0auctb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,6] <- left
ccconf.a.tb[3,6] <- right
ccconf.a.tb[1,6] <- mean_value


# #Calculate CI of ccOPLS
# s <- sd(as.matrix(cckoplsauctb[-1]))
# m <- mean(as.matrix(cckoplsauctb[-1]))
# ccconftb[1,3] <- m
# n <- ncol(cckoplsauctb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconftb[2,3] <- left
# right <- m + 1.645*error
# ccconftb[3,3] <- right
# 
# #Calculate CI of O-PLS
# s <- sd(as.matrix(koplsauctb))
# m <- mean(as.matrix(koplsauctb))
# ccconftb[1,4] <- m
# n <- ncol(koplsauctb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconftb[2,4] <- left
# right <- m + 1.645*error
# ccconftb[3,4] <- right
# 
# #Calculate CI of ccSVM
# s <- sd(as.matrix(ccSVMauctb[-1]))
# m <- mean(as.matrix(ccSVMauctb[-1]))
# ccconftb[1,1] <- m
# n <- ncol(ccSVMauctb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconftb[2,1] <- left
# right <- m + 1.645*error
# ccconftb[3,1] <- right
# 
# #Calculate CI of SVM
# s <- sd(as.matrix(SVMauctb))
# m <- mean(as.matrix(SVMauctb))
# ccconftb[1,2] <- m
# n <- ncol(SVMauctb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconftb[2,2] <- left
# right <- m + 1.645*error
# ccconftb[3,2] <- right
# 
# #Calculate CI of ccnox0
# s <- sd(as.matrix(ccnox0auctb[-1]))
# m <- mean(as.matrix(ccnox0auctb[-1]))
# ccconftb[1,5] <- m
# n <- ncol(ccnox0auctb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconftb[2,5] <- left
# right <- m + 1.645*error
# ccconftb[3,5] <- right
# 
# #Calculate CI of nox0
# s <- sd(as.matrix(nox0auctb))
# m <- mean(as.matrix(nox0auctb))
# ccconftb[1,6] <- m
# n <- ncol(nox0auctb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconftb[2,6] <- left
# right <- m + 1.645*error
# ccconftb[3,6] <- right
