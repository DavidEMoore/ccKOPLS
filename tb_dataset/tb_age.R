setwd('~/')
df.tb <- read.csv('~/ccSVM/ccSVM/data_sets/TBdata_better_edited.csv')
df.tb <- na.omit(df.tb)
A.tb <- which(grepl('Active',df.tb[,1]))
a.tb <- which(grepl('active',df.tb[,1]))
c.tb <- which(grepl('control',df.tb[,1]))
df.tb <- df.tb[sort(c(A.tb,a.tb,c.tb)),]

y.tb <- df.tb[1]
y.tb <- as.matrix(y.tb)
#confounders
a.tb <- df.tb[3]
df.tb <- df.tb[,-c(1:4)]

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

a.tb <- as.matrix(a.tb)
for (i in 1:nrow(a.tb)){
  a.tb[i] <- strsplit(a.tb[i],' ')[[1]][2]
}
a.tb <- as.matrix(as.numeric(a.tb))
L.a.tb <- matrix(0,nrow(X.tb),nrow(X.tb))
#make the age L matrix
for (i in 1:nrow(L.a.tb)){
  for (j in 1:ncol(L.a.tb)){
    L.a.tb[i,j] <- k.rbf(a.tb[i],a.tb[j])
  }
}

# Common parameters
kfold <- 5      #computing auc
opt.kfold <- 2  #optimizing params
n.iter = 50     #iterations

cckoplsauc.a.tb <- matrix(0,nrow=kfold,ncol=n.iter)
cckopls.scores.a.tb <- list() #cckopls scores
cckopls.roc.a.tb <- list()    #roc curves
cckopls.predict.a.tb <- list()

koplsauc.a.tb <- matrix(0,nrow=kfold,ncol=n.iter)
kopls.scores.a.tb <- list()   #kopls scores
kopls.roc.a.tb <- list()
kopls.predict.a.tb <- list()

ccSVMauc.a.tb <- matrix(0,nrow=kfold,ncol=n.iter)
ccSVM.scores.a.tb <- list()   #ccSVM scores 
ccSVM.roc.a.tb <- list()
ccSVM.predict.a.tb <- list()

SVM.scores.a.tb <- list()     #SVM scores
SVMauc.a.tb <- matrix(0,nrow=kfold,ncol=n.iter)
SVM.roc.a.tb <- list()
SVM.predict.a.tb <- list()

ccnox0auc.a.tb <- matrix(0,nrow=kfold,ncol=n.iter)
ccnox0.scores.a.tb <- list()
ccnox0.roc.a.tb <- list()
ccnox0.predict.a.tb <- list()

nox0.scores.a.tb <- list()
nox0auc.a.tb <- matrix(0,nrow=kfold,ncol=n.iter)
nox0.roc.a.tb <- list()
nox0.predict.a.tb <- list()

#cckopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X.tb),kfold)
  method <- 'cckopls'
  cckopls.predict.a.tb <- cc.auc(X.tb,y.tb,L.a.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(cckopls.predict.a.tb[[1]])){
    cckoplsauc.a.tb[[j,i]] <- cckopls.predict.a.tb[[1]][1,j] 
  }
  cckopls.scores.a.tb[[i]] <- cckopls.predict.a.tb[[2]]
  cckopls.roc.a.tb[[i]] <- cckopls.predict.a.tb[[4]]
  print("cckopls iteration = ")
  counter <- counter + 1
  print(counter)
}

#kopls
set.seed(19, kind = NULL, normal.kind = NULL)
counter <- 19
for (i in 19:n.iter) {
  test.inxs = generate.test.inxs(nrow(X.tb),kfold)
  method <- 'kopls'
  kopls.predict.a.tb <- cc.auc(X.tb,y.tb,L.a.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(kopls.predict.a.tb[[1]])){
    koplsauc.a.tb[[j,i]] <- kopls.predict.a.tb[[1]][1,j] 
  }
  kopls.scores.a.tb[[i]] <- kopls.predict.a.tb[[2]]
  kopls.roc.a.tb[[i]] <- kopls.predict.a.tb[[4]]
  print("kopls iteration = ")
  counter <- counter + 1
  print(counter)
}

#ccSVM
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X.tb),kfold)
  method <- 'ccsvm'
  ccSVM.predict.a.tb <- cc.auc(X.tb,y.tb,L.a.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(ccSVM.predict.a.tb[[1]])){
    ccSVMauc.a.tb[[j,i]] <- ccSVM.predict.a.tb[[1]][1,j] 
  }
  ccSVM.scores.a.tb[[i]] <- ccSVM.predict.a.tb[[2]]
  ccSVM.roc.a.tb[[i]] <- ccSVM.predict.a.tb[[4]]
  print("ccSVM iteration = ")
  counter = counter + 1
  print(counter)
}

#SVM: debug
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X.tb),kfold)
  method = 'svm'
  SVM.predict.a.tb <- cc.auc(X.tb,y.tb,L.a.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(SVM.predict.a.tb[[1]])){
    SVMauc.a.tb[[j,i]] <- SVM.predict.a.tb[[1]][1,j] 
  }
  SVM.scores.a.tb[[i]] <- SVM.predict.a.tb[[2]]
  SVM.roc.a.tb[[i]] <- SVM.predict.a.tb[[4]]
  print("SVM iteration = ")
  counter <- counter + 1
  print(counter)
}

#ccnox0
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X.tb),kfold)
  method = 'ccnox0'
  ccnox0.predict.a.tb <- cc.auc(X.tb,y.tb,L.a.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(ccnox0.predict.a.tb[[1]])){
    ccnox0auc.a.tb[[j,i]] <- ccnox0.predict.a.tb[[1]][1,j] 
  }
  ccnox0.scores.a.tb[[i]] <- ccnox0.predict.a.tb[[2]]
  ccnox0.roc.a.tb[[i]] <- ccnox0.predict.a.tb[[4]]
  print("ccnox0 iteration = ")
  counter <- counter + 1
  print(counter)
}

#nox0
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs = generate.test.inxs(nrow(X.tb),kfold)
  method = 'nox0'
  nox0.predict.a.tb <- cc.auc(X.tb,y.tb,L.a.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(nox0.predict.a.tb[[1]])){
    nox0auc.a.tb[[j,i]] <- nox0.predict.a.tb[[1]][1,j] 
  }
  nox0.scores.a.tb[[i]] <- nox0.predict.a.tb[[2]]
  nox0.roc.a.tb[[i]] <- nox0.predict.a.tb[[4]]
  print("nox0 iteration = ")
  counter = counter + 1
  print(counter)
}

#AGE
ccconf.a.tb <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0,ccnox0=0,nox0=0)
ccconf.a.tb[1:3,] <- 0
rownames(ccconf.a.tb) <- c('auc','left','right')

#cckopls CI
ci <- compute.auc.ci(cckoplsauc.a.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,3] <- left
ccconf.a.tb[3,3] <- right
ccconf.a.tb[1,3] <- mean_value

#kopls CI
ci <- compute.auc.ci(koplsauc.a.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,4] <- left
ccconf.a.tb[3,4] <- right
ccconf.a.tb[1,4] <- mean_value

#ccSVM CI
ci <- compute.auc.ci(ccSVMauc.a.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,1] <- left
ccconf.a.tb[3,1] <- right
ccconf.a.tb[1,1] <- mean_value

#SVM CI
ci <- compute.auc.ci(SVMauc.a.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,2] <- left
ccconf.a.tb[3,2] <- right
ccconf.a.tb[1,2] <- mean_value

#ccnox0 CI
ci <- compute.auc.ci(ccnox0auc.a.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,5] <- left
ccconf.a.tb[3,5] <- right
ccconf.a.tb[1,5] <- mean_value

#nox0 CI
ci <- compute.auc.ci(nox0auc.a.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.a.tb[2,6] <- left
ccconf.a.tb[3,6] <- right
ccconf.a.tb[1,6] <- mean_value

# #Calculate CI of ccOPLS
# s <- sd(as.matrix(cckoplsauc.a.tb[-1]))
# m <- mean(as.matrix(cckoplsauc.a.tb[-1]))
# ccconf.a.tb[1,3] <- m
# n <- ncol(cckoplsauc.a.tb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.a.tb[2,3] <- left
# right <- m + 1.645*error
# ccconf.a.tb[3,3] <- right
# 
# #Calculate CI of O-PLS
# s <- sd(as.matrix(koplsauc.a.tb))
# m <- mean(as.matrix(koplsauc.a.tb))
# ccconf.a.tb[1,4] <- m
# n <- ncol(koplsauc.a.tb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.a.tb[2,4] <- left
# right <- m + 1.645*error
# ccconf.a.tb[3,4] <- right
# 
# #Calculate CI of ccSVM
# s <- sd(as.matrix(ccSVMauc.a.tb[-1]))
# m <- mean(as.matrix(ccSVMauc.a.tb[-1]))
# ccconf.a.tb[1,1] <- m
# n <- ncol(ccSVMauc.a.tb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.a.tb[2,1] <- left
# right <- m + 1.645*error
# ccconf.a.tb[3,1] <- right
# 
# #Calculate CI of SVM
# s <- sd(as.matrix(SVMauc.a.tb))
# m <- mean(as.matrix(SVMauc.a.tb))
# ccconf.a.tb[1,2] <- m
# n <- ncol(SVMauc.a.tb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.a.tb[2,2] <- left
# right <- m + 1.645*error
# ccconf.a.tb[3,2] <- right
# 
# #Calculate CI of ccnox0
# s <- sd(as.matrix(ccnox0auc.a.tb[-1]))
# m <- mean(as.matrix(ccnox0auc.a.tb[-1]))
# ccconf.a.tb[1,5] <- m
# n <- ncol(ccnox0auc.a.tb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.a.tb[2,5] <- left
# right <- m + 1.645*error
# ccconf.a.tb[3,5] <- right
# 
# #Calculate CI of nox0
# s <- sd(as.matrix(nox0auc.a.tb))
# m <- mean(as.matrix(nox0auc.a.tb))
# ccconf.a.tb[1,6] <- m
# n <- ncol(nox0auc.a.tb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.a.tb[2,6] <- left
# right <- m + 1.645*error
# ccconf.a.tb[3,6] <- right