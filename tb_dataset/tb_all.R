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
g.tb <- df.tb[2]
a.tb <- df.tb[3]
e.tb <- df.tb[4]
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

g.tb <- as.matrix(g.tb)
g.tb[which(grepl('Male',g.tb))] <- 'Male'
g.tb[which(grepl('Female',g.tb))] <- 'Female'
g.tb <- as.factor(g.tb)

a.tb <- as.matrix(a.tb)
for (i in 1:nrow(a.tb)){
  a.tb[i] <- strsplit(a.tb[i],' ')[[1]][2]
}
a.tb <- as.matrix(as.numeric(a.tb))

e.tb <- as.matrix(e.tb)
for (i in 1:nrow(e.tb)){
  e.tb[i] <- strsplit(e.tb[i], ':')[[1]][2]
}
e.tb <- sub("^\\s+", "", e.tb)
e.tb <- as.factor(e.tb)

L.all.tb <- calc.L.tb(a.tb,e.tb,g.tb)

# Common parameters
kfold <- 5      #computing auc
opt.kfold <- 2  #optimizing params
n.iter = 50     #iterations

cckoplsauc.all.tb <- matrix(0,nrow=kfold,ncol=n.iter)
cckopls.scores.all.tb <- list() #cckopls scores
cckopls.roc.all.tb <- list()    #roc curves
cckopls.predict.all.tb <- list()

koplsauc.all.tb <- matrix(0,nrow=kfold,ncol=n.iter)
kopls.scores.all.tb <- list()   #kopls scores
kopls.roc.all.tb <- list()
kopls.predict.all.tb <- list()

ccSVMauc.all.tb <- matrix(0,nrow=kfold,ncol=n.iter)
ccSVM.scores.all.tb <- list()   #ccSVM scores 
ccSVM.roc.all.tb <- list()
ccSVM.predict.all.tb <- list()

SVM.scores.all.tb <- list()     #SVM scores
SVMauc.all.tb <- matrix(0,nrow=kfold,ncol=n.iter)
SVM.roc.all.tb <- list()
SVM.predict.all.tb <- list()

ccnox0auc.all.tb <- matrix(0,nrow=kfold,ncol=n.iter)
ccnox0.scores.all.tb <- list()
ccnox0.roc.all.tb <- list()
ccnox0.predict.all.tb <- list()

nox0.scores.all.tb <- list()
nox0auc.all.tb <- matrix(0,nrow=kfold,ncol=n.iter)
nox0.roc.all.tb <- list()
nox0.predict.all.tb <- list()

#cckopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter <- 0
for (i in 1:n.iter) {
  test.inxs <- generate.test.inxs(nrow(X.tb),kfold)
  method <- 'cckopls'
  cckopls.predict.all.tb <- cc.auc(X.tb,y.tb,L.all.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(cckopls.predict.all.tb[[1]])){
    cckoplsauc.all.tb[[j,i]] <- cckopls.predict.all.tb[[1]][1,j] 
  }
  cckopls.scores.all.tb[[i]] <- cckopls.predict.all.tb[[2]]
  cckopls.roc.all.tb[[i]] <- cckopls.predict.all.tb[[4]]
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
  kopls.predict.all.tb <- cc.auc(X.tb,y.tb,L.all.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(kopls.predict.all.tb[[1]])){
    koplsauc.all.tb[[j,i]] <- kopls.predict.all.tb[[1]][1,j] 
  }
  kopls.scores.all.tb[[i]] <- kopls.predict.all.tb[[2]]
  kopls.roc.all.tb[[i]] <- kopls.predict.all.tb[[4]]
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
  ccSVM.predict.all.tb <- cc.auc(X.tb,y.tb,L.all.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(ccSVM.predict.all.tb[[1]])){
    ccSVMauc.all.tb[[j,i]] <- ccSVM.predict.all.tb[[1]][1,j] 
  }
  ccSVM.scores.all.tb[[i]] <- ccSVM.predict.all.tb[[2]]
  ccSVM.roc.all.tb[[i]] <- ccSVM.predict.all.tb[[4]]
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
  SVM.predict.all.tb <- cc.auc(X.tb,y.tb,L.all.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(SVM.predict.all.tb[[1]])){
    SVMauc.all.tb[[j,i]] <- SVM.predict.all.tb[[1]][1,j] 
  }
  SVM.scores.all.tb[[i]] <- SVM.predict.all.tb[[2]]
  SVM.roc.all.tb[[i]] <- SVM.predict.all.tb[[4]]
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
  ccnox0.predict.all.tb <- cc.auc(X.tb,y.tb,L.all.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(ccnox0.predict.all.tb[[1]])){
    ccnox0auc.all.tb[[j,i]] <- ccnox0.predict.all.tb[[1]][1,j] 
  }
  ccnox0.scores.all.tb[[i]] <- ccnox0.predict.all.tb[[2]]
  ccnox0.roc.all.tb[[i]] <- ccnox0.predict.all.tb[[4]]
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
  nox0.predict.all.tb <- cc.auc(X.tb,y.tb,L.all.tb,kfold,opt.kfold,test.inxs,method=method,cluster.size=5)
  for (j in 1:ncol(nox0.predict.all.tb[[1]])){
    nox0auc.all.tb[[j,i]] <- nox0.predict.all.tb[[1]][1,j] 
  }
  nox0.scores.all.tb[[i]] <- nox0.predict.all.tb[[2]]
  nox0.roc.all.tb[[i]] <- nox0.predict.all.tb[[4]]
  counter <- counter + 1
  print("nox0 iteration = ")
  print(counter)
}


#ALL
ccconf.all.tb <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0,ccnox0=0,nox0=0)
ccconf.all.tb[1:3,] <- 0
rownames(ccconf.all.tb) <- c('auc','left','right')

#cckopls CI
ci <- compute.auc.ci(cckoplsauc.all.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.all.tb[2,3] <- left
ccconf.all.tb[3,3] <- right
ccconf.all.tb[1,3] <- mean_value

#kopls CI
ci <- compute.auc.ci(koplsauc.all.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.all.tb[2,4] <- left
ccconf.all.tb[3,4] <- right
ccconf.all.tb[1,4] <- mean_value

#ccSVM CI
ci <- compute.auc.ci(ccSVMauc.all.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.all.tb[2,1] <- left
ccconf.all.tb[3,1] <- right
ccconf.all.tb[1,1] <- mean_value

#SVM CI
ci <- compute.auc.ci(SVMauc.all.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.all.tb[2,2] <- left
ccconf.all.tb[3,2] <- right
ccconf.all.tb[1,2] <- mean_value

#ccnox0 CI
ci <- compute.auc.ci(ccnox0auc.all.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.all.tb[2,5] <- left
ccconf.all.tb[3,5] <- right
ccconf.all.tb[1,5] <- mean_value

#nox0 CI
ci <- compute.auc.ci(nox0auc.all.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.all.tb[2,6] <- left
ccconf.all.tb[3,6] <- right
ccconf.all.tb[1,6] <- mean_value


# #Calculate CI of ccOPLS
# s <- sd(as.matrix(cckoplsauc.all.tb[-1]))
# m <- mean(as.matrix(cckoplsauc.all.tb[-1]))
# ccconf.all.tb[1,3] <- m
# n <- ncol(cckoplsauc.all.tb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.all.tb[2,3] <- left
# right <- m + 1.645*error
# ccconf.all.tb[3,3] <- right
# 
# #Calculate CI of O-PLS
# s <- sd(as.matrix(koplsauc.all.tb))
# m <- mean(as.matrix(koplsauc.all.tb))
# ccconf.all.tb[1,4] <- m
# n <- ncol(koplsauc.all.tb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.all.tb[2,4] <- left
# right <- m + 1.645*error
# ccconf.all.tb[3,4] <- right
# 
# #Calculate CI of ccSVM
# s <- sd(as.matrix(ccSVMauc.all.tb[-1]))
# m <- mean(as.matrix(ccSVMauc.all.tb[-1]))
# ccconf.all.tb[1,1] <- m
# n <- ncol(ccSVMauc.all.tb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.all.tb[2,1] <- left
# right <- m + 1.645*error
# ccconf.all.tb[3,1] <- right
# 
# #Calculate CI of SVM
# s <- sd(as.matrix(SVMauc.all.tb))
# m <- mean(as.matrix(SVMauc.all.tb))
# ccconf.all.tb[1,2] <- m
# n <- ncol(SVMauc.all.tb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.all.tb[2,2] <- left
# right <- m + 1.645*error
# ccconf.all.tb[3,2] <- right
# 
# #Calculate CI of ccnox0
# s <- sd(as.matrix(ccnox0auc.all.tb[-1]))
# m <- mean(as.matrix(ccnox0auc.all.tb[-1]))
# ccconf.all.tb[1,5] <- m
# n <- ncol(ccnox0auc.all.tb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.all.tb[2,5] <- left
# right <- m + 1.645*error
# ccconf.all.tb[3,5] <- right
# 
# #Calculate CI of nox0
# s <- sd(as.matrix(nox0auc.all.tb))
# m <- mean(as.matrix(nox0auc.all.tb))
# ccconf.all.tb[1,6] <- m
# n <- ncol(nox0auc.all.tb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.all.tb[2,6] <- left
# right <- m + 1.645*error
# ccconf.all.tb[3,6] <- right