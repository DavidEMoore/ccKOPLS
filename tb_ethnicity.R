setwd('~/')
df.tb <- read.csv('TBdata_better_edited.csv')
df.tb <- na.omit(df.tb)
A.tb <- which(grepl('Active',df.tb[,1]))
a.tb <- which(grepl('active',df.tb[,1]))
c.tb <- which(grepl('control',df.tb[,1]))
df.tb <- df.tb[sort(c(A.tb,a.tb,c.tb)),]

y.tb <- df.tb[1]
y.tb <- as.matrix(y.tb)
#confounders
e.tb <- df.tb[4]
df.tb <- df.tb[,-c(1:4)]

X.tb <- as.matrix(df.tb)

y.tb[which(grepl('control',y.tb))] <- 0
y.tb[which(grepl('active',y.tb))] <- 1
y.tb[which(grepl('Active',y.tb))] <- 1
y.tb <- as.numeric(y.tb)
y.tb <- as.matrix(y.tb)
y.tb <- factor(y.tb[,1])
y.tb <- sample(y.tb)

e.tb <- as.matrix(e.tb)
for (i in 1:nrow(e.tb)){
  e.tb[i] <- strsplit(e.tb[i], ':')[[1]][2]
}
e.tb <- sub("^\\s+", "", e.tb)
e.tb <- as.factor(e.tb)

L.e.tb <- matrix(0,nrow(X.tb),nrow(X.tb))
#make the ethnicity L matrix
for (i in 1:nrow(L.e.tb)){
  for (j in 1:ncol(L.e.tb)){
    if (i == j){
      L.e.tb[i,j] <- 1
    } else if (e.tb[i] == e.tb[j]){
      L.e.tb[i,j] <- 1
    } else{
      L.e.tb[i,j] <- 0
    }
  }
}

cckoplsauc.e.tb <- data.frame(ccKOPLS=0)
cckopls.scores.e.tb <- list() #cckopls scores
cckopls.roc.e.tb <- list()    #roc curves
cckopls.predict.e.tb <- list()

koplsauc.e.tb <- data.frame(KOPLS=0)
kopls.scores.e.tb <- list()   #kopls scores
kopls.roc.e.tb <- list()
kopls.predict.e.tb <- list()

ccSVMauc.e.tb <- data.frame(ccSVM=0)
ccSVM.scores.e.tb <- list()   #ccSVM scores 
ccSVM.roc.e.tb <- list()
ccSVM.predict.e.tb <- list()

SVM.scores.e.tb <- list()     #SVM scores
SVMauc.e.tb <- data.frame(SVM=0)
SVM.roc.e.tb <- list()
SVM.predict.e.tb <- list()

ccnox0auc.e.tb <- data.frame(ccnox0=0)
ccnox0.scores.e.tb <- list()
ccnox0.roc.e.tb <- list()
ccnox0.predict.e.tb <- list()

nox0.scores.e.tb <- list()
nox0auc.e.tb <- data.frame(nox0=0)
nox0.roc.e.tb <- list()
nox0.predict.e.tb <- list()

#cckopls
set.seed(0, kind = NULL, normal.kind = NULL)
counter = 0
for (i in 1:50) {
  method <- 'cckopls'
  kfold <- 5
  cckoplsauc.e.tb[1,i] <- 1
  cckopls.predict.e.tb <- cc.auc(X.tb,y.tb,L.e.tb,kfold,method=method)
  for (j in 1:ncol(cckopls.predict.e.tb[[1]])){
    cckoplsauc.e.tb[[j,i]] <- cckopls.predict.e.tb[[1]][1,j] 
  }
  cckopls.scores.e.tb[[i]] <- cckopls.predict.e.tb[[2]]
  cckopls.roc.e.tb[[i]] <- cckopls.predict.e.tb[[4]]
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
  koplsauc.e.tb[1,i] <- 1
  kopls.predict.e.tb <- cc.auc(X.tb,y.tb,L.e.tb,kfold,method=method)
  for (j in 1:ncol(kopls.predict.e.tb[[1]])){
    koplsauc.e.tb[[j,i]] <- kopls.predict.e.tb[[1]][1,j] 
  }
  kopls.scores.e.tb[[i]] <- kopls.predict.e.tb[[2]]
  kopls.roc.e.tb[[i]] <- kopls.predict.e.tb[[4]]
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
  ccSVMauc.e.tb[1,i] <- 1 
  ccSVM.predict.e.tb <- cc.auc(X.tb,y.tb,L.e.tb,kfold,method=method)
  for (j in 1:ncol(ccSVM.predict.e.tb[[1]])){
    ccSVMauc.e.tb[[j,i]] <- ccSVM.predict.e.tb[[1]][1,j] 
  }
  ccSVM.scores.e.tb[[i]] <- ccSVM.predict.e.tb[[2]]
  ccSVM.roc.e.tb[[i]] <- ccSVM.predict.e.tb[[4]]
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
  SVMauc.e.tb[1,i] <- 1
  SVM.predict.e.tb <- cc.auc(X.tb,y.tb,L.e.tb,kfold,method=method)
  for (j in 1:ncol(SVM.predict.e.tb[[1]])){
    SVMauc.e.tb[[j,i]] <- SVM.predict.e.tb[[1]][1,j] 
  }
  SVM.scores.e.tb[[i]] <- SVM.predict.e.tb[[2]]
  SVM.roc.e.tb[[i]] <- SVM.predict.e.tb[[4]]
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
  ccnox0auc.e.tb[1,i] <- 1
  ccnox0.predict.e.tb <- cc.auc(X.tb,y.tb,L.e.tb,kfold,method=method)
  for (j in 1:ncol(ccnox0.predict.e.tb[[1]])){
    ccnox0auc.e.tb[[j,i]] <- ccnox0.predict.e.tb[[1]][1,j] 
  }
  ccnox0.scores.e.tb[[i]] <- ccnox0.predict.e.tb[[2]]
  ccnox0.roc.e.tb[[i]] <- ccnox0.predict.e.tb[[4]]
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
  nox0auc.e.tb[1,i] <- 1
  nox0.predict.e.tb <- cc.auc(X.tb,y.tb,L.e.tb,kfold,method=method)
  for (j in 1:ncol(nox0.predict.e.tb[[1]])){
    nox0auc.e.tb[[j,i]] <- nox0.predict.e.tb[[1]][1,j] 
  }
  nox0.scores.e.tb[[i]] <- nox0.predict.e.tb[[2]]
  nox0.roc.e.tb[[i]] <- nox0.predict.e.tb[[4]]
  counter = counter + 1
  print("nox0 iteration = ")
  print(counter)
}

#ETHNICITY
ccconf.e.tb <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0,ccnox0=0,nox0=0)
ccconf.e.tb[1:3,] <- 0
rownames(ccconf.e.tb) <- c('auc','left','right')

#cckopls CI
ci <- compute.auc.ci(cckoplsauc.e.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.e.tb[2,3] <- left
ccconf.e.tb[3,3] <- right
ccconf.e.tb[1,3] <- mean_value

#kopls CI
ci <- compute.auc.ci(koplsauc.e.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.e.tb[2,4] <- left
ccconf.e.tb[3,4] <- right
ccconf.e.tb[1,4] <- mean_value

#ccSVM CI
ci <- compute.auc.ci(ccSVMauc.e.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.e.tb[2,1] <- left
ccconf.e.tb[3,1] <- right
ccconf.e.tb[1,1] <- mean_value

#SVM CI
ci <- compute.auc.ci(SVMauc.e.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.e.tb[2,2] <- left
ccconf.e.tb[3,2] <- right
ccconf.e.tb[1,2] <- mean_value

#ccnox0 CI
ci <- compute.auc.ci(ccnox0auc.e.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.e.tb[2,5] <- left
ccconf.e.tb[3,5] <- right
ccconf.e.tb[1,5] <- mean_value

#nox0 CI
ci <- compute.auc.ci(nox0auc.e.tb)
left <- ci[1]
right <- ci[2]
mean_value <- ci[3]
ccconf.e.tb[2,3] <- left
ccconf.e.tb[3,3] <- right
ccconf.e.tb[1,3] <- mean_value

# #Calculate CI of ccOPLS
# s <- sd(as.matrix(cckoplsauc.e.tb[-1]))
# m <- mean(as.matrix(cckoplsauc.e.tb[-1]))
# ccconf.e.tb[1,3] <- m
# n <- ncol(cckoplsauc.e.tb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.e.tb[2,3] <- left
# right <- m + 1.645*error
# ccconf.e.tb[3,3] <- right
# 
# #Calculate CI of O-PLS
# s <- sd(as.matrix(koplsauc.e.tb))
# m <- mean(as.matrix(koplsauc.e.tb))
# ccconf.e.tb[1,4] <- m
# n <- ncol(koplsauc.e.tb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.e.tb[2,4] <- left
# right <- m + 1.645*error
# ccconf.e.tb[3,4] <- right
# 
# #Calculate CI of ccSVM
# s <- sd(as.matrix(ccSVMauc.e.tb[-1]))
# m <- mean(as.matrix(ccSVMauc.e.tb[-1]))
# ccconf.e.tb[1,1] <- m
# n <- ncol(ccSVMauc.e.tb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.e.tb[2,1] <- left
# right <- m + 1.645*error
# ccconf.e.tb[3,1] <- right
# 
# #Calculate CI of SVM
# s <- sd(as.matrix(SVMauc.e.tb))
# m <- mean(as.matrix(SVMauc.e.tb))
# ccconf.e.tb[1,2] <- m
# n <- ncol(SVMauc.e.tb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.e.tb[2,2] <- left
# right <- m + 1.645*error
# ccconf.e.tb[3,2] <- right
# 
# #Calculate CI of ccnox0
# s <- sd(as.matrix(ccnox0auc.e.tb[-1]))
# m <- mean(as.matrix(ccnox0auc.e.tb[-1]))
# ccconf.e.tb[1,5] <- m
# n <- ncol(ccnox0auc.e.tb[-1])
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.e.tb[2,5] <- left
# right <- m + 1.645*error
# ccconf.e.tb[3,5] <- right
# 
# #Calculate CI of nox0
# s <- sd(as.matrix(nox0auc.e.tb))
# m <- mean(as.matrix(nox0auc.e.tb))
# ccconf.e.tb[1,6] <- m
# n <- ncol(nox0auc.e.tb)
# error <- s/sqrt(n)
# left <- m - 1.645*error
# ccconf.e.tb[2,6] <- left
# right <- m + 1.645*error
# ccconf.e.tb[3,6] <- right