
setwd('~/R_work')
X <- read.csv('X.csv',header=FALSE)
X = t(X)
y <- read.csv('y.csv',header=FALSE)
L <- read.csv('L.csv',header=FALSE)
y <- as.matrix(y)
y <- factor(y[,1])
L <- as.matrix(L)

ccauc <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0)
correct.labels <- list()
cckopls.scores <- list()
ccSVM.scores <- list()
SVM.scores <- list()
opls.scores <- list()
for (i in 1:10){
  idx <- round(runif(round(.6*nrow(X)), 1, nrow(X)))
  correct.labels[[i]] <- y[-idx]
  ccSVM.predict <- ccSVM.demo(X,y,L,2,idx)
  ccauc[i,1] <- ccSVM.predict[[1]]
  ccauc[i,2] <- ccSVM.predict[[2]]
  ccSVM.scores[[i]] <- ccSVM.predict[[3]]
  SVM.scores[[i]] <- ccSVM.predict[[4]]
  kopls.predict <- kopls.demo(X,y,L,2,idx) 
  ccauc[i,3] <- kopls.predict[[1]]
  ccauc[i,4] <- kopls.predict[[2]]
  cckopls.scores[[i]] <- kopls.predict[[3]]
  opls.scores[[i]] <- kopls.predict[[4]]
}