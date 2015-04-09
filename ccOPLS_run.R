
setwd('~/R_work')
X <- read.csv('X.csv',header=FALSE)
X = t(X)
y <- read.csv('y.csv',header=FALSE)
L <- read.csv('L.csv',header=FALSE)
y <- as.matrix(y)
y <- factor(y[,1])
L <- as.matrix(L)

ccauc1 <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0)
correct.labels1 <- list()
cckopls.scores1 <- list()
ccSVM.scores1 <- list()
SVM.scores1 <- list()
opls.scores1 <- list()
for (i in 1:10){
  idx <- round(runif(round(.6*nrow(X)), 1, nrow(X)))
  correct.labels1[[i]] <- y[-idx]
#   ccSVM.predict1 <- ccSVM.demo(X,y,L,2,idx)
#   ccauc1[i,1] <- ccSVM.predict1[[1]]
#   ccauc1[i,2] <- ccSVM.predict1[[2]]
#   ccSVM.scores1[[i]] <- ccSVM.predict1[[3]]
#   SVM.scores1[[i]] <- ccSVM.predict1[[4]]
  kopls.predict1 <- kopls.demo(X[idx,],y[idx],L[idx,idx],2)
  ccauc1[i,3] <- kopls.predict1[[1]]
  ccauc1[i,4] <- kopls.predict1[[2]]
  cckopls.scores1[[i]] <- kopls.predict1[[3]]
  opls.scores1[[i]] <- kopls.predict1[[4]]
}