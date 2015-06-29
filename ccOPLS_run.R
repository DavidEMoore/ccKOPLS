
setwd('~/Downloads')
#load the data
X <- read.csv('X.csv',header=FALSE)
X <- t(X)
y <- read.csv('y.csv',header=FALSE)
L <- read.csv('L.csv',header=FALSE)
y <- as.matrix(y)
y <- factor(y[,1])
L <- as.matrix(L)

#construct matrices for display
#ccauc1 <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0)  #auc
cckoplsauc <- data.frame(ccKOPLS=0)
koplsauc <- data.frame(KOPLS=0)
ccSVMauc <- data.frame(ccSVM=0)
SVMauc <- data.frame(SVM=0)
#correct.labels1 <- list() #correct labels
cckopls.scores1 <- list() #cckopls scores
ccSVM.scores1 <- list()   #ccSVM scores 
SVM.scores1 <- list()     #SVM scores
kopls.scores1 <- list()   #kopls scores
cckopls.roc <- list()    #roc curves
kopls.roc <- list()
ccSVM.roc <- list()
SVM.roc <- list()
t <- c()
cckopls.predict1 <- list()
kopls.predict1 <- list()
ccSVM.predict1 <- list()

set.seed(0, kind = NULL, normal.kind = NULL)

for (i in 11:50){  #number of k-fold CV iterations
  #cckopls
  cckopls.predict1 <- cckopls.demo(X,y,L,5)
  for(j in 1:ncol(cckopls.predict1[[1]])){
    cckoplsauc[i,j] <- cckopls.predict1[[1]][1,j]
  }
#   ccauc1[i,3] <- cckopls.predict1[[1]]
  cckopls.scores1[[i]] <- cckopls.predict1[[2]]
  cckopls.roc <- cckopls.predict1[[4]]
  t <- cckopls.predict1[[5]]
}
    
for (i in 11:50){
  #kopls
  kopls.predict1 <- kopls.demo(X,y,L,5,t)
  for(j in 1:ncol(kopls.predict1[[1]])){
    koplsauc[i,j] <- kopls.predict1[[1]][1,j]
  }
  #ccauc1[i,4] <- kopls.predict1[[1]]
  kopls.scores1[[i]] <- kopls.predict1[[2]]
  kopls.roc <- kopls.predict1[[4]]
}
 
for (i in 1:50){  
   #ccSVM
  ccSVM.predict1 <- ccSVM.demo(X,y,L,5,t)
  for(j in 1:ncol(ccSVM.predict1[[1]])){
    ccSVMauc[i,j] <- ccSVM.predict1[[1]][1,j]
  }
  #ccauc1[i,1] <- ccSVM.predict1[[1]]
  ccSVM.scores1[[i]] <- ccSVM.predict1[[3]]
  ccSVM.roc[[i]] <- ccSVM.predict1[[5]]
  
  #SVM
  for(j in 1:ncol(ccSVM.predict1[[2]])){
    SVMauc[i,j] <- ccSVM.predict1[[2]][1,j]
  }
  #ccauc1[i,2] <- ccSVM.predict1[[2]]
  SVM.scores1[[i]] <- ccSVM.predict1[[4]]
  SVM.roc[[i]] <- ccSVM.predict1[[6]]
}

ccconf1 <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0)
ccconf1[1:3,] <- 0
rownames(ccconf1) <- c('auc','left','right')

#Calculate CI of ccOPLS
s <- sd(as.matrix(cckoplsauc))
m <- mean(as.matrix(cckoplsauc))
ccconf1[1,3] <- m
n <- ncol(cckoplsauc)
error <- qt(0.975,df=n-1)*s/sqrt(n)
left <- m - error
ccconf1[2,3] <- left
right <- m + error
ccconf1[3,3] <- right

#Calculate CI of O-PLS
s <- sd(as.matrix(koplsauc))
m <- mean(as.matrix(koplsauc))
ccconf1[1,4] <- m
n <- ncol(koplsauc)
error <- qt(0.975,df=n-1)*s/sqrt(n)
left <- m - error
ccconf1[2,4] <- left
right <- m + error
ccconf1[3,4] <- right

#Calculate CI of ccSVM
s <- sd(as.matrix(ccSVMauc))
m <- mean(as.matrix(ccSVMauc))
ccconf1[1,1] <- m
n <- ncol(ccSVMauc)
error <- qt(0.975,df=n-1)*s/sqrt(n)
left <- m - error
ccconf1[2,1] <- left
right <- m + error
ccconf1[3,1] <- right

#Calculate CI of SVM
s <- sd(as.matrix(SVMauc))
m <- mean(as.matrix(SVMauc))
ccconf1[1,2] <- m
n <- ncol(SVMauc)
error <- qt(0.975,df=n-1)*s/sqrt(n)
left <- m - error
ccconf1[2,2] <- left
right <- m + error
ccconf1[3,2] <- right




# ccaccu1 <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0) #accuracy
# ccsens1 <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0) #sensitivity
# ccspec1 <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0) #specificity

#idx <- round(runif(round(.6*nrow(X)), 1, nrow(X))) #indices for kopls.demo
#correct.labels1[[i]] <- y[-idx] #y labels kopls is trying to predict

#cckopls accuracy
# a <- accuracy(cckopls.scores1[[i]],correct.labels1[[i]])
# w <- which(a$cutoffs > .5 & a$cutoffs < .6)
# index <- which(a$cutoffs > .5 & a$cutoffs < .6)[length(w)]
# ccaccu1[i,3] <- a$measure[index]
# #cckopls sensitivity
# s <- sensitivity(cckopls.scores1[[i]],correct.labels1[[i]])
# w <- which(s$cutoffs > .5 & s$cutoffs < .6)
# index <- which(s$cutoffs > .5 & s$cutoffs < .6)[length(w)]
# ccsens1[i,3] <- s$measure[index]
# #cckopls specificity
# sp <- specificity(cckopls.scores1[[i]],correct.labels1[[i]])
# w <- which(sp$cutoffs > .5 & sp$cutoffs < .6)
# index <- which(sp$cutoffs > .5 & sp$cutoffs < .6)[length(w)]
# ccspec1[i,3] <- sp$measure[index]
#kopls accuracy
# a <- accuracy(opls.scores1[[i]],correct.labels1[[i]])
# w <- which(a$cutoffs > .5 & a$cutoffs < .6)
# index <- which(a$cutoffs > .5 & a$cutoffs < .6)[length(w)]
# ccaccu1[i,4] <- a$measure[index]
# #kopls sensitivity
# s <- sensitivity(opls.scores1[[i]],correct.labels1[[i]])
# w <- which(s$cutoffs > .5 & s$cutoffs < .6)
# index <- which(s$cutoffs > .5 & s$cutoffs < .6)[length(w)]
# ccsens1[i,4] <- s$measure[index]
# #kopls specificity
# sp <- specificity(opls.scores1[[i]],correct.labels1[[i]])
# w <- which(sp$cutoffs > .5 & sp$cutoffs < .6)
# index <- which(sp$cutoffs > .5 & sp$cutoffs < .6)[length(w)]
# ccspec1[i,4] <- sp$measure[index]


