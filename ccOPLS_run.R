
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
ccauc1 <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0)  #auc
correct.labels1 <- list() #correct labels
cckopls.scores1 <- list() #cckopls scores
ccSVM.scores1 <- list()   #ccSVM scores 
SVM.scores1 <- list()     #SVM scores
kopls.scores1 <- list()   #kopls scores

for (i in 1:10){  #number of k-fold CV iterations
  #cckopls
  cckopls.predict1 <- cckopls.demo(X,y,L,10)
  ccauc1[i,3] <- cckopls.predict1[[1]]
  cckopls.scores1[[i]] <- cckopls.predict1[[2]]
  
  #kopls
  kopls.predict1 <- kopls.demo(X,y,L,10)
  ccauc1[i,4] <- kopls.predict1[[1]]
  kopls.scores1[[i]] <- kopls.predict1[[2]]

  ccSVM.predict1 <- ccSVM.demo(X,y,L,2)
  ccauc1[i,1] <- ccSVM.predict1[[1]]
  ccauc1[i,2] <- ccSVM.predict1[[2]]
  ccSVM.scores1[[i]] <- ccSVM.predict1[[3]]
  SVM.scores1[[i]] <- ccSVM.predict1[[4]]  
}

ccconf1 <- data.frame(ccSVM=0,SVM=0,ccOPLS=0,OPLS=0)
ccconf1[1:3,] <- 0
rownames(ccconf1) <- c('auc','left','right')

#Calculate CI of ccOPLS
s <- sd(ccauc1$ccOPLS)
m <- mean(ccauc1$ccOPLS)
ccconf1[1,3] <- m
n <- nrow(ccauc1)
error <- qt(0.975,df=n-1)*s/sqrt(n)
left <- m - error
ccconf1[2,3] <- left
right <- m + error
ccconf1[3,3] <- right

#Calculate CI of O-PLS
s <- sd(ccauc1$OPLS)
m <- mean(ccauc1$OPLS)
ccconf1[1,4] <- m
n <- nrow(ccauc1)
error <- qt(0.975,df=n-1)*s/sqrt(n)
left <- m - error
ccconf1[2,4] <- left
right <- m + error
ccconf1[3,4] <- right




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


