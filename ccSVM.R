setwd('~/Downloads')
df <- read.csv('breast-cancer-wisconsin.data.txt', header=FALSE, col.names=c('Sample', 
               'Thickness', 'CellSize', 'CellShape', 'Adhesion', 'EpiCellSize', 'Nuclei', 
               'Chromatin', 'Nucleoli', 'Mitoses', 'Class'), na.strings = '?')

idx <- round(runif(round(.6*nrow(df)), 1, nrow(df)))
df.train <- df[idx,];
df.test <- df[-idx,];


#ccSVM Demo
setwd('~/R_work')
X <- read.csv('X.csv',header=FALSE)
y <- read.csv('y.csv')
L <- read.csv('L.csv')

#TO choose training and test dataset
n <- dim(X)[1]
m <- dim(X)[2]

idx <- round(runif(round(.6*nrow(X)), 1, nrow(X)))
idxy <- round(runif(round(.6*nrow(y)), 1, nrow(y)))
idxL <- round(runif(round(.6*nrow(L)), 1, nrow(L)))
X.train <- X[idx,]
X.test <- X[-idx,]

# CVO = cvpartition(m,'k',5)
# test = find(CVO.test(1))
# train = find(CVO.training(1))


#setting up to select parameter lambda and C
LambdaRange <- c(1e-8,1e-4,1e-2,1,1e+2,1e+4,1e+8)
CRange <- c(2^-8,2^-4,2^-2,2^0,2^2,2^4,2^8)
kfold <- 2

#parameter selection by kfold cross validation based only on training data
z <- parameter.setting(X.train,y[idxy,1],L[idxL,idxL],LambdaRange,CRange,kfold)
z[1] <- lambda
z[2] <- C

#to do prediction on test data using ccSVM
ccSVM.predict <- ccSVM(X,train,test,y,L,lambda,C)
# Predict.label 
# dec
# accuracy
# ccauc
# w


#to do prediction on test data using standard SVM, setting lambda as 0
cauc <- ccSVM(X,train,test,y,L,0,C)


#bye bye