parameter.setting <- function(X,y,L,CRange,kfold){

library(kernlab)

# n <- dim(X)[1]
# m <- dim(X)[2]

# idx <- round(runif(round(.6*nrow(X)), 1, nrow(X)))
# X.train <- X[idx,]
# X.test <- X[-idx,]

#CVO = cvpartition(m,'k',kfold);


# choose best C

#K <- X%*%t(X)

kcauc <- matrix(0,nrow=length(CRange),ncol=kfold)

size <- round(nrow(X)/kfold)
test.inxs <- list()
for(i in 1:kfold){
  start <- 1 + size*(i-1)
  end <- min(nrow(X),size + size*(i-1))
  test.inxs[[i]] <- start:end
}
library(AUC)
for (i in 1:length(CRange)){
  c <- CRange[i]  
  for (j in 1:kfold){
    test <- test.inxs[[j]]
    K <- as.kernelMatrix(crossprod(t(X[-test,])))
#     ksvm.obj <- ksvm(K,y[-test],C=c,kernel='matrix',prob.model=T,type='nu-svc')
    ok = F
    while(ok == F) {
      tryCatch({
        ksvm.obj <- ksvm(K,y[-test],C=c,kernel='matrix',prob.model=T,type='nu-svc')
        Ktest <- as.kernelMatrix(crossprod(t(X[test,]),t(X[SVindex(ksvm.obj), ])))  
        predictions <- predict(ksvm.obj,Ktest,type='probabilities')[,2]
        labels = y[test]
        kcauc[i,j] <- auc(roc(predictions,labels))
        ok = T
      },
      error = function(e) {
        print('retrying ksvm')
        print('param')
        print(e)
        ok = F
      })
    }
#     Ktest <- as.kernelMatrix(crossprod(t(X[test,]),t(X[SVindex(ksvm.obj), ])))  
#     predictions <- predict(ksvm.obj,Ktest,type='probabilities')[,2]
#     labels = y[test]
#     kcauc[i,j] <- auc(roc(predictions,labels))
  }
}

a <- max(rowMeans(kcauc))
b <- which(rowMeans(kcauc) == a)

C <- CRange[b[1]]

return (C)
}