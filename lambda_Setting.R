lambda.setting <- function(X,y,L,LambdaRange,c,kfold){
  
library(kernlab)

kcauc <- matrix(0,nrow=length(LambdaRange),ncol=kfold)

size <- round(nrow(X)/kfold)
test.inxs <- list()
for(i in 1:kfold){
  start <- 1 + size*(i-1)
  end <- min(nrow(X),size + size*(i-1))
  test.inxs[[i]] <- start:end
}


for (i in 1:length(LambdaRange)){
  lam <- LambdaRange[i]
  for (j in 1:kfold){
    test <- test.inxs[[j]]
    rescaled <- Rescaling(X,L,lam)
    X.new <- rescaled[[1]]
    K.new <- rescaled[[2]]
    l <- rescaled[[3]]
    ok <- F
    while(ok == F) {
      tryCatch({
        ksvm.obj <- ksvm(K.new[-test,-test],y[-test],C=c,kernel='matrix',prob.model=T,type='nu-svc')
        Ktest.new <- as.kernelMatrix(crossprod(t(X.new[test,]),t(X.new[SVindex(ksvm.obj), ])))  
        predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
        labels <- y[test]
        kcauc[i,j] <- auc(roc(predictions,labels))
        ok = T
      },
      error = function(e) {
        print('retrying ksvm')
        print('lambda')
        print(e)
        ok = F
      })
    }
  }   
}

a <- max(rowMeans(kcauc))
b <- which(rowMeans(kcauc) == a)

lambda <- LambdaRange[b[1]]

return(lambda)
}