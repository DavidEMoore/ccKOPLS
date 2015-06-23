ccSVM <- function(X,y,L,lambda,c,kfold){

rescaled <- Rescaling(X,L,lambda)
X.new <- rescaled[[1]]
K.new <- rescaled[[2]]
l <- rescaled[[3]]

t <- shuffle(nrow(X))
size <- round(nrow(X)/kfold)
test.ixs <- list()
for(i in 1:kfold){
  start <- 1 + size*(i-1)
  end <- min(nrow(X),size + size*(i-1))
  test.ixs[[i]] <- t[start:end]
}


for(i in 1:length(test.ixs)){
  ok = F
  while(ok == F) {
    tryCatch({
      # ksvm.obj <- ksvm(K.new,y,C=c,kernel='matrix',prob.model=T)
      ksvm.obj <- ksvm(K.new[-test.ixs[[i]],-test.ixs],y[-test.ixs[[i]]],C=c,kernel='matrix',prob.model=T,type='nu-svc')
      # Ktest.new <- as.kernelMatrix(crossprod(t(X.new),t(X.new[SVindex(ksvm.obj), ]))) 
      Ktest.new <- as.kernelMatrix(crossprod(t(X.new[test.ixs[[i]],]),t(X.new[SVindex(ksvm.obj), ])))  
      # predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
      predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
      # labels <- y
      labels <- y[test.ixs[[i]]]
      kcauc <- auc(roc(predictions,labels))
      ok <- T
    },
    error = function(e) {
      print('retrying ksvm')
      print('run')
      print(e)
      ok <- F
    })
  } 
}

return(list(kcauc,predict(ksvm.obj,Ktest.new,type='probabilities')[,2]))


# ksvm.obj <- ksvm(K.new[-test.ixs,-test.ixs],y[-test.ixs],C=c,kernel='matrix',prob.model=T,type='nu-svc')
# # Ktest.new <- as.kernelMatrix(crossprod(t(X.new),t(X.new[SVindex(ksvm.obj), ]))) 
# Ktest.new <- as.kernelMatrix(crossprod(t(X.new[test.ixs,]),t(X.new[SVindex(ksvm.obj), ])))  
# # predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
# predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
# # labels <- y
# labels <- y[test.ixs]
# kcauc <- auc(roc(predictions,labels))

}