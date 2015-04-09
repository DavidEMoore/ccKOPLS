ccSVM <- function(X,y,test.ixs,L,lambda,c){

rescaled <- Rescaling(X,L,lambda)
X.new <- rescaled[[1]]
K.new <- rescaled[[2]]
l <- rescaled[[3]]

# ksvm.obj <- ksvm(K.new[-test.ixs,-test.ixs],y[-test.ixs],C=c,kernel='matrix',prob.model=T,type='nu-svc')
# # Ktest.new <- as.kernelMatrix(crossprod(t(X.new),t(X.new[SVindex(ksvm.obj), ]))) 
# Ktest.new <- as.kernelMatrix(crossprod(t(X.new[test.ixs,]),t(X.new[SVindex(ksvm.obj), ])))  
# # predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
# predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
# # labels <- y
# labels <- y[test.ixs]
# kcauc <- auc(roc(predictions,labels))

ok = F
while(ok == F) {
  tryCatch({
    # ksvm.obj <- ksvm(K.new,y,C=c,kernel='matrix',prob.model=T)
    ksvm.obj <- ksvm(K.new[-test.ixs,-test.ixs],y[-test.ixs],C=c,kernel='matrix',prob.model=T,type='nu-svc')
    # Ktest.new <- as.kernelMatrix(crossprod(t(X.new),t(X.new[SVindex(ksvm.obj), ]))) 
    Ktest.new <- as.kernelMatrix(crossprod(t(X.new[test.ixs,]),t(X.new[SVindex(ksvm.obj), ])))  
    # predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
    predictions <- predict(ksvm.obj,Ktest.new,type='probabilities')[,2]
    # labels <- y
    labels <- y[test.ixs]
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

return(list(kcauc,predict(ksvm.obj,Ktest.new,type='probabilities')[,2]))
}
