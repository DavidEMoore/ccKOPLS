#' Confounder correcting kernel O-PLS
#'
#' Does something
#'
#' @param X - input data (rows are input patterns)
#' @param y - labels
#' @param L - a thing
#' @param kfold - number of partitions for k-fold cross-validation
#'
#' @examples
#' download.example.data()
#'
#' @export
kopls.demo <- function(X,y,L,kfold){
  
  library(kopls)
  library(kernlab)
  library(AUC)
  library(modeest)
  library(permute)
  
  #optimize nox
  noxRange <- 1:5
  kcauc <- matrix(0, nrow=length(noxRange),ncol=kfold)
  ytr <- matrix(0,nrow=length(y),2)
  ytr[y==1,1] <- 1
  ytr[y==2,2] <- 1
  
  t <- shuffle(nrow(X))
  size <- round(nrow(X)/kfold)
  test.inxs <- list()
  for(i in 1:kfold){
    start <- 1 + size*(i-1)
    end <- min(nrow(X),size + size*(i-1))
    test.inxs[[i]] <- t[start:end]
  }
  
  # X is all the data
  # y is all the labels
  # test.inxs[[i]]
  
  n <- c()
  l <- c()
  
  for (i in 1:length(test.inxs)) {
    train.X <- X[-test.inxs[[i]],]
    test.X <- X[test.inxs[[i]],]
    train.L <- L[-test.inxs[[i]],-test.inxs[[i]]]
    train.y <- y[-test.inxs[[i]]]
    test.y <- y[test.inxs[[i]]]
    train.ytr <- ytr[-test.inxs[[i]],]
    test.ytr <- ytr[test.inxs[[i]],]
    test.L <- L[test.inxs[[i]],test.inxs[[i]]]
    
    n[i] <- optimize.nox(train.X,train.ytr,noxRange,2)
    
    rescaled <- Rescaling(X,L,0)
    X.new <- rescaled[[1]]
    K.new <- rescaled[[2]]
    #train.idx <- round(runif(round(.7*nrow(K.new)), 1, nrow(K.new)))
    #modelCV <- koplsCV(K.new,ytr,1,10,nrcv=7,cvType='nfold',preProcK='mc',preProcY='mc',modelType='da')
    modelOrg <- koplsModel(K.new[-test.inxs[[i]],-test.inxs[[i]]],ytr[-test.inxs[[i]],],1,n[i],'mc','mc')
    modelOrgPred<-koplsPredict(K.new[test.inxs[[i]],-test.inxs[[i]]],K.new[test.inxs[[i]],test.inxs[[i]]],K.new[-test.inxs[[i]],-test.inxs[[i]]],modelOrg,rescaleY=TRUE)
    #kcauc <- auc(roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]]))
    r <- roc(modelOrgPred$Yhat[,2],y[test.inxs[[i]]])
    print(auc(r))
    kcauc[1,i] <- auc(r)
    #plot(r,main='ROC Curve for ccKOPLS')
  }
  
}