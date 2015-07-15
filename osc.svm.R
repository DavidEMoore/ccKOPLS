osc <- function(X,Y,num_OPLS_fact) {
    w_ortho = matrix(nrow=ncol(X),ncol=num_OPLS_fact)
    t_ortho = matrix(nrow=nrow(X),ncol=num_OPLS_fact)
    p_ortho = matrix(nrow=ncol(X),ncol=num_OPLS_fact)
    
    Xres = scale(X,center=TRUE,scale=FALSE)
    Yres = scale(Y,center=TRUE,scale=FALSE)
    SS_Y=sum(sum(Yres^2))
    SS_X=sum(sum(Xres^2))
    
    if (num_OPLS_fact > 0) {
      for (iter in 1:num_OPLS_fact) {
        #find PLS component
        w = t(t(Yres)%*%Xres / (t(Yres)%*%Yres)[1])
        w = w / norm(w)
        t = Xres%*%w / (t(w)%*%w)[1]
        p = t(t(t)%*%Xres / (t(t)%*%t)[1])
        
        #run OSC filter on Xres
        w_ortho[,iter] = p - (t(w)%*%p / (t(w)%*%w)[1])[1] * w
        w_ortho[,iter] = w_ortho[,iter] / norm(matrix(w_ortho[,iter]))
        t_ortho[,iter] = Xres%*%w_ortho[,iter] / (t(w_ortho[,iter])%*%w_ortho[,iter])[1]
        p_ortho[,iter] = t(t(t_ortho[,iter])%*%Xres / (t(t_ortho[,iter])%*%t_ortho[,iter])[1])
        Xres = Xres - t_ortho[,iter]%*%t(p_ortho[,iter])
      }
    }

    l <- list()
    l$Xres <- Xres
    l$Yres <- Yres
    return(l)
}

X <- read.csv('~/dev/CCPredict/inst/extdata/X.csv',header=F)
y <- read.csv('~/dev/CCPredict/inst/extdata/y.csv',header=F)
res <- osc(X,y,3)

K <- as.kernelMatrix(crossprod(t(res$Xres)))
ksvm.obj <- ksvm(K,y,C=2e-2,kernel='matrix')
preds <- predict(ksvm.obj,K,type='decision')
