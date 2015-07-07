#Uses optimized lambda to rescale K (the kernel matrix) by way of X (data set), returns rescaled X and K

#' Kernel matrix rescaling
#'
#' Uses an optimized lambda to rescale K (the kernel matrix) by way of X
#' (the data set)
#'
#' @param X - input data matrix
#' @param L - side information matrix
#' @param lambda - optimized lambda value (see Li et al.)
#'
#' @return the rescaled matrices X and K
#'
#' @export
Rescaling <- function(X,L,lambda){
  
  #instead of lambda, nox

n <- dim(X)[1]
m <- dim(X)[2]

H <- diag(n,n)-1/n*matrix(1,n,n)
L <- H%*%L%*%H/((m-1)^2)

l <- c()
print(lambda)
if (lambda > 0){
  for (i in 1:m){
    xi <- X[,i]
    l[i] <- sqrt(lambda*t(xi)%*%L%*%xi+1)
    X[,i] <- xi/l[i]
  }             
l = t(l)
} else{
  l = matrix(1,m,1)
}

X.new = X
K.new = X.new%*%t(X.new)

return (list(X.new,K.new))
}
