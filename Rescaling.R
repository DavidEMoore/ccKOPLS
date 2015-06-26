#' Rescaling
#'
#' Rescales the original data
#'
#' @param X - stuff
#' @param L - other stuff
#' @param lambda - some param
#'
#' @return scaled data
#'
#' @examples
#' download.example.data()
#' list(X.new,K.new,l) <- Rescaling(X,L,lambda)
#'
#' @export
Rescaling <- function(X,L,lambda){
  
  #instead of lambda, nox

n <- dim(X)[1]
m <- dim(X)[2]

H <- diag(n,n)-1/n*matrix(1,n,n)
L <- H%*%L%*%H/((m-1)^2)

l <- c()
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

return (list(X.new,K.new,l))
}