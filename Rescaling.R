#To do rescaling on the original data        
Rescaling <- function(X,L,lambda){

n <- dim(X)[1]
m <- dim(X)[2]

H <- diag(m,m)-1/m*matrix(1,m,m)
L <- H*L*H/((m-1)^2)

l <- c()
if (lambda > 0){
  for (i in 1:n){
    xi <- X[i,]
    l[i] <- sqrt(lambda*xi*L*t(xi)+1)
            X[i,] <- xi/l[i]
  }             
l = t(l)
}
else{
  l = ones(n,1)
}

X_new = X
K_new = t(X_new)*X_new

return (c(X_new,K_new,l))
}