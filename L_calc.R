k.rbf <- function(x,y,sigma=10) {  #x, y samples
    return(exp((x-y)^2 / (-2*sigma)))
  }

calc.L.tb <- function(age,ethnicity,gender,sigma=10) {  #side.information
#     age <- side.information$age
     n.samples <- nrow(age)
#     ethnicity <- side.information$ethnicity
#     gender <- side.information$gender
    
    L <- matrix(nrow=n.samples,ncol=n.samples)
    
    for(i in 1:n.samples) {
      for(j in 1:n.samples) {
        b1 <- gender[i] == gender[j]
        b2 <- ethnicity[i] == ethnicity[j]
        r <- k_rbf(age[i], age[j], sigma=sigma)
        
       f <- 4*floor(r) + (r - floor(r)) + 2*b1 + b2
       L[i,j] <- f
      }
    }
    
    return(L)
  }

# side.information <- list()
# side.information$age <- c(10,20,30,40,50,60,70,80,90,100)
# side.information$ethnicity <- c(1,1,3,2,1,2,2,3,2,1)
# side.information$gender <- c(0,0,1,1,0,1,0,0,1,1)
# L <- calc.L(side.information)
# 
# L

