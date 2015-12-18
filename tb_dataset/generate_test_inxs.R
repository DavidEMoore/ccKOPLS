generate.test.inxs <- function(n,kfold) {
  t.inxs <- shuffle(n)
  size <- round(n/kfold)
  test.inxs <- list()
  for(i in 1:kfold){
    start <- 1 + size*(i-1)
    end <- min(nrow(X),size + size*(i-1))
    test.inxs[[i]] <- t.inxs[start:end]
  }
  return(test.inxs)
}

