mytranspose <- function(x) {
  if (is.null(x)== T) {
    print('null')
  } else {
    x <- as.matrix(x)
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 0:nrow(x)) {
      for(j in 0:ncol(x)) {
        y[j,i] <- x[i,j]
      }
    }
    return(y)
  }
}

