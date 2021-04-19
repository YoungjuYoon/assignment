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

myvar1 <-  matrix(1:10, nrow=5, ncol=2)
answ1 <- matrix(1:10, nrow=2, ncol=5, byrow=T)
all.equal(mytranspose(myvar1),answ1)

myvar2 <-  matrix(NA, nrow=0, ncol=0)
answ2 <- matrix(NA, nrow=0, ncol=0)
all.equal(mytranspose(myvar2),answ2)

myvar3 <-  matrix(c(1,2), nrow=1, ncol=2)
answ3 <- matrix(c(1,2), nrow=2, ncol=1)
all.equal(mytranspose(myvar3),answ3)

myvar4 <-  matrix(c(1,2), nrow=2, ncol=1)
answ4 <- matrix(c(1,2), nrow=1, ncol=2)
all.equal(mytranspose(myvar4),answ4)

myvar5 <- c(1,2,NA,3)
answ5  <- matrix(c(1,2,NA,3), nrow=4, ncol=1)
all.equal(mytranspose(myvar5),answ5)

myvar6 <- c(NA)
answ6 <- matrix(c(NA))
all.equal(mytranspose(myvar6),answ6)

myvar7 <- c()
answ7 <-'null'
all.equal(mytranspose(myvar7),answ7)

d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
answ_mydata <- matrix(c("1","2","3","4","red","white","red",NA, "TRUE","TRUE","TRUE","FALSE"), nrow=3, ncol=4,byrow=T)
answ_mydata
mytranspose(mydata)
all.equal(mytranspose(mydata),answ_mydata)
