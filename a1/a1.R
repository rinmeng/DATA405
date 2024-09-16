unirand <- function(n, a=171, m=30269, seed=1) {
  x <- numeric(min(m-1,n))
  x[1] <- seed
  for (i in 1:min(m-1,n)){
    y <- x[i]
    x[i+1] <- (a*y)%%m
  }
  x[2:(n+1)]/m
}
unirand(5)
