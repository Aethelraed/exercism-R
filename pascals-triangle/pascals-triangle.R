pascals_triangle <- function(n, b = c(), c = vector("list",length = n)) {
  if(n<0|!is.numeric(n))stop("input error")
  for (i in 0:n) {
    c[i] <- list(b)
    b <- c(b+c(0,b)[1:length(b)],1)    
  }
  c
}