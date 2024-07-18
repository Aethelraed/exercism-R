fizz_buzz <- function(n) {
  x <- 1:n
  out <- x
  out[x%%3==0] <- "Fizz"
  out[x%%5==0] <- "Buzz"
  out[x%%15==0] <- "Fizz Buzz"
  as.character(out)
}