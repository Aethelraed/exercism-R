is_valid <- function(input) {
  x <- input|>strsplit(NULL)|>unlist()
  if(input|>grepl(pattern="[^0-9 ]")|length(x[!x==" "]|>strsplit(NULL)|>unlist())<2){return(F)}
  a <- x[!x==" "]|>as.numeric()
  a[length(a):1%%2==0] <- (2*a[length(a):1%%2==0])
  !(sum(a-9*(a%/%10))%%10)
}