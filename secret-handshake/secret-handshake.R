handshake <- function(n) {
  if(n<1 | n> 31 | n==16) return(c())
  action <- c("wink","double blink","close your eyes","jump")
  binary <- function(n){(n%/%c(16,8,4,2,1))%%2}
  out <- action[1:4*binary(n)[5:2]]
  ifelse((binary(n)[1]),return(out[length(out):1]),return(out)) 
}