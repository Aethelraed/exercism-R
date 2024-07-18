sieve <- function(limit) {
  if(limit<2) return(c())
  compounds <- rep(F,limit)
  primes <- c()
  for (i in 2:limit) {
    if(!compounds[i]) {primes <- append(primes,i,after=0)
    compounds[seq.int(from=i, to=limit, by=primes[1])]<- T}}
primes|>sort()}