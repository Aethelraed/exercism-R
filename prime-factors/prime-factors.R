prime_factors <- function(num,d=2,out=c()) {
  while (num > 1) {
    if (num %% d == 0) {
      num <- num / d
      out <- append(out, d)
    }
    else{d <- d + 1}
  }
  out
}