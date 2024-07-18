collatz_step_counter <- function(input) {
f <- function(num) {
  if (num < 1) {
    stop()
  }
  if (num == 1)
    return(0)
  n <- 0
  iter <- function(num) {
    if (!num %% 2) {
      return(num / 2)
    }
    3 * num + 1
  }
  while (num > 1) {
    n <- n + 1
    num <- iter(num)
  }
  n
}
Map(f,input)|>unlist()
}