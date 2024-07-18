score <- function(x, y) {
  two_norm <- function(x) {
    sum(x ** 2) |> sqrt()
  }
  outside <- 0
  outer <- 1
  middle <- 5
  inner <- 10
  distance <- two_norm(c(x, y))
  if (distance > 10) {
    return (outside)
  }
  if (distance > 5) {
    return (outer)
  }
  if (distance > 1) {
    return (middle)
  }
  inner
}
