# this is a stub function that takes a natural_number
# and should return the difference-of-squares as described
# in the README.md
difference_of_squares <- function(n) {
nsum <- function(n){
  n*(n+1)/2
}
g <- function(n){((0:n)**2)|>sum()}
nsum(n)**2-g(n)
}
