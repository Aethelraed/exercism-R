number_type <- function(n) {
  if (n < 1) stop()
factors <- function(num) {
  candidates <- 1:(sqrt(num)|>ceiling())
  c(candidates[num%%candidates==0],num/candidates[num%%candidates==0])|>unique()|>sort()
}
  if (length(factors(n)) < 3) return("deficient")
  classifyer <- function(shot, target) {
    if (shot < target) return("deficient")
    if (shot == target) return("perfect")
    else return("abundant")
  }
aliquot_sum <- function(n) (factors(n) |> sum()-n)
  aliquot_sum(n) |> classifyer(n)
}
