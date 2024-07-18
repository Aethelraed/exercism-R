square <- \(n) (n<1 | n>64) && stop("n ∈ [1,64]") || return(2**(n-1))
total <- \(n=64)2*square(n)-1