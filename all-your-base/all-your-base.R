rebase <- function(from_base=2, digits, to_base=10) {
  (from_base<2 && stop("input base must be >= 2")   ||digits|>sum()!=0&&min(digits)<0 && stop("all digits must satisfy 0 <= d < input base")   ||to_base<2 && stop("output base must be >= 2")   ||digits|>sum()!=0&&max(digits)>=from_base && stop("all digits must satisfy 0 <= d < input base")) || digits|>sum()==0&& return(0) || (length(digits)==1&&digits[1]<2) && return(digits[1])
a <- (c(0,digits)*from_base**((length(c(0,digits)):1)-1))|>sum()
  a%%to_base**((log(a,to_base)|>ceiling()):0)|>diff()|>abs()/to_base**((log(a,to_base)|>ceiling()-1):0)
  }