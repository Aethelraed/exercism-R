roman <- function(arabic,a=c(M=1000,D=500,C=100,L=50,X=10,V=5,I=1)) {
o <- rep(names(a),c(arabic%/%a[1],(c(0,arabic%%a)[1:(length(a))]%/%a)[2:length(a)]))|>paste0(collapse = "")
for (i in 1:(length(a)+2)) {
  o <- gsub(c(Map(\(x)rep(names(a)[x],4)|>paste0(collapse = ""),2:length(a))|>unlist(),"VIV","LXL","DCD")[i],c(Map(\(x)paste0(c(names(a)[x],names(a)[x-1]),collapse = ""),2:length(a))|>unlist(),"IX","XC","CM")[i],o)
}
o
}
