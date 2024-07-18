triangle <- function(x, y, z,out=c(x,y,z),classes=c("equilateral","isosceles","scalene")) {
  if(!((x + y >= z) && (y + z >= x) && (x + z >= y))|sum(out)==0){stop()}
  class(out) <- classes[c((x==y&y==z),(x==y|y==z|z==x),(x!=y&y!=z&z!=x))]
  out
}