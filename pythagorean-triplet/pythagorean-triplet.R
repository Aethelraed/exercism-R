pythagorean_triplet <- function(n) {
b <- Map(\(x) (floor(n/4)):(x - 1),(floor(n/4):floor(n/2)))
c <- Map (\(i)rep((floor(n/4):floor(n/2))[i],length(b[i]|>unlist())),1:length(b))|>unlist()
b <- unlist(b)
bb <- b[((n-b-c)**2+b**2==c**2)]
cc <- c[((n-b-c)**2+b**2==c**2)]
r <- Map(\(i)c(n-bb[i]-cc[i],bb[i],cc[i]),1:length(bb))[((n-bb-cc)<bb)&(bb<cc)]
if(r|>length()==0)r else r[length(r):1]
}