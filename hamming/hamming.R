# This is a stub function to take two strings
# and calculate the hamming distance
hamming <- function(strand1, strand2) {
  s1 <- strsplit(strand1, split = NULL)|>unlist()
s2 <- strsplit(strand2, split = NULL)|>unlist()
if(length(s1)!=length(s2)){
  simpleError("Different Lengths") 
  stop()}
  sum(s1 != s2)

}
