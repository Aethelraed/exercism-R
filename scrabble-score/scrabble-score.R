scrabble_score <- function(input){
  a <- c(1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10)
  names(a) <- letters
  a[input|>strsplit(NULL)|>unlist()|>tolower()]|>sum()
}