anagram <- function(subject, candidates) {
  trn <- function(input){
    tolower(input)|>strsplit(NULL)|>unlist()|>sort()|>paste0(collapse = "")
  }
  c <- tolower(candidates)
  s <- trn(subject)
  x <- rep(0,length(candidates))
  for (i in 1:length(candidates)) {
    x[i] <- trn(c[i])==s
  }
  if(sum(x)<1){return(c())}
  names(candidates) <- c
  candidates <- candidates[as.logical(x)]
  y <- candidates[names(candidates)!=tolower(subject)]
  names(y) <- NULL
  if(length(y)<1){return(NULL)}
  y
  
}