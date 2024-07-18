is_isogram <- function(word) {
  x <- word|>strsplit(NULL)|>unlist()|>tolower()
  x <- x[x%in%c(letters,LETTERS)]
  x|>length()==x|>unique()|>length()
}