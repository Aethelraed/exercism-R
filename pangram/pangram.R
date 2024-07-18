is_pangram <- function(input) {
  x <- input|>strsplit(NULL)|>unlist()|>tolower()
  x <- x[x%in%letters]
  x|>unique()|>length()==length(letters)
}