word_count <- function(input) {
  word_breakers <- c(" ",":", "!","?","\t", "\n",",",".","&","@","$","%","^")
  words <- tolower(input)
  for (i in 1:length(word_breakers)) {
    words <- strsplit(words|>unlist(),word_breakers[i], fixed = T)
  }
  words <- strsplit(words|>unlist(),"^'")
  words <- strsplit(words|>unlist(),"'$")
  words <- unlist(words, recursive = T)
  y <- unique(words)|>sort()
  Map(function(x){grep(paste0("^",x,"$"),words)|>length()},y)
}
