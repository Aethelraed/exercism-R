is_armstrong_number <- function(n) {sum((n|>as.character()|>strsplit(NULL)|>unlist()|>as.numeric())**(n|>as.character()|>strsplit(NULL)|>unlist()|>length()))==n}
