normalized_plaintext <- function(input) {input|>tolower()|>strsplit("[^a-z0-9]")|>unlist()|>paste(collapse = "")}

plaintext_segments <- function(input,f = function(x){input[(0:(length(input)-1))%/%(sqrt(length(input))|>ceiling())==x]}) {
  if(input==""){return(input)}
  input <- input|>normalized_plaintext()|>strsplit(NULL)|>unlist()
  b <- Map(f,function(input){0:(sqrt((length(input)|>sqrt()|>ceiling())**2)-1)})
  if(b[length(b)]|>unlist()|>length()<1){b <- b[1:length(b)-1]}
  Map(function(x){paste(x,collapse = "")},b)|>unlist()|>c()
  }


encoded <- function(input,fill="",b=input|>plaintext_segments()|>strsplit(NULL)) {
  if(input==""){return("")}
  apply(c(b,rep(fill,length(b)*length(b[[1]])- b|>unlist()|>length()))|>unlist()|>matrix(ncol=length(b)), 1, function(x){paste(x,collapse = "")})|>paste(collapse = fill)
}

ciphertext <- function(input) {input|>encoded(fill=" ")}
