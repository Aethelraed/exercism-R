rotate <- function(text, key) {
  rotate_char <- function(char, key){
    if(!(char %in% c(letters, LETTERS))){return(char)}
    if(char %in% LETTERS){return (LETTERS[(grep(char,LETTERS)+key-1)%%26+1])}
    letters[(grep(char,letters)+key-1)%%26+1]
  }
  Map(function(x){rotate_char(x,key%%26)},text|>strsplit(NULL)|>unlist())|>unlist()|>paste(collapse = "")
}