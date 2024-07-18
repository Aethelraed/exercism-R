bob <- function(input) {
  if(input|>grepl(pattern = ".*[a-zA-Z]+.*") &input == toupper(input) & input|>grepl(pattern = "*\\?[_]*")){return("Calm down, I know what I'm doing!")}
  if(input|>grepl(pattern = "*\\?[_ ]*$")){return("Sure.")}
  if(input|>grepl(pattern = ".*[a-zA-Z]+.*") &input == toupper(input)){return("Whoa, chill out!")}
  if(!input|>grepl(pattern = "[^ \t\n]")){return("Fine. Be that way!")}
  return("Whatever.")
}