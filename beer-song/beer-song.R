lyrics <- function(first, last) {
  Map(verse,first:last)|>unlist()|>paste(sep = "",collapse = "\n")
}

verse <- function(number) {
  if (number == 2) {
    return(paste(
      paste0(
        number,
        " bottles of beer on the wall, ",
        number,
        " bottles of beer."
      ),
      paste0(
        "Take one down and pass it around, ",
        number - 1,
        " bottle of beer on the wall.\n"
      ),
      sep = "\n"
    ))
  }
  if (number == 1) {
    return(paste(
      paste0(
        number,
        " bottle of beer on the wall, ",
        number,
        " bottle of beer."
      ),
      paste0(
        "Take it down and pass it around,",
        " no more bottles of beer on the wall.\n"
      ),
      sep = "\n"
    ))
  }
  if (number == 0) {
    return(paste(
      paste0(
        "No more",
        " bottles of beer on the wall, ",
        "no more",
        " bottles of beer."
      ),
      "Go to the store and buy some more, 99 bottles of beer on the wall.\n",
      sep = "\n"
    ))
  }
  paste(paste0(number," bottles of beer on the wall, ",number," bottles of beer."),
        paste0("Take one down and pass it around, ",number-1," bottles of beer on the wall.\n"),sep = "\n")
}