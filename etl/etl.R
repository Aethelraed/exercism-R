etl <- function(input) {
  b <- input |> unlist() 
  c <- Map(length, input)|>unlist()
  names(b) <- rep(names(c),c)
  b <- b|> tolower() |> sort()
  eval(str2expression(text = paste(
    "list(", paste(paste(
      b, "=", names(b), ""
    ), collapse = ","), ")"
  )))
}