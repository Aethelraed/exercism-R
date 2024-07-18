diamond <- function(letter) {
  if(letter=="A")return("A")
  a <- 1:26
  names(a) <- LETTERS
  cols <- 2 * (a[letter]) - 1
  Map(function(char) {
    x <- rep(" ", cols)
    x[c((cols + 1) / 2 - a[char] + 1, (cols + 1) / 2 + a[char] - 1)] <- char
    paste(x, sep = "", collapse = "")
  }, LETTERS[c(1:a[letter], (a[letter]-1):1)]) |> paste(collapse = "\n")
}