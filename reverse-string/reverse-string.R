reverse <- function(text) {
  a <- text|>strsplit("")|>unlist()
  if(text=="")"" else a[length(a):1]|>paste0(collapse = "")
}
