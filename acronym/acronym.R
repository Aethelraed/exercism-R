acronym <- function(input) {
  Map(function(x) {
    if(x%in%c("")){return(NULL)}
    y <- strsplit(x, NULL) |> unlist()
    y[1]
  }, input |> strsplit("[_ -]") |> unlist())|>unlist()|>paste(collapse = "")|>toupper()
}
