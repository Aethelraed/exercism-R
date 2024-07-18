two_fer <- function(input="") {
  me <- ", one for me."
  Of <- "One for "
  if(input=="") input <- "you"
  paste0(Of,input,me)
}
