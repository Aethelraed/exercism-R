raindrops <- function(number) {
  out <- ""
  if(number%%3==0){
    out <- paste0(out,"Pling")
  }
  if(number%%5==0){
    out <- paste0(out,"Plang")
  }
  if(number%%7==0){
    out <- paste0(out,"Plong")
  }
  if(out!=""){return(out)}
  as.character(number)
}
