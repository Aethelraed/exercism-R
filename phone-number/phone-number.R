parse_phone_number <- function(number_string) {
  x <- strsplit(number_string,NULL)|>unlist()
  x <- x[x%in%0:9]
  if(x[1]==1&length(x)==11){x <- x[2:11]}
  if(length(x)!=10 | sum(x[c(1,4)]<2)){return(NULL)}
  paste0(collapse = "",x)
  
}