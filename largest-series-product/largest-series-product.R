largest_series_product <- function(digits, span){
  y <- digits|>strsplit(NULL)|>unlist()
  if(y|>grepl(pattern = "[^0-9]")|>sum()>0|span<1|span>y|>length()) stop()
Map(function(x){x|>unlist()|>as.numeric()|>prod()},Map(function(x){y[x:(x+span-1)]},1:(length(y)-span+1)))|>unlist()|>max()

}
