saddle_point <- function(input) {
  if(input|>dim()|>prod()==0)return(data.frame(row = numeric(), col = numeric()))
  hip <- matrix(rep(dim(input)|>diff()|>sign()*Inf,(dim(input)|>max())**2),nrow =(dim(input)|>max())) 
  hip[1:dim(input)[1],1:dim(input)[2]] <- input
  which((Map(function(x){x==max(x,na.rm=T)},hip|>t()|>data.frame())|>unlist()|>matrix(nrow = dim(hip)[1],ncol = dim(hip)[2])|>t()+Map(function(x){x==min(x,na.rm=T)},hip|>data.frame())|>unlist()|>matrix(nrow = dim(hip)[1],ncol = dim(hip)[2]))==2,T)|>data.frame()
}