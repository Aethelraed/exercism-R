translate <- function(bases) {
  m <- c(  AUG = "Methionine",  UUU = "Phenylalanine",  UUC = "Phenylalanine", UUA = "Leucine",  UUG = "Leucine", UCU = "Serine",  UCC = "Serine",  UCA = "Serine",  UCG = "Serine", UAU = "Tyrosine",  UAC = "Tyrosine", UGU = "Cysteine",  UGC = "Cysteine",  UGG = "Tryptophan", UAA = "STOP",  UAG = "STOP",  UGA = "STOP")
h <- bases|>strsplit("")|>unlist()
j <- c(Map(\(x)h[0:(length(h)-1)%/%3==x]|>paste0(collapse = ""),(1:(length(h)%/%3)-1))|>unlist(),if(length(h)%%3>0){h[length(h)+1-(1:(length(h)%%3))]},"STOP")
for (i in 1:length(m)) j <- gsub(names(m)[i],m[i],j)
j <- j[1:grep("STOP",c(j,"STOP"))[1]-1]
if(grepl("NA|STOP",j)|>sum()>0|length(j)<1) NULL else if(!sum(j%in%m)==length(j)) stop() else j
}
