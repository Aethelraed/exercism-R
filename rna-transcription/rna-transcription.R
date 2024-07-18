to_rna <- function(dna) {
  if(dna|>grepl(pattern="[^ACGT]")){stop()}
  aminos <- c("G","C","U","A")
  names(aminos) <- c("C","G","A","T")
  Map(function(aa){aminos[aa]},dna|>strsplit(NULL))|>unlist()|>paste(collapse = "")
}