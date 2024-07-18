nucleotide_count <- function(input, aminos = c("A", "C", "G", "T")) {
  if((input!="")&(input|>strsplit(NULL)|>unlist()|>grepl(pattern="[^ACGT]")|>sum()>0)){stop("crispr")}
  Map(function(x) {input |> strsplit(NULL) |> unlist() |> grep(pattern = x) |> length()}, aminos)
}
