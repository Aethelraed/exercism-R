create <- function(row, col) {if(row<0||row>7||col<0||col>7) stop("invalid position") else c(row,col)}  

can_attack <- function(queen1, queen2) {  queen1[1]==queen2[1]||queen2[2]==queen1[2]||sum(queen1)==sum(queen2)||queen1[1]-queen1[2]==queen2[1]-queen2[2]}