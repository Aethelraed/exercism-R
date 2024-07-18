allergy <- function(num) {
  if(num%%256==0){return(character())}
  binary <- function(n){(n%/%c(128,64,32,16,8,4,2,1))%%2}
  allergies <- c("cats","pollen","chocolate","tomatoes","strawberries","shellfish","peanuts","eggs")
  allergies[num%%256|>binary()|>as.logical()]
}

allergic_to <- function(allergy_object, allergy) {
  allergy%in%allergy_object
}

list_allergies <- function(allergy_object) {
  allergy_object  
}
