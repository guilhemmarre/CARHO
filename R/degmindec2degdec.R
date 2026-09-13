
#####################################################################################################
### Function to transform a vector of coordinates from degrees decimal minutes to decimal degrees ###
#####################################################################################################

toDegDec <- function(coords_vect){
  
  coords_vect <- gsub(",",".",coords_vect)
  degree <- sapply(coords_vect, function(x)as.numeric(strsplit(x, " ")[[1]][1]))
  dec <- sapply(coords_vect, function(x)as.numeric(strsplit(x, " ")[[1]][2]) / 60)
  
  dec_degree <- unname(degree + dec)
  
  return(dec_degree)
  
}#eo function toDegDec