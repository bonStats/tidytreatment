is_01_integer_vector <- function(x){

  class(x) == "integer" & all( x %in% c(0,1) )

}
