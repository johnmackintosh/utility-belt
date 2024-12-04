#' formats numbers represented as proportions as percentage
#'
#' @param x value to format as percentage
#' @param places number of decimal places in result
#'
#' @return input formatted as character, to desired decimal places
#' @export
#'
#' @examples
#' as_percent(0.2656, 1)
#' as_percent(0.2656, 2)
#' 
as_percent <- function(x, places = 1){
  
  in_place_val <- paste0("%.",places,"f%%")
  
  x <- sprintf(in_place_val, x * 100)
  
return(x) 
  
}
