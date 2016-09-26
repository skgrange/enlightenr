#' Function to apply a linear map transformation to a numeric vector. 
#' 
#' @param x Numeric vector.
#' @param from Starting value for linear map. 
#' @param to End value for linear map. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
linear_map <- function(x, from, to)
  (x - min(x, na.rm = TRUE)) / max(x - min(x, na.rm = TRUE), na.rm = TRUE) * 
  (to - from) + from
