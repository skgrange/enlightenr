#' Function to register parallel backend. 
#'
#' @author Stuart K. Grange
#' 
#' @return Invisible
#'
#' @export
register_cores <- function(cores = NA) {
  
  # Default is n - 1
  if (is.na(cores)) cores <- parallel::detectCores() - 1
  
  # Register
  doMC::registerDoMC()
  
  # No return
  
}
