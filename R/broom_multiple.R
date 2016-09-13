#' Function to apply all of \strong{broom}'s tidying functions and return a list.
#' 
#' @param model A model object. 
#' @param transformed Should the transformed original data be returned too? 
#' Default is \code{FALSE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
broom_multiple <- function(model, transformed = FALSE) {

  # Use broom  
  df_tidy <- broom::tidy(model)
  
  df_glance <- broom::glance(model)
  
  if (transformed) df_augment <- broom::augment(model) else df_augment <- NULL
  
  # Build list object
  list_broom <- list(
    summary = df_tidy,
    summary_concise = df_glance,
    transformed = df_augment
  )
  
  # Drop null list elements
  index <- sapply(list_broom, is.null)
  list_broom <- list_broom[!index]
  
  # Return
  list_broom
  
}
