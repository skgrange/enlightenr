#' Function to predict using a model object and return a vector. 
#' 
#' @param list_model Model object which can be used with \code{\link{predict}}. 
#' 
#' @param df Data frame containing input data which can be used by \code{model},
#' not always needed. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Predict using a random forest model
#' value_predict <- make_prediction(list_random_forest, data_testing_set)
#' 
#' }
#' 
#' @export
make_prediction <- function(model, df) {
  
  # Get class of model object
  model_class <- class(model)[1]
  
  # Different prediction logic dependent on model type
  if (model_class == "gam") {
    
    # Seems to be generic
    x <- unname(predict(model, df))
    
  } else if (model_class == "randomForest.formula") {
    
    # Can be generic, but use name space for when the package is not loaded
    x <- unname(randomForest:::predict.randomForest(model, df))
    
  } else if (model_class == "ksvm") {
    
    # Not generic and returns a matrix
    x <- unname(kernlab::predict(model, df))[, 1]
    
  } else if (model_class == "gbm") {
    
    # Use a vector but needs an extra argument, comes from model object
    x <- gbm::predict.gbm(
      model, 
      df, 
      n.trees = length(model$trees)
    )
    
  } else {
    
    stop("Model not recognised.", call. = FALSE)
    
  }
  
  return(x)
  
}
