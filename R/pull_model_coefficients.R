#' Functions to pull useful pieces out of model objects. 
#' 
#' @author Stuart K. Grange
#' 
#' @param model A model object. 
#' 
#' @param named Should the return vector be named? 
#' 
#' @return A vector, either named or unnamed. 
#' 
#' @export
pull_model_coefficients <- function(model, named = FALSE) {
  
  if (class(model)[1] == "lm") {
    
    # Extract
    x <- coefficients(model)
    
    # Better names, or drop them
    if (named) {
      
      names(x) <- c("intercept", "slope")
      
    } else {
      
      x <- unname(x)
      
    }
    
  } else {
    
    stop("`model` not recognised...", call. = FALSE)
    
  }
  
  return(x)
  
}


#' @rdname pull_model_coefficients
#' 
#' @export
pull_model_r_squared <- function(model) {
  
  if (class(model)[1] == "lm") {
    
    x <- summary(model)$r.squared
    
  } else if (class(model)[1] == "randomForest.formula") {
    
    x <- max(model$rsq)
    
  } else {
    
    stop("`model` not recognised...", call. = FALSE)
    
  }
  
  return(x)
  
} 


#' @rdname pull_model_coefficients
#' 
#' @export
pull_model_mse <- function(model) {
  
  if (class(model)[1] == "randomForest.formula") {
    
    x <- min(model$mse)
    
  } else {
    
    stop("`model` not recognised...", call. = FALSE)
    
  }
  
  return(x)
  
}


#' @rdname pull_model_coefficients
#' 
#' @export
pull_model_slope <- function(model) {
  
  if (class(model)[1] == "lm") {
    
    x <- unname(coefficients(model)[2])
    
  } else {
    
    stop("`model` not recognised...", call. = FALSE)
    
  }
  
  return(x)
  
}


#' @rdname pull_model_coefficients
#' 
#' @export
pull_model_intercept <- function(model) {
  
  if (class(model)[1] == "lm") {
    
    x <- unname(coefficients(model)[1])
    
  } else {
    
    stop("`model` not recognised...", call. = FALSE)
    
  }
  
  return(x)
  
}


#' @rdname pull_model_coefficients
#' 
#' @export
pull_model_p_value <- function(model) {
  
  if (class(model)[1] == "lm") { 
    
    x <- summary(model)$coefficients[2, 4]
    
  } else {
    
    stop("`model` not recognised...", call. = FALSE)
    
  }
  
  return(x)
  
}


#' @rdname pull_model_coefficients
#' 
#' @export
pull_model_f_statistic <- function(model) {
  
  if (class(model)[1] == "lm") { 
    
    x <- unname(summary(model)$fstatistic[1])
    
  } else {
    
    stop("`model` not recognised...", call. = FALSE)
    
  }
  
  return(x)
  
}


#' @rdname pull_model_coefficients
#' 
#' @export
pull_model_degrees_of_freedom <- function(model) {
  
  if (class(model)[1] == "lm") { 
    
    x <- summary(model)$df
    
  } else {
    
    stop("`model` not recognised...", call. = FALSE)
    
  }
  
  return(x)
  
}
