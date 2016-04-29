#' Function to apply linear regression techniques to observations. 
#' 
#' \code{y} is a function of \code{x} or \code{y = y(x)}. 
#' 
#' @author Stuart K. Grange
#' 
#' @import ggplot2
#' 
#' @export
least_squares_regression <- function(df, y, x, zero_intercept = FALSE,
                                     weights = NULL,  round = 6, plot = FALSE) {
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  
  # Force intercept
  if (zero_intercept) formula <- stringr::str_c(formula, " -1")
  
  # Parse
  formula <- as.formula(formula)
  
  # Model
  fit <- lm(formula, data = df, weights)
  
  # Get tidy data from fit
  fit <- broom_sweep(fit, extra = TRUE, round)
  fit$method <- "least_squares_regression"
  
  if (plot) {
    
    m <- extract_slope(fit)
    c <- extract_intercept(fit)
    c <- ifelse(is.na(c), 0, c)
    
    # Add method
    df$method <- fit$method[1]
    
    plot <- ggplot(df, aes_string(x, y)) + 
      geom_point(pch = 1, colour = "dodgerblue", na.rm = TRUE) + 
      geom_abline(slope = m, intercept = c) + theme(aspect.ratio = 1) + 
      facet_wrap("method")
    
    print(plot)
    
  }
  
  # Return
  fit
  
}


#' @rdname least_squares_regression
#' @import ggplot2
#' @export
robust_linear_regression <- function(df, y, x, zero_intercept = FALSE,
                                     weights = NULL, method = "M", round = 6,
                                     plot = FALSE) {
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  
  # Force intercept
  if (zero_intercept) formula <- stringr::str_c(formula, " -1")
  
  # Parse
  formula <- as.formula(formula)
  
  # Model
  fit <- MASS::rlm(formula, data = df, weights, method = method)
  
  # Get tidy data from fit
  fit <- broom_sweep(fit, extra = TRUE, round)
  fit$method <- "robust_linear_regression"
  
  if (plot) {
    
    m <- extract_slope(fit)
    c <- extract_intercept(fit)
    # 
    c <- ifelse(is.null(c), NA, c)
    c <- ifelse(is.na(c), 0, c)
    
    # Add method
    df$method <- fit$method[1]
    
    plot <- ggplot(df, aes_string(x, y)) + 
      geom_point(pch = 1, colour = "darkorange", na.rm = TRUE) + 
      geom_abline(slope = m, intercept = c) + theme(aspect.ratio = 1) + 
      facet_wrap("method")
    
    print(plot)
    
  }
  
  # Return
  fit
  
}


#' @rdname least_squares_regression
#' @import ggplot2
#' @export
quantile_regression <- function(df, y, x, zero_intercept = FALSE,
                                weights = NULL, tau = 0.5, method = "br", 
                                round = 6, plot = FALSE) {
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  
  # Force intercept
  if (zero_intercept) formula <- stringr::str_c(formula, " -1")
  
  # Parse
  formula <- as.formula(formula)
  
  # Model
  fit <- quantreg::rq(formula, tau = tau, data = df, weights, method = method)
  
  # Get tidy data from fit
  fit <- broom_sweep(fit, extra = TRUE, round)
  fit$method <- "quantile_regresssion"
  
  
  if (plot) {
    
    m <- extract_slope(fit)
    c <- extract_intercept(fit)
    # 
    c <- ifelse(is.null(c), NA, c)
    c <- ifelse(is.na(c), 0, c)
    
    # Add method
    df$method <- fit$method[1]
    
    plot <- ggplot(df, aes_string(x, y)) + 
      geom_point(pch = 1, colour = "darkorange", na.rm = TRUE) + 
      geom_abline(slope = m, intercept = c) + theme(aspect.ratio = 1) + 
      facet_wrap("method")
    
    print(plot)
    
  }
  
  # Return
  fit
  
}
