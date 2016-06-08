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
                                     method = "qr", weights = NULL, round = 6, 
                                     plot = TRUE) {
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  
  # Force intercept
  if (zero_intercept) formula <- stringr::str_c(formula, " -1")
  
  # Parse
  formula <- as.formula(formula)
  
  # Get weights vector
  if (!is.null(weights)) 
    weights_vector <- df[, weights] else weights_vector <- NULL
  
  # Model
  fit <- lm(formula, data = df, method = method, weights = weights_vector)
  
  # Get tidy data from fit
  fit <- broom_sweep(fit, extra = TRUE, round)
  
  # Give some names
  if (is.null(weights_vector)) {
    
    fit$method <- "least_squares_regression"
    fit$method_name <- "Least squares regression"
    
  } else {
    
    fit$method <- "weighted_least_squares_regression"
    fit$method_name <- "Weighted least squares regression"
    
  }
  
  if (plot) {
    
    m <- extract_slope(fit)
    c <- extract_intercept(fit)
    c <- ifelse(is.null(c), NA, c)
    c <- ifelse(is.na(c), 0, c)

    # Add method
    df$method_name <- fit$method_name[1]
    
    plot <- ggplot(df, aes_string(x, y)) + 
      geom_point(pch = 1, colour = "dodgerblue", na.rm = TRUE) + 
      geom_abline(slope = m, intercept = c) + theme(aspect.ratio = 1) + 
      facet_wrap("method_name")
    
    print(plot)
    
  }
  
  # Return
  fit
  
}


#' @rdname least_squares_regression
#' @export
simple_linear_regression <- least_squares_regression


#' @rdname least_squares_regression
#' @import ggplot2
#' @export
robust_linear_regression <- function(df, y, x, zero_intercept = FALSE,
                                     method = "MM", weights = NULL, round = 6,
                                     plot = TRUE) {
  
  # Ensure uppercase
  method <- stringr::str_to_upper(method)
  
  if (!method %in% c("M", "MM")) 
    stop("'method' must be 'M' or 'MM'.", call. = FALSE)
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  
  # Force intercept
  if (zero_intercept) formula <- stringr::str_c(formula, " -1")
  
  # Parse
  formula <- as.formula(formula)
  
  # Get weights vector
  if (!is.null(weights))
    weights_vector <- df[, weights] else weights_vector <- NULL
  
  # The "best" MM-estimator does not allow for weights
  if (method == "MM" & !is.null(weights)) {
    
    # No weighting
    weights_vector <- NULL
    
    # Message
    warning("The 'MM'-method cannot use weights. Therefore the weights have been removed.\nConsider altering 'method'.",
            call. = FALSE)
    
  }
  
  # Model
  fit <- MASS::rlm(formula, data = df, method = method, weights = weights_vector)
  
  # Get tidy data from fit
  fit <- broom_sweep(fit, extra = TRUE, round)
  
  # Give some names
  if (is.null(weights_vector)) {
    
    fit$method <- "robust_linear_regression"
    fit$method_name <- "Robust linear regression"
    
  } else {
    
    fit$method <- "weighted_robust_linear_regression"
    fit$method_name <- "Weighted robust linear regression"
    
  }
  
  if (plot) {
    
    m <- extract_slope(fit)
    c <- extract_intercept(fit)
    # 
    c <- ifelse(is.null(c), NA, c)
    c <- ifelse(is.na(c), 0, c)
    
    # Add method
    df$method_name <- fit$method_name[1]
    
    plot <- ggplot(df, aes_string(x, y)) + 
      geom_point(pch = 1, colour = "darkorange", na.rm = TRUE) + 
      geom_abline(slope = m, intercept = c) + theme(aspect.ratio = 1) + 
      facet_wrap("method_name")
    
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
                                round = 6, plot = TRUE) {
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  
  # Force intercept
  if (zero_intercept) formula <- stringr::str_c(formula, " -1")
  
  # Parse
  formula <- as.formula(formula)
  
  # Get weights vector
  if (!is.null(weights)) 
    weights_vector <- df[, weights] else weights_vector <- NULL
  
  # Model
  fit <- quantreg::rq(formula, tau = tau, data = df,  method = method, 
                      weights = weights_vector)
  
  # Get tidy data from fit
  fit <- broom_sweep(fit, extra = TRUE, round)
  
  # Give some names
  if (is.null(weights_vector)) {
    
    fit$method <- "quantile_regresssion"
    fit$method_name <- "Quantile regression"
    
  } else {
    
    fit$method <- "weighted_quantile_regresssion"
    fit$method_name <- "Weighted quantile regression"
    
  }
  
  if (plot) {
    
    m <- extract_slope(fit)
    c <- extract_intercept(fit)
    # 
    c <- ifelse(is.null(c), NA, c)
    c <- ifelse(is.na(c), 0, c)
    
    # Add method
    df$method_name <- fit$method_name[1]
    
    plot <- ggplot(df, aes_string(x, y)) + 
      geom_point(pch = 1, colour = "purple", na.rm = TRUE) + 
      geom_abline(slope = m, intercept = c) + theme(aspect.ratio = 1) + 
      facet_wrap("method_name")
    
    print(plot)
    
  }
  
  # Return
  fit
  
}
