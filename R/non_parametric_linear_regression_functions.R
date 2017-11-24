#' Functions to apply non-parametric linear regression methods to observations. 
#' 
#' The Theil-Sen single median and Siegel repeated medians regression techniques 
#' are considered robust regression techniques and often suitable when parametric
#' techniques fail or their assumptions are violated. 
#' 
#' @seealso \code{\link{mblm}}
#' 
#' @author Stuart K. Grange
#' 
#' @import ggplot2
#' 
#' @export
theil_sen_estimator <- function(df, y, x, round = 6, plot = FALSE) {
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  formula <- as.formula(formula)
  
  # Only complete observations
  df <- df[, c(x, y)]
  df <- na.omit(df)
  
  # Repeated false is the Thiel-Sen test
  fit <- mblm::mblm(formula, df, repeated = FALSE)
  
  # Get tidy data from fit
  fit <- broom_sweep(fit, extra = TRUE, round)
  fit$method <- "theil_sen_single_median"
  
  if (plot) {
    
    m <- extract_slope(fit)
    c <- extract_intercept(fit)
    c <- ifelse(is.na(c), 0, c)
    
    # Add method
    df$method <- fit$method[1]
    
    plot <- ggplot(df, aes_string(x, y)) + 
      geom_point(pch = 1, colour = "black", na.rm = TRUE) + 
      geom_abline(slope = m, intercept = c) + theme(aspect.ratio = 1) + 
      facet_wrap("method")
    
    print(plot)
    
  }
  
  return(fit)
  
}


#' @rdname theil_sen_estimator
#' 
#' @import ggplot2
#' 
#' @export
siegel_repeated_medians <- function(df, y, x, round = 6, plot = FALSE) {
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  formula <- as.formula(formula)
  
  # Only complete observations
  df <- df[, c(x, y)]
  df <- na.omit(df)
  
  # Repeated true is Siegel repeated medians
  fit <- mblm::mblm(formula, df, repeated = TRUE)
  
  # Get tidy data from fit
  fit <- broom_sweep(fit, extra = TRUE, round)
  fit$method <- "siegel_repeated_medians"
  
  if (plot) {
    
    m <- extract_slope(fit)
    c <- extract_intercept(fit)
    c <- ifelse(is.na(c), 0, c)
    
    # Add method
    df$method <- fit$method[1]
    
    plot <- ggplot(df, aes_string(x, y)) + 
      geom_point(pch = 1, colour = "red", na.rm = TRUE) + 
      geom_abline(slope = m, intercept = c) + theme(aspect.ratio = 1) + 
      facet_wrap("method")
    
    print(plot)
    
  }
  
  return(fit)
  
}


#' Function to calculate the Thiel-Sen estimate of slope after the application 
#' of a prewhitening algorithm. 
#' 
#' The prewhitening algorithm helps deal with autocorrelation. 
#' 
#' @param x Vector of data. This will usually be a padded timeseries. 
#' 
#' @param method Prewhitening method to use. Can be \code{"yuepilon"} or 
#' \code{"zhang"} and default is \code{"yuepilon"}. 
#' 
#' @param round Returned coefficients precision. Default is 6 dp. 
#' 
#' @return Data frame of coefficients. 
#' 
#' @author Stuart K. Grange
#' 
#' @import ggplot2
#' 
#' @export
theil_sen_prewhiten <- function(x, method = "yuepilon", round = 6, plot = FALSE) {
  
  # Model
  fit <- zyp::zyp.trend.vector(x, method = method, conf.intervals = TRUE)
  
  # To data frame
  fit <- data.frame(t(fit))
  
  # Change names
  names(fit) <- ifelse(names(fit) == "trend", "slope", names(fit))
  
  # Add method
  fit$method <- "prewhitened_thiel_sen"
  
  # Round
  if (!is.na(round)) fit <- threadr::round_numeric(fit, round)
  
  if (plot) {
    
    # Build a data frame
    df <- data.frame(
      row_number = seq.int(1, length(x)),
      x = x,
      method = fit$method
    )
    
    # Build plot
    plot <- ggplot(df, aes(row_number, x)) + 
      geom_point(pch = 1, colour = "purple", na.rm = TRUE) + 
      geom_abline(slope = fit$slope, intercept = fit$intercept) + 
      theme(aspect.ratio = 1) + facet_wrap("method")
    
    # Display
    print(plot)
    
  }
  
  return(fit)
  
}
