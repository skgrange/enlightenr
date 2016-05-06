#' Function to quickly plot many quantiles regression lines on a scatterplot. 
#' 
#' @param df Data frame.
#' 
#' @param y Variable which is dependent on \code{x}. 
#'  
#' @param x Variable which predicts \code{y}. 
#' 
#' @param type Type of plot. Can either be \code{"scatterplot"} or 
#' \code{"coefficients"}. 
#' 
#' @param taus Quantiles to plot. Default uses \code{seq(0.05, 0.95, by = 0.05)}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
plot_many_quantiles <- function(df, y, x, type = "scatterplot", 
                                taus = seq(0.05, 0.95, by = 0.05)) {
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  formula <- as.formula(formula)
  
  if (type == "scatterplot") {
    
    # Square plot bitte
    graphics_anchor <- par()$pty
    par(pty = "s")
    
    # Get axes limits to make plot 
    min <- min(c(df[, x], df[, y]), na.rm = TRUE)
    max <- max(c(df[, x], df[, y]), na.rm = TRUE)
    
    # grid(NULL, NULL, lty = 6, col = "lightgrey")
    
    # Base plot, just points
    plot(df[, x], df[, y], type = "p", xlim = c(min, max), ylim = c(min, max))
    
    # Plot ordinary least squares line
    abline(lm(formula, data = df), lty = 2, col = "red")
    
    # Plot median line
    abline(quantreg::rq(formula, tau = 0.5, data = df), col = "blue")
    
    # Drop median
    taus <- setdiff(taus, 0.5)
    
    # Plot all other quantiles
    plyr::l_ply(taus, function(z) add_quantile_lines(df, z, y = y, x = x))
    
    # Build legend
    legend("topleft", 
           legend = c("Data", "OLS", "Median fit", "Other quantile fits"), 
           pch = c(1, NA, NA, NA), 
           lty = c(NA, 2, 1, 1), 
           col = c("black", "red", "blue", "darkgrey"), 
           bty = "o")
    
    # Remove graphics setting
    par(pty = graphics_anchor)
    
  }
  
  if (type == "coefficients") {
    
    # Build many models
    models <- quantreg::rq(formula, tau = taus, data = df)
    
    # Make summary
    suppressWarnings(
      models <- summary(models)
    )
    
    # Plot coefficients
    plot(models, mfrow = c(2, 1))
    
  }
  
  # No return
  
}


# No export
add_quantile_lines <- function(df, tau, y, x) {
  
  formula <- stringr::str_c(y, " ~ ", x)
  formula <- as.formula(formula)
  abline(quantreg::rq(formula, tau = tau, data = df), col = "darkgrey")
  
}
