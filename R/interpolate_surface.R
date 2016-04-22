
#' Function to conveniently interpolate a reglar surface from irregular 
#' observations. 
#' 
#' \code{interpolate_surface} uses the \strong{akima} package to interpolate
#' and the \strong{fields} package to smooth the surface.
#' 
#' @param df Data frame.
#' @param x Variable.
#' @param y Variable. 
#' @param z Variable. 
#' @param smooth Should the surface be smoothed? 
#' @param length Length of grid to interpolate to. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
interpolate_surface <- function(df, x = "x", y = "y", z = "z", smooth = TRUE,
                                length = 100) {
  
  # Prepare input
  df <- threadr::base_df(df)
  
  # No NAs in interpolation function
  df <- df[, c(x, y, z)]
  df <- df[complete.cases(df), ]
  
  # Build output grid
  xo <- seq(min(df[, x]), max(df[, x]), length.out = length)
  yo <- seq(min(df[, y]), max(df[, y]), length.out = length)
  
  # Get a surface
  surface <- akima::interp(df[, x], df[, y], df[, z], xo = xo, yo = yo,
                           duplicate = "mean")
  
  # Smooth slightly
  if (smooth)
    surface <- fields::image.smooth(surface, xwidth = 1, ywidth = 1, theta = 0.5)
  
  # To data frame
  df_surface <- expand.grid(surface[1:2])
  df_surface$z <- as.vector(surface$z)
  
  # Alter names
  names(df_surface) <- c(x, y, z)
  
  # Return
  df_surface
  
}
