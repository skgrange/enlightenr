#' Function to further clean \strong{broom}'s tidy data frames. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
broom_sweep <- function(fit, extra = TRUE, round = 6) {
  
  # Get tidy data
  df <- broom::tidy(fit)
  
  # Clean names
  names(df) <- stringr::str_replace_all(names(df), "\\.", "_")
  # names(df) <- stringr::str_replace_all(names(df), "^std", "standard")
  
  # Clean intercept
  # df[, 1] <- ifelse(df[, 1] == "(Intercept)", "intercept", df[, 1])
  
  if (extra) {
    
    df_extra <- broom::glance(fit)
    names(df_extra) <- stringr::str_replace_all(names(df_extra), "\\.", "_")
    names(df_extra) <- stringr::str_to_lower(names(df_extra))
    names(df_extra) <- ifelse(names(df_extra) == "adj_r_squared",
                              "r_squared_adjusted",  names(df_extra))
    
    # Select only a few variables for now
    df_extra <- df_extra[, grep("r_squared|statistic|df", names(df_extra))]
    
    # Bind
    df <- cbind(df, df_extra)
    
  }
  
  # Round
  if (!is.na(round)) df <- threadr::round_numeric(df, round)
  
  return(df)
  
}


#' @rdname broom_sweep
#' @export
extract_slope <- function(df) {
  
 x <- as.numeric(df[df$term != "(Intercept)", ][2])
 if (is.na(x)) x <- df$slope
 if (is.null(x)) x <- NA
 return(x)
   
}


#' @rdname broom_sweep
#' @export
extract_intercept <- function(df) {
  
  x <- as.numeric(df[df$term == "(Intercept)", ][2])
  if (is.na(x)) x <- df$intercept
  if (is.null(x)) x <- NA
  return(x)
  
}
