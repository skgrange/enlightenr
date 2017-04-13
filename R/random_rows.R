#' Function to get a vector of random rows for data frame sampling, usually for
#' creating training and testing data frames for models. 
#' 
#' @param df Data frame to get random rows from. 
#' 
#' @param seed What \code{set.seed} should be used? 
#' 
#' @param fraction Fraction of \code{df} to sample. 
#' 
#' @return Integer vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
random_rows <- function(df, seed = 123, fraction = 0.8) {
  
  # Set seed
  set.seed(seed)
  
  # Get n
  n <- nrow(df)
  
  # Sample
  rows <- sample(n, round(n * fraction))
  
  return(rows)
  
}
