#' Function to apply kernel smoothing functions. 
#' 
#' @description \code{kernel_smoother} is used to transform a vector. 
#' 
#' @details Current kernels which are supported are:
#'  
#' \itemize{
#'  \item Gaussian/normal
#'  \item Epanechnikov
#'  \item Logistic
#'  \item Cosine
#'  \item Triangular
#'  \item Box/uniform
#'  \item Tricube
#'  \item Triweight
#'  \item Biweight/quartic
#' }
#' 
#' @param x Vector to transform/smooth. 
#' 
#' @param kernel Type of smoothing kernel to apply to \code{x}. See details 
#' for supported methods. 
#' 
#' @return Vector the length of \code{x}. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \href{https://en.wikipedia.org/wiki/Kernel_(statistics)}{wikipedia}
#' 
#' @examples 
#' 
#' # Create vector
#' vector <- seq(-5, 5, by = 0.1)
#' 
#' # Transform
#' vector_smooth <- kernel_smoother(vector, "gaussian")
#' 
#' # Or use a different kernel
#' vector_smooth_epanechnikov <- kernel_smoother(vector, "epanechnikov")
#' # plot(vector_smooth_epanechnikov, type = "l")
#' 
#' @export
kernel_smoother <- function(x, kernel = "gaussian") {
  
  # Parse
  kernel <- stringr::str_to_lower(kernel)
  
  # Normal
  if (kernel %in% c("gaussian", "normal")) 
    x <- (2 * pi) ^ -0.5 * exp(-0.5 * x ^ 2)
  
  if (kernel == "epanechnikov") x <- 3 / 4 * (1 - x ^ 2) * indicator_function(x)
  
  if (kernel == "logistic") x <- 1 / (exp(x) + 2 + exp(-x))
  
  if (kernel == "cosine") 
    x <- pi / 4 * cos((pi / 2) * x) * indicator_function(x)
  
  if (kernel == "triangular") x <- (1 - abs(x)) * indicator_function(x)
  
  if (kernel %in% c("box", "uniform")) x <- 1 / 2 * indicator_function(x)
  
  if (kernel == "tricube") 
    x <- 70 / 81 * (1 - abs(x) ^ 3) ^ 3 * indicator_function(x)
  
  if (kernel == "triweight")
    x <- 35 / 32 * (1 - x ^ 2) ^ 3 * indicator_function(x)
  
  if (kernel %in% c("biweight", "quartic"))
    x <- 15 / 16 * (1 - x ^ 2) ^ 2 * indicator_function(x)
  
  return(x)
  
}


# No export 
indicator_function <- function(x) ifelse(abs(x) <= 1, 1, 0)
