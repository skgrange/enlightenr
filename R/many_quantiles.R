#' Function to apply many quantile regression models and return a tidy data 
#' frame. 
#' 
#' @param df Data frame.
#' 
#' @param y Variable which is dependent on \code{x}. 
#'  
#' @param x Variable which predicts \code{y}. 
#' 
#' @param taus Quantiles to plot. Default uses \code{seq(0.05, 0.95, by = 0.05)}. 
#' 
#' @param round Decimal point to round all numeric variables in the returned 
#' data frame. Default is 6. 
#' 
#' @return Data frame.
#' 
#' @author Stuart K. Grange
#' 
#' @export
many_quantiles <- function(df, y, x, taus = seq(0.05, 0.95, by = 0.05), 
                           round = 6) {
  
  # Build formula
  formula <- stringr::str_c(y, " ~ ", x)
  formula <- as.formula(formula)
  
  # Build many models
  models <- quantreg::rq(formula, tau = taus, data = df)
  
  # Make summary
  suppressWarnings(
    models_summary <- summary(models)
  )
  
  # Tidy
  # Get coefficents
  df_models <- lapply(models_summary, coef)
  df_models <- do.call("rbind", df_models)
  df_models <- as.data.frame(df_models)
  df_models$variable <- row.names(df_models)
  df_models$variable <- ifelse(
    df_models$variable == "(Intercept)", "intercept", df_models$variable)
  row.names(df_models) <- NULL
  
  # Clean names
  names(df_models) <- stringr::str_to_lower(names(df_models))
  names(df_models) <- stringr::str_replace_all(names(df_models), " ", "")
  
  # Add tau variable
  tau <- sapply(models_summary, function(x) x$tau)
  tau <- rep(tau, each = 2)
  df_models$tau <- tau
  
  # Arrange
  df_models <- threadr::arrange_left(df_models, c("tau", "variable"))
  
  # Round
  df_models <- threadr::round_numeric(df_models, round = round)
  
  return(df_models)
  
}
