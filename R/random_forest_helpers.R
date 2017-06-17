#' Function to plot partial dependence plots for a \code{randomForest} object.
#'
#' @param list_model A \code{randomForest} object.
#' 
#' @param df Data frame to predict. 
#' 
#' @param variable Variable names to predict. 
#'
#' @author Stuart K. Grange
#' 
#' @import ggplot2
#' 
#' @return Invisible.
#' 
#' @seealso \code{\link{randomForest}}, \code{\link{importance_tidy}}, 
#' \code{\link{ggimportance}}
#'
#' @export
ggpartial <- function(list_model, df, variable, reorder = TRUE) {
  
  # For variable length
  df_partial <- plyr::ldply(variable, function(x) 
    partial_plot_predict(list_model, df, x))
  
  # Categorical variables
  if (class(df_partial$x) %in% c("character", "factor") & reorder) {
      
      plot <- ggplot(df_partial, aes(reorder(x, y), y, colour = reorder(x, y))) + 
        geom_point(size = 4) + theme_minimal() + theme(legend.position = "none") +
        facet_wrap("variable", scales = "free_x") +
        viridis::scale_colour_viridis(option = "inferno", begin = 0.3, end = 0.8,
                                      discrete = TRUE)
      

  } else {
    
    # For factors
    if (!is.numeric(df_partial[, "x"])) 
      df_partial[, "x"] <- as.numeric(df_partial[, "x"])
    
    plot <- ggplot(df_partial, aes(x, y, colour = y)) + 
      geom_point(size = 4) + theme_minimal() + theme(legend.position = "none") + 
      facet_wrap("variable", scales = "free_x") +
      viridis::scale_colour_viridis(option = "inferno", begin = 0.3, end = 0.8)
    
  }
  
  # Return
  plot
  
}


#' Function to calculate partial dependencies from a \code{randomForest} model. 
#' 
#' @param list_model A \code{randomForest} model. 
#' 
#' @param df Data frame to be used for prediction. 
#' 
#' @param variable The variable to calculate the partial dependency of. 
#' 
#' @param as.character Should the variables be forced to be a character data 
#' type? This is helpful when binding many returns from 
#' \code{partial_plot_predict}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @export
partial_plot_predict <- function(list_model, df, variable, as.character = FALSE) {
  
  # Check
  if (!length(variable) == 1) 
    stop("Only one 'variable' can be used", call. = FALSE)
  
  # No nas, for numeric variables I think
  df <- df[!is.na(df[, variable]), ]
  
  # Call the function, eval issues here
  list_predict <- do.call(getFromNamespace("partialPlot", "randomForest"), 
    list(x = list_model, pred.data = df, x.var = variable, plot = FALSE))
  
  # Add identifier
  df <- cbind(variable, data.frame(list_predict))
  
  # Force variables to be characters, useful for binding data frames
  if (as.character) df[, 2:3] <- lapply(df[, 2:3], as.character)
  
  return(df)
  
}


#' Function to tidy the \code{importance} matrix for a \code{randomForest} object.
#'
#' @param list_model A \code{randomForest} object.
#' 
#' @param scale Should the importance variables be scaled?
#' 
#' @return Data frame. 
#'
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{randomForest}}, \code{\link{ggimportance}}
#'
#' @export
importance_tidy <- function(list_model, scale = TRUE) {
  
  # Get things
  matrix <- randomForest::importance(list_model, scale = scale)
  colnames(matrix) <- stringr::str_replace(colnames(matrix), "%", "")
  names <- row.names(matrix)
  
  df <- data.frame(
    variable = names,
    matrix,
    stringsAsFactors = FALSE
  )
  
  row.names(df) <- NULL
  
  # Add variables if they do no exist
  if (!any(grepl("IncNodePurity", names(df)))) df$IncNodePurity <- NA
  if (!any(grepl("IncMSE", names(df)))) df$IncMSE <- NA
  
  # Clean names
  names(df) <- ifelse(
    names(df) == "IncNodePurity", "increase_in_node_purity", names(df))
  
  names(df) <- ifelse(names(df) == "IncMSE", "increase_in_mse", names(df))
  
  # Order by importance
  df <- dplyr::arrange(df, -increase_in_mse)
  
  return(df)
  
}


#' Function to plot the \code{increase_in_mse} variable for a \code{randomForest} 
#' object.
#'
#' @param list_model  A \code{randomForest} object.
#'
#' @author Stuart K. Grange
#' 
#' @import ggplot2
#' 
#' @return Invisible.
#' 
#' @seealso \code{\link{randomForest}}, \code{\link{importance_tidy}}
#'
#' @export
ggimportance <- function(list_model) {
  
  # Tidy output
  df <- importance_tidy(list_model)
  
  # To percent
  df$increase_in_mse <- df$increase_in_mse / 100
  
  plot <- ggplot(df, aes(increase_in_mse, reorder(variable, increase_in_mse), 
                         colour = increase_in_mse)) + 
    geom_point(size = 4) + theme_minimal() + 
    geom_segment(aes(x = 0, y = variable, xend = increase_in_mse, yend = variable)) + 
    viridis::scale_colour_viridis(option = "inferno", begin = 0.3, end = 0.8) +
    ylab("Variable") + theme(legend.position = "none") + 
    scale_x_continuous(labels = scales::percent)
  
  return(plot)
  
}


#' Function to plot the MSE and number of trees for a \code{randomForest} object.
#'
#' @param list_model A \code{randomForest} object.
#' 
#' @return Invisible.
#' 
#' @import ggplot2
#'
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{randomForest}}
#'
#' @export
ggtrees <- function(list_model) {
  
  mode <- list_model$type
  
  if (mode == "regression") {
    
    # Get vectors
    error <- list_model$mse
    trees <- seq(1, length(error), 1)
    
    # Build data frame
    df <- data.frame(
      trees,
      error
    )
    
    # Plot
    plot <- ggplot(df, aes(trees, error, colour = error)) + geom_line(size = 1) + 
      viridis::scale_colour_viridis(option = "inferno", begin = 0.3, end = 0.8) + 
      theme_minimal() + theme(legend.position = "none") + ylab("MSE") + 
      xlab("Trees")
    
  }
  
  if (mode == "classification") {
    
    # Get matrix
    matrix_error <- list_model$err.rate
    
    # Build data frame
    df <- data.frame(matrix_error)
    df$trees <- seq(1, nrow(df), 1)
    df <- tidyr::gather(df, variable, value, -trees)
    
    # Plot
    plot <- ggplot(df, aes(trees, value, colour = variable)) + geom_line(size = 1) + 
      viridis::scale_colour_viridis(option = "inferno", begin = 0.3, end = 0.8,
                                    discrete = TRUE) +
      theme_minimal() + ylab("Error") + xlab("Trees")
    
  }
  
  return(plot)
  
}


#' @export
extract_rf_mse <- function(list_model, na.rm = FALSE)
  min(list_model$mse, na.rm = na.rm)


#' @export
extract_rf_r_squared <- function(list_model, na.rm = FALSE)
  max(list_model$rsq, na.rm = na.rm) * 100
