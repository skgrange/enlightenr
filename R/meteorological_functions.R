#' Function to prepare meteorological and air quality data for modelling. 
#' 
#' @author Stuart K. Grange
#' 
#' @return A named list containing two data frames. 
#' 
#' @export
prepare_input_data <- function(df) {
  
  # Check input
  names <- names(df)
  
  if (!any(grepl("value", names))) 
    stop("Data must contain a `value`` variable.", call. = FALSE)
  
  if (!any(grepl("date", names))) 
    stop("Data must contain a `date`` variable.", call. = FALSE)
  
  if (!any(grepl("POSIXct", class(df$date))))
    stop("`date` variable needs to be a parsed date (POSIXct).", call. = FALSE)
  
  # Add date variables
  if (!any(grepl("date_unix", names))) 
    df[, "date_unix"] <- as.numeric(df[, "date"])
  
  if (!any(grepl("week", names)))
    df[, "week"] <- lubridate::week(df[, "date"])
  
  if (!any(grepl("weekday", names)))
    df[, "weekday"] <- threadr::wday_monday(df[, "date"])
  
  if (!any(grepl("hour", names)))
    df[, "hour"] <- lubridate::hour(df[, "date"])
  
  # Impute numeric variables
  index <- sapply(df, function (x) is.numeric(x) | is.integer(x))
  
  # Median
  df[index] <- lapply(df[index], function(x) 
    ifelse(is.na(x), median(x, na.rm = TRUE), x))
  
  # Sample to create test and training data
  random_rows <- random_rows(df)
  df_training <- df[random_rows, ]
  df_testing <- df[-random_rows, ]
  
  # Create list
  list_data <- list(
    training = df_training,
    testing = df_testing
  )
  
  class(list_data) <- "ssmodel_data"
  
  return(list_data)
  
}


#' Function to model concentration based on meteorological and time variables. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list containing two data frames and a model object. 
#' 
#' @export
calculate_model <- function(list_input_data, variables = NA, ntree = 200,
                            verbose = TRUE) {
  
  if (!class(list_input_data) == "ssmodel_data") 
    stop("Not of correct class.", call. = FALSE)
  
  # Get pieces
  df_training <- list_input_data$training
  df_testing <- list_input_data$testing
  
  if (is.na(variables[1])) 
    variables <- c("temp", "rh", "ws", "wd", "date_unix", "week", "weekday", "hour")
  
  variables <- c("value", variables)  
  
  # Select the variables
  df_training <- df_training[, variables]
  df_testing <- df_testing[, variables]
  
  # For rf progress
  do.trace <- ifelse(verbose, 2, FALSE)
  
  # Model
  list_model <- randomForest::randomForest(
    value ~ ., 
    data = df_training,
    na.action = na.omit,
    do.trace = do.trace, 
    keep.forest = TRUE, 
    importance = TRUE,
    mtry = 5, 
    nodesize = 1, 
    ntree = ntree
  )
  
  # Build return
  list_model <- list(
    data_training = df_training,
    data_testing = df_testing,
    model = list_model
  )
  
  # Give class
  class(list_model) <- "ssmodel_model"
  
  return(list_model)
  
}


#' Function to normalise a concentration variable based on "average" 
#' meteorological conditions. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @export
normalise_for_meteorology <- function(list_model, df, variables = NA, n = 100) {
  
  if (is.na(variables[1])) 
    variables <- c("wd", "ws", "temp", "rh", "hour", "weekday", "week")
  
  # Do in parallel
  df <- plyr::ldply(1:n, function(x) 
    randomly_sample_meteorology(list_model, df, variables),
    .parallel = TRUE) %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(value_predict = mean(value_predict, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  # Free
  gc()
  
  return(df)
  
}


# No export
randomly_sample_meteorology <- function(list_model, df, 
                                        variables = c("wd", "ws", "temp"),
                                        replace = FALSE) {
  
  # Randomly sample observations
  n_rows <- nrow(df)
  index_rows <- sample(1:n_rows, replace = replace)
  
  # Transform data frame to include sampled variables
  df[variables] <- lapply(df[variables], function(x) x[index_rows])
  
  value_predict <- unname(predict(list_model, df))
  
  if (class(value_predict) == "matrix") value_predict <- value_predict[, 1]
  
  # Build data frame of predictions
  df <- data.frame(
    date = df$date,
    value_predict = value_predict
  )
  
  return(df)
  
}


# from deweather package
## randomly sample from original data
# doPred <- function(mydata, mod, metVars) {
#   
#   ## random samples 
#   n <- nrow(mydata) 
#   id <- sample(1 : n, n, replace = FALSE)
#   
#   ## new data with random samples
#   mydata[metVars] <- lapply(mydata[metVars], function (x) x[id])
#   
#   prediction <- predict.gbm(mod, mydata, 1000)
#   prediction <- data.frame(date = mydata$date, pred = prediction)
#   
#   return(prediction)
#   
# }



#' Function to detect breakpoints in a data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.
#' 
#' @export
detect_breakpoints <- function(df) {
  
  breakpoints <- strucchange::breakpoints(value ~ date, data = df)
  df <- df[breakpoints$breakpoints, ]
  df
  
}
