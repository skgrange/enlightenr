#' Function to decompose time-series with loess. 
#' 
#' @author Stuart K. Grange
#' 
#' @import ggplot2
#' 
#' @export
decompose_stl <- function(df, variable = "value", plot = TRUE, 
                          invalidate = TRUE) {
  
  # Aggregate/pad
  df <- openair::timeAverage(df, avg.time = "month")
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Ensure full years are present
  df <- threadr::time_pad(df, "month", round = "year")
  
  # Find the date ranges
  date_start <- min(df$date)
  date_end <- max(df$date)
  
  # Pick out integers
  start_year <- min(year(date_start))
  start_month <- min(month(date_start))
  end_year <- max(year(date_end))
  end_month <- max(month(date_end))
  
  # No missing data allowed in the function, this will push values backwards too
  df[, variable] <- approx(df[, variable], n = length(df[, variable]))$y
  
  # Create time series object, monthly
  time_series <- ts(df[, variable], start = c(start_year, start_month), 
                    end = c(end_year, end_month), frequency = 12)
  
  # Decompose with loess, use openair defaults
  decomposition <- stl(time_series, s.window = 35, robust = TRUE, s.degree = 0)
  
  # Build tidy data frame
  # Create the output
  df_decomposition <- data.frame(
    date = df$date,
    data = as.numeric(time_series),
    decomposition$time.series
  )
  
  # Transform some variables
  df_decomposition$deseason <- df_decomposition$trend + df_decomposition$remainder
  df_decomposition <- threadr::add_row_numbers(df_decomposition)
  
  if (invalidate) {
    
    # Invalidate start and end pieces
    df_decomposition[1:6, -1:-3] <- NA
    
    index <- nrow(df_decomposition)
    index_start <- index - 6
    df_decomposition[index_start:index, -1:-3] <- NA
    
  }
  
  if (plot) {
    
    # Reshape
    df_decomposition_long <- tidyr::gather(df_decomposition, key, value, 
                                           -row_number, -date)
    
    # Order facets
    df_decomposition_long$key <- factor(df_decomposition_long$key, 
      levels = c("data", "seasonal", "trend", "remainder", "deseason"))
    
    # Plot
    plot <- ggplot(df_decomposition_long, aes(date, value, colour = key)) + 
      geom_line() + facet_wrap("key", scales = "free_y", ncol = 1) + 
      theme(legend.position = "none") + theme_minimal()
    
    print(plot)
    
  }
  
  # Return, only if assigned
  invisible(df_decomposition)
  
}


#' Function to decompose time-series with Kolmogorov-Zurbenko filters.  
#' 
#' @author Stuart K. Grange
#' 
#' @export
decompose_kz <- function(df, variable = "value", plot = TRUE, invalidate = TRUE) {
  
  # Aggregate/pad
  df <- openair::timeAverage(df, avg.time = "day")
  
  # Drop tbl_df
  df <- threadr::base_df(df)
  
  # Create timeseries
  time_series <- ts(df[, variable])
  
  # Apply filters, options from Wise2005
  # Works for daily data
  baseline <- kza::kz(time_series, m = 15, k = 5)
  
  # 
  trend <- kza::kz(time_series, m = 365, k = 3)
  
  # Build data frame
  df_decomposition <- data.frame(
    date = df$date,
    data = df[, variable],
    baseline = as.numeric(baseline),
    trend = as.numeric(trend)
  )
  
  # Transform and add variables
  df_decomposition$data_no_trend <- df_decomposition$data - df_decomposition$trend
  
  df_decomposition$short_term <- df_decomposition$data - df_decomposition$baseline
  
  df_decomposition$remainder <- df_decomposition$data - 
    (df_decomposition$trend + df_decomposition$data_no_trend)
  
  df_decomposition <- threadr::add_row_numbers(df_decomposition)
  
  if (invalidate) {
    
    # Invalidate start and end pieces
    df_decomposition[1:180, -1:-3] <- NA
    
    index <- nrow(df_decomposition)
    index_start <- index - 180
    df_decomposition[index_start:index, -1:-3] <- NA
    
  }
  
  if (plot) {
    
    # Reshape
    df_decomposition_long <- tidyr::gather(df_decomposition, key, value, 
                                           -row_number, -date)
    
    # Order facets
    df_decomposition_long$key <- factor(df_decomposition_long$key, 
      levels = c("data", "baseline", "trend", "data_no_trend", "remainder", "short_term"))
    
    # Plot
    plot <- ggplot(df_decomposition_long, aes(date, value, colour = key)) + 
      geom_line(na.rm = TRUE) + facet_wrap("key", scales = "free_y", ncol = 1) + 
      theme(legend.position = "none") + theme_minimal()
    
    print(plot)
    
  }
  
  # Return, only if assigned
  invisible(df_decomposition)
  
}
