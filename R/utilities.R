# Prepare time series  -------------------------------------------------------

#' Prepare time series
#'
#' Reads in a data frame with columns for the target variable(s) and the
#' regressors; returns a list with the time series for the main target variable,
#' any extra target variables and the regressors.
prepare_timeseries <- function(data, STARTDATA, STARTMODEL, FREQ, 
                               TARGET_VAR = NULL,
                               REGCOLUMNS = NULL,
                               plot_timeseries = FALSE) {
  # Define a vector of dates from STARTDATA, with length length(data) and 
  # stepsize FREQ.
  #time_vec <- STARTDATA
  #timeseries <- xts(data, order.by=time_vec) %>%
  # Trims the time series to STARTMODEL.

  timeseries <- ts(data, start = STARTDATA, frequency = FREQ,
                  names = names(data)) %>%
    window(start = STARTMODEL)
  
  if (is.null(TARGET_VAR)) {
    target_series <- NULL
    extra_target_series <- NULL
  } else {
    # The first item in the vector TARGET_VAR becomes the first target series,
    # the other items will be stored as extra_target_series.
    target_series <- timeseries[, TARGET_VAR[1]]
    if (length(TARGET_VAR) > 1) {
      extra_target_series <- timeseries[, TARGET_VAR[2:length(TARGET_VAR)]]
    } else {
      extra_target_series <- NULL
    }
  }
  
  if (is.null(REGCOLUMNS)) {
    regressors <- NULL
  } else {
    regressors <- timeseries[, REGCOLUMNS]
  }
  
  if (plot_timeseries) {
    ts.plot(target_series, gpars = list(main = "Target series"))
    
    if (!is.null(regressors)) {
      ts.plot(regressors, gpars = list(main = "Regressors"))
    }
  }
  
  return(list("target_series" = target_series,
              "regressors" = regressors,
              "extra_target_series" = extra_target_series))
}

plot_rol_hor_list <- function(rol_hor_list, plot_title, freq,
                              zoom = TRUE, target_series = NULL){
  if (zoom == FALSE & is.null(target_series)) {
    stop("With zoom = FALSE a target series needs to be defined.")
  }
  
  for (item in rol_hor_list$plot_data) {
    item_ts <- ts(item, start = item[, 'date_vector'][1],
                  end = item[, 'date_vector'][nrow(item)],
                  frequency = freq)
    
    if (zoom == FALSE) {
      reeks <- target_series
    } else {
      reeks <- item_ts[, 'actual']
    } 
    
    ts.plot(reeks, item_ts[, 'prediction'], 
            item_ts[, 'lower_conf'], item_ts[, 'upper_conf'], 
            gpars=list(main = plot_title,
                       col = c('black', 'red', 'blue', 'blue')))
  }
}

