
# Prepare time series  -------------------------------------------------------

#' Prepare time series
#'
#' Reads in a data frame with columns for the target variable(s) and the
#' regressors; returns a list with the time series for the main target variable,
#' any extra target variables and the regressors.
prepare_timeseries <- function(data, tims_object) {
  
  return(tims_object)
}

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
