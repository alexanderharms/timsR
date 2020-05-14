generate_time_vector <- function(startdate, stepsize, vec_length){
  steps_vec <- seq(0, vec_length - 1, length.out = vec_length)
  if (stepsize == "quarters"){
      stepsize <- "months"
      steps_vec <- steps_vec * 3
  }
  time_vec <- sapply(steps_vec, function(step, start, unit) {
                       as.character(start + lubridate::period(step, units=unit))
                       }, start=startdate, unit=stepsize)
  return(time_vec)
}
# Prepare time series  -------------------------------------------------------

#' Prepare time series
#'
#' Reads in a data frame with columns for the target variable(s) and the
#' regressors; returns a list with the time series for the main target variable,
#' any extra target variables and the regressors.
prepare_timeseries <- function(data, STARTDATA, STARTMODEL, FREQ, 
                               TARGET_VAR = NULL,
                               REGCOLUMNS = NULL) {
  # Define a vector of dates from STARTDATA, with length length(data) and 
  # stepsize FREQ.
  startdata <- lubridate::ymd_hms(STARTDATA)
  startmodel <- lubridate::ymd_hms(STARTMODEL)
  time_vec <- generate_time_vector(startdata, FREQ, nrow(data))

  timeseries <- xts(data, order.by=as.Date(time_vec), stepsize=FREQ) %>%
      window(start = startmodel)
  
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
  
  return(list("target_series" = target_series,
              "regressors" = regressors,
              "extra_target_series" = extra_target_series,
              "stepsize" = FREQ))
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

