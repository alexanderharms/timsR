# Prepare time series  -------------------------------------------------------

#' Prepare time series
#'
#' Reads in a data frame with columns for the target variable(s) and the
#' regressors; returns a list with the time series for the main target variable,
#' any extra target variables and the regressors.
prepare_timeseries <- function(tims_object) {
  # Trim timeseries to the start of modeling
  print(tims_object$start_data)
  print(tims_object$start_model)
  timeseries <- ts(tims_object$dataset, start = tims_object$start_data, 
                   frequency = tims_object$frequency, 
                   names = names(tims_object$dataset)) %>%
    window(start = tims_object$start_model)
  
  tims_object$target_series <- timeseries[, tims_object$target_var]
  tims_object$reg_series <- timeseries[, tims_object$reg_var]
  print("Extracted target and regressor series; setting dataset to NULL.")
  tims_object$dataset <- NULL
  
  return(tims_object)
}

calculate_horizon_dates <- function(tims_object, hor_num) {
  hor_num <- hor_num - 1
  horizon_spacing <- 1
  start_horizon <- tims_object$start_test[1] + 
    (tims_object$start_test[2] - 1 + hor_num * horizon_spacing)/tims_object$frequency
  end_horizon <- start_horizon + tims_object$horizon/tims_object$frequency
  start_training <- tims_object$start_model
  end_training <- start_horizon - 1/tims_object$frequency
  
  tims_object$start_training <- tims_object$start_model
  tims_object$end_training <- end_training
  tims_object$start_horizon <- start_horizon
  tims_object$end_horizon <- end_horizon
  
  return(tims_object)
}

get_train_data <- function(tims_object, hor_num) {
  tims_object <- calculate_horizon_dates(tims_object, hor_num)
  
  tims_object$target_train <- tims_object$target_series %>%
    window(start=tims_object$start_training,
           end=tims_object$end_training)
  tims_object$reg_train <- tims_object$reg_series %>%
    window(start=tims_object$start_training,
           end=tims_object$end_training)
  
  return(tims_object)
}

get_test_data <- function(tims_object, hor_num) {
  tims_object <- calculate_horizon_dates(tims_object, hor_num)
  
  tims_object$target_test <- tims_object$target_series %>%
    window(start=tims_object$start_horizon,
           end=tims_object$end_horizon)
  tims_object$reg_test <- tims_object$reg_series %>%
    window(start=tims_object$start_horizon,
           end=tims_object$end_horizon)
  return(tims_object)
}