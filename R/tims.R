new_tims <- function(model_names = character(), startmodel, 
		     starttest, horizon = integer(), 
		     num_tests = integer(), 
		     horizon_spacing, aggregate_fun = NULL){
  
  # Define quantities related to the time series
  # Startmodel, starttest, horizon.
  # If test is TRUE: num_tests, horizon_spacing

  # Get the function names for the train and prediction 
  # functions. 
  train_funcs <- list()
  pred_funcs <- list()
  for (idx in 1:length(model_names)) {
    train_funcs[[idx]] <- get(paste0("train_", model_names[idx]))
    pred_funcs[[idx]] <- get(paste0("pred_", model_names[idx]))
  }

  if (!is.null(num_tests)) {
    metrics <- data.frame("Model name" = c(), 
                          "ME" = c(), "RMSE" = c(), "MAE" = c(), 
                          "MPE" = c(), "MAPE" = c())
    error_models <- c()
  } else {
    metrics <- NULL
    error_models <- NULL
  }

  if (!is.null(aggregate_fun)) {
    metrics_aggr <- data.frame("Model name" = c(), 
                               "ME" = c(), "RMSE" = c(), "MAE" = c(), 
                               "MPE" = c(), "MAPE" = c())
    if (is.character(aggregate_fun)) aggregate_fun <- get(aggregate_fun)
  } else {
    metrics_aggr <- NULL
  }

  tims_list <- list("model_names" = model_names, 
		    "startmodel" = startmodel, 
		    "starttest" = starttest, 
		    "train_funcs" = train_funcs,
		    "pred_funcs" = pred_funcs,
                    "trained_models" = list(list()),
		    "horizon" = horizon,
		    "num_tests" = num_tests,
		    "hor_spacing" = horizon_spacing,
		    "aggregate_fun" = aggregate_fun,
		    "metrics" = metrics,
		    "metrics_aggr" = metrics_aggr,
		    "error_models" = error_models)
  class(tims_list) <- "tims"
  return(tims_list)
}

tims <- function(model_names, startmodel, starttest, horizon,
	 num_tests = NULL, horizon_spacing = NULL, 
	 aggregate_fun = NULL) {
	
  startmodel <- as.Date(startmodel)
  starttest  <- as.Date(starttest)
  return(new_tims(model_names, startmodel, starttest, horizon,
            		  num_tests, horizon_spacing, aggregate_fun))
}


# Plot a test with a certain index
plot.tims <- function(tims_obj, timeseries, model_idx, hor_idx) {
    trained_model <- tims_obj$trained_models[[model_idx]][[hor_idx+1]]
    pred_fun <- tims_obj$pred_funcs[[model_idx]]
    
    hor_dates <- calculate_horizon_dates(tims_obj, timeseries$stepsize, hor_idx)
    timeseries <- calculate_test_series(timeseries, tims_obj, hor_dates)
    
    prediction_df <- predict_model(tims_obj, timeseries, model_idx, hor_idx)
    time_vec <- generate_time_vector(hor_dates$start_hor, timeseries$stepsize, 
                                     nrow(prediction_df))
    prediction_df <- prediction_df %>%
      xts(order.by = as.Date(time_vec)) 
      ggplot2::fortify()
    target_series <- timeseries$target_series %>% ggplot2::fortify()
    colnames(target_series) <- c("Index", "target")
    
    ggplot2::ggplot() +
      ggplot2::geom_line(target_series, mapping=aes(x=Index, y=target)) +
      ggplot2::geom_line(prediction_df, mapping=aes(x=Index, y=prediction),
                         colour = "red") + 
      ggplot2::geom_line(prediction_df, mapping=aes(x=Index, y=lower_conf),
                         colour = "blue") + 
      ggplot2::geom_line(prediction_df, mapping=aes(x=Index, y=upper_conf),
                         colour = "blue") 
}

# Logs data and results
logging <- function(obj, log_filename) {
  UseMethod("logging")
}

logging.default <- function(obj, log_filename) {
  print("Use the 'tims' method")
}

logging.tims <- function(obj, log_filename) {
  sink(log_filename, append = TRUE)
  print("Start of training set:")
  print(obj$startmodel)
  print("Start of test set:")
  print(obj$starttest)
  print("Horizon:")
  print(obj$horizon)
  print("N_TEST:")
  print(obj$num_tests)
  print("Model vector:")
  print(obj$model_names) 
  print("Metrics:")
  print(obj$metrics)
  if (!is.null(obj$aggregate_fun)) {
    print("Metrics, aggregated:")
    print(obj$metrics_aggr)
  }
  print("Error models:")
  print(obj$error_models)
  sink()
}

sort_metrics <- function(obj, ...) {
  UseMethod("sort_metrics")
}

sort_metrics.default <- function(obj, ...) {
  print("Use the 'tims' method")
}

sort_metrics.tims <- function(obj, sort_tag) {
  sort_tag <- rlang::enquo(sort_tag)
  obj$metrics <- obj$metrics %>% dplyr::arrange(!! sort_tag)
  if (!is.null(obj$aggregate_fun)) {
    obj$metrics_aggr <- obj$metrics_aggr %>% dplyr::arrange(!! sort_tag)
  }
  return(obj)
}

export <- function(obj, ...) {
  UseMethod("export")
}

export.default <- function(obj, ...) {
  print("Use the 'tims' method")
}

export.tims <- function(obj, log_filename) {
  csv_filename <- substr(log_filename, start = 1, 
			 stop = nchar(log_filename) - 4) %>% paste0(".csv")
  csv_filename_aggr <- substr(log_filename, start = 1, 
			      stop = nchar(log_filename) - 4) %>% paste0("_aggr.csv")
  
  write.csv(obj$metrics, file = csv_filename, row.names = FALSE)
  if (!is.null(obj$aggregate_fun)) {
    write.csv(obj$metrics_aggr, file = csv_filename_aggr, row.names = FALSE)
  }
}

