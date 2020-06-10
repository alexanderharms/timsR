new_tims <- function(settings_list, dataset) {
  # Define quantities related to the time series
  # Startmodel, starttest, horizon.

  # Get the function names for the train and prediction 
  # functions. 
  train_funcs <- list()
  pred_funcs <- list()
  model_names <- settings_list$model_names
  for (idx in 1:length(model_names)) {
    train_funcs[[idx]] <- get(paste0("train_", model_names[idx]))
    pred_funcs[[idx]] <- get(paste0("pred_", model_names[idx]))
  }

  if (!is.null(settings_list$num_tests)) {
    metrics <- data.frame("Model name" = c(), 
                          "ME" = c(), "RMSE" = c(), "MAE" = c(), 
                          "MPE" = c(), "MAPE" = c())
    error_models <- c()
  } else {
    metrics <- NULL
    error_models <- NULL
  }
  
  metrics_aggr <- NULL

  # if (!is.null(aggregate_fun)) {
  #   metrics_aggr <- data.frame("Model name" = c(), 
  #                              "ME" = c(), "RMSE" = c(), "MAE" = c(), 
  #                              "MPE" = c(), "MAPE" = c())
  #   if (is.character(aggregate_fun)) aggregate_fun <- get(aggregate_fun)
  # } else {
  #   metrics_aggr <- NULL
  # }

  tims_list <- list(
    "dataset" = dataset,
    "logfile" = settings_list$logfile,
    "target_var" = settings_list$target_var,
    "reg_var" = settings_list$reg_var,
    "frequency" = settings_list$frequency,
    "model_names" = model_names, 
    "start_data" = settings_list$start_data,
    "start_model" = settings_list$start_model, 
    "start_test" = settings_list$start_test,  
    "train_funcs" = train_funcs, 
    "pred_funcs" = pred_funcs,  
    "horizon" = settings_list$horizon, 
    "num_tests" = settings_list$num_tests,  
    # "horizon_spacing" = horizon_spacing,  
    # "aggregate_fun" = aggregate_fun,  
    "metrics" = metrics,  
    "metrics_aggr" = metrics_aggr,  
    "error_models" = error_models,
    "trained_models" = list(list()))
  class(tims_list) <- "tims"
  return(tims_list)
}

tims <- function(settings_list, dataset) {
  
  return(new_tims(settings_list, dataset))
}
