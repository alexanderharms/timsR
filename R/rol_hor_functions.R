rol_hor_model_loop <- function(target_series, MODEL_VECTOR, STARTTEST, 
                               N_TEST, H, FREQ,
                               regressors = NULL) {
  
  # Initialiseer de data frames voor de metrieken
  # Initialize data frames for the metrics
  metrics <- data.frame("Model name" = c(), 
                        "ME" = c(), "RMSE" = c(), "MAE" = c(), 
                        "MPE" = c(), "MAPE" = c())
  if (check_aggr()) {
    metrics_aggr <- data.frame("Model name" = c(), 
                               "ME" = c(), "RMSE" = c(), "MAE" = c(), 
                               "MPE" = c(), "MAPE" = c())
  } else {
    metrics_aggr <- NULL
  }

  error_models <- c()
  
  # Loop over the models in MODEL_VECTOR
  for (model_name in MODEL_VECTOR) {
    print(paste0("Model: ", model_name))
    
    # For every model a train- and a prediction-function should be defined. They
    # should start with "train_" and "pred_" respectively.
    train_fun <- paste0("train_", model_name)
    pred_fun  <- paste0("pred_", model_name)
    
    metrics_list <- NULL
    
    # If the models don't converge, they are added to the error_models vector.
    # In this way the loop can continue.
    metrics_list <- tryCatch({
      # In rol_hor_list the metrics, metrics_aggr, plot_data and the models are
      # contained.
      rol_hor_list <- rolling_horizon(target_series, FUN = train_fun, 
                                      FUN_PRED = pred_fun,
                                      start_eval = STARTTEST, 
                                      eval_periods = N_TEST,
                                      h = H, regressors = regressors)
      
      plot_rol_hor_list(rol_hor_list, model_name, freq = FREQ, zoom = TRUE, 
                        target_series = target_series)
      
      # The metrics are stored as factors in the data frames. They have to be
      # converted to characters and then to numeric.
      metrics <- rbind(metrics, cbind(model_name, rol_hor_list$metrics), 
                       make.row.names = FALSE) %>%
        mutate(ME = ME %>% as.character() %>% as.numeric(),
               RMSE = RMSE %>% as.character() %>% as.numeric(),
               MAE = MAE %>% as.character() %>% as.numeric(),
               MPE = MPE %>% as.character() %>% as.numeric(),
               MAPE = MAPE %>% as.character() %>% as.numeric())
      if (check_aggr()) {
        metrics_aggr <- rbind(metrics_aggr, cbind(model_name, rol_hor_list$metrics), 
                         make.row.names = FALSE) %>%
          mutate(ME = ME %>% as.character() %>% as.numeric(),
                 RMSE = RMSE %>% as.character() %>% as.numeric(),
                 MAE = MAE %>% as.character() %>% as.numeric(),
                 MPE = MPE %>% as.character() %>% as.numeric(),
                 MAPE = MAPE %>% as.character() %>% as.numeric())
      } 
      
      metrics_list <- list("metrics" = metrics,
                           "metrics_aggr" = metrics_aggr,
                           "error_models" = error_models)
    },
    # If the rolling horizon gives an error, add the model name to the vector
    # of error_models.
    error = function(cond) {
      error_models <- c(error_models, model_name)
      print(cond) # Print the error
      return(list("metrics" = metrics, 
                  "metrics_aggr" = metrics_aggr, 
                  "error_models" = error_models))
    })
    
    if (!is.null(metrics_list)) {
      metrics <- metrics_list$metrics
      metrics_aggr <- metrics_list$metrics_aggr
      error_models  <- metrics_list$error_models
    }
  }
  
  return(list("metrics" = metrics, 
              "metrics_aggr" = metrics_aggr, 
              "error_models" = error_models))
}


#' Rolling horizon
#' 
#' Test time series models with the rolling horizon method.
#' 
#'@param series Time series of the target variable.
#'@param FUN Training function, returns a model-object.
#'@param FUN_PRED Prediction function, returns a prediction.
#'@param start_eval Vector in the shape c(year, period) that form the last
#' point of the first rolling horizon.
#'@param eval_periods Number of rolling horizons.
#'@param h Horizon length.
#'  
#'@return A list of the metrics, data for plotting and the trained models.
#'  
#'@export
rolling_horizon <- function(series, FUN, FUN_PRED, start_eval, 
                            eval_periods = 4, h = 14,regressors = NULL) {
  # If FUN or FUN_PRED are strings, then the corresponding functions are
  # gathered.
  if (is.character(FUN)) FUN <- get(FUN)
  if (is.character(FUN_PRED)) FUN_PRED <- get(FUN_PRED)
  
  nowcasts <- data.frame("actual" = c(), 
                         "prediction" = c())
  if (check_aggr()) {
    nowcasts_aggr <- data.frame("actual" = c(), 
                                "prediction" = c())
  }
  
  plot_data <- list()
  trained_models <- list()
  for (i in 0:(eval_periods - 1)) {
    # The -1 makes sure that the start_eval-date is taken as the first value for
    # i = 0.
    # tsp(series)[3] is the frequency of the series
    enddate <- start_eval[1] + (start_eval[2] + h - 1 + i)/tsp(series)[3]
    series_nieuw <- window(series, end = enddate)
    actual <- series_nieuw[(length(series_nieuw) - h + 1):length(series_nieuw)]
    series_nieuw[(length(series_nieuw) - h + 1):length(series_nieuw)] <- NA

    if (!is.null(regressors)) { 
        reg_train <- window(regressors, end = (enddate - h/tsp(series)[3]))
        
        reg_pred <- window(regressors, start = (enddate - (h - 1)/tsp(series)[3]),
                           end = enddate)
    }
    
    # Train the model.
    if (!is.null(regressors)){
        trained_model <- FUN(series_nieuw, regressors = reg_train)
    } else {
        trained_model <- FUN(series_nieuw)
    }
    trained_models[[i+1]] <- trained_model
    
    # FUN_PRED returns a data frame with the prediction and the 95% confidence
    # interval. 
    if (!is.null(regressors)){
        return_df <- trained_model %>% FUN_PRED(h, regressors = reg_pred)
    } else {
        return_df <- trained_model %>% FUN_PRED(h)
    }
    prediction <- return_df$prediction
    
    actual_pred <- cbind(actual, prediction)
    
    nowcasts <- rbind(nowcasts, actual_pred)

    if (check_aggr()) {
      actual_aggr <- aggr_fun(actual)
      prediction_aggr <- aggr_fun(actual)
      actual_pred_aggr <- cbind(actual_aggr, prediction_aggr)
      nowcasts_aggr <- rbind(nowcasts_aggr, actual_pred_aggr)
    } 
    
    # Accompany plot_data with time information to later plot it as a time
    # series.
    date_vector <- enddate +
      seq(-(h - 1), 0, length.out = h)/tsp(series)[3]
    plot_data[[i+1]] <- cbind(actual, return_df, date_vector)
  }
  
  # Calculate the metrics based on the actual value and the prediction
  metrics <- accuracy(nowcasts[, 'actual'], 
                      nowcasts[, 'prediction'])
  if (check_aggr()) {
    metrics_aggr <- accuracy(nowcasts_aggr[, 'actual'], 
                             nowcasts_aggr[, 'prediction'])
  } else {
    metrics_aggr <- NULL
  }
  
  return(list("metrics" = metrics, 
              "metrics_aggr" = metrics_aggr, 
              "plot_data" = plot_data,
              "models" = trained_models))
}

