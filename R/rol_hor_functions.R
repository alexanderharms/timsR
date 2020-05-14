rol_hor_model_loop <- function(timeseries, tims_obj) {

  # Loop over the models in MODEL_VECTOR
  for (model_idx in 1:length(tims_obj$model_names)) {
    model_name <- tims_obj$model_names[model_idx]
    print(paste0("Model: ", model_name))
    
    # If the models don't converge, they are added to the error_models vector.
    # In this way the loop can continue.
   tims_obj <- tryCatch({
     # In rol_hor_list the metrics, metrics_aggr, plot_data and the models are
     # contained.
     tims_obj <- rolling_horizon(timeseries, model_idx, tims_obj)
     tims_obj
   },
   # If the rolling horizon gives an error, add the model name to the vector
   # of error_models.
   error = function(cond) {
     tims_obj$error_models <- c(tims_obj$error_models, model_name)
     print(cond) # Print the error
     return(tims_obj) 
   })
  }
  
  return(tims_obj)
}

add_metrics <- function(model_name, obj_metrics, metrics) {
    obj_metrics <- rbind(obj_metrics, 
          		 cbind(model_name, 
          		       metrics), 
                         make.row.names = FALSE) %>%
      mutate(ME = ME %>% as.character() %>% as.numeric(),
             RMSE = RMSE %>% as.character() %>% as.numeric(),
             MAE = MAE %>% as.character() %>% as.numeric(),
             MPE = MPE %>% as.character() %>% as.numeric(),
             MAPE = MAPE %>% as.character() %>% as.numeric())

  return(obj_metrics)
}

calculate_horizon_dates <- function(tims_obj, stepsize, hor_idx) {
    if (is.null(tims_obj$hor_spacing)) {
      hor_spacing <- 1
      hor_units <- stepsize
    } else {
      hor_spacing <- as.numeric(tims_obj$hor_spacing[1])
      hor_units   <- tims_obj$hor_spacing[2]
    }
    end_train <- tims_obj$starttest - 
        lubridate::period(1, units=stepsize) +
        lubridate::period(hor_idx * hor_spacing, 
                          units=hor_units)
    start_hor <- tims_obj$starttest + 
        lubridate::period(hor_idx * hor_spacing, 
                          units=hor_units)
    end_hor <- tims_obj$starttest + 
        lubridate::period(tims_obj$horizon, units=stepsize) +
        lubridate::period(hor_idx * hor_spacing, 
                          units=hor_units)
    hor_dates <- list("end_train" = end_train,
                      "start_hor" = start_hor,
                      "end_hor"   = end_hor)
    
    return(hor_dates)
}

calculate_train_series <- function(timeseries, tims_obj, hor_dates) {
    target_train <- timeseries$target_series %>% 
        window(end = hor_dates$end_train)

    if (!is.null(timeseries$regressors)) { 
        reg_train <- timeseries$regressors %>% 
            window(end = hor_dates$end_train)
    }

    timeseries$target_train <- target_train
    timeseries$reg_train <- reg_train
    return(timeseries)
}

calculate_test_series <- function(timeseries, tims_obj, hor_dates) {
    actual <- timeseries$target_series %>% 
        window(start = hor_dates$start_hor, end = hor_dates$end_hor)

    if (!is.null(timeseries$regressors)) { 
        reg_pred <- timeseries$regressors %>%
            window(start = hor_dates$start_hor, end = hor_dates$end_hor)
    }

    timeseries$actual   <- actual
    timeseries$reg_pred <- reg_pred
    return(timeseries)
}

train_model <- function(tims_obj, timeseries, model_idx, hor_idx) {
    target_train <- timeseries$target_train
    reg_train  <- timeseries$reg_train
    # Train the model.
    if (!is.null(timeseries$regressors)){
    	tims_obj$trained_models[[model_idx]][[hor_idx+1]] <- 
    		tims_obj$train_funcs[[model_idx]](target_train, 
    						  regressors = reg_train)
    } else {
    	tims_obj$trained_models[[model_idx]][[hor_idx+1]] <- 
    		tims_obj$train_funcs[[model_idx]](target_train)
    }

    return(tims_obj)
}

predict_model <- function(tims_obj, timeseries, model_idx, hor_idx){
    reg_pred <- timeseries$reg_pred

    if (!is.null(timeseries$regressors)){
        prediction_df <- tims_obj$trained_models[[model_idx]][[hor_idx+1]] %>% 
		tims_obj$pred_funcs[[model_idx]](tims_obj$horizon,
						 regressors = reg_pred)
    } else {
        prediction_df <- tims_obj$trained_models[[model_idx]][[hor_idx+1]] %>% 
		tims_obj$pred_funcs[[model_idx]](tims_obj$horizon)
    }

    return(prediction_df)
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
rolling_horizon <- function(timeseries, model_idx, tims_obj) {
  
  nowcasts <- data.frame("actual" = c(), "prediction" = c())
  if (!is.null(tims_obj$aggregate_fun)) {
    nowcasts_aggr <- data.frame("actual" = c(), "prediction" = c())
  }
  
  # plot_data <- list()
  for (i in 0:(tims_obj$num_tests - 1)) {
    hor_dates <- calculate_horizon_dates(tims_obj, timeseries$stepsize, i)
    timeseries <- calculate_train_series(timeseries, tims_obj, hor_dates)

    tims_obj <- train_model(tims_obj, timeseries, model_idx, i)

    timeseries <- calculate_test_series(timeseries, tims_obj, hor_dates)
    prediction_df <- predict_model(tims_obj, timeseries, model_idx, i)
    
    prediction <- prediction_df$prediction
    actual_pred <- cbind(timeseries$actual, prediction)
    colnames(actual_pred) <- c("actual", "prediction")
    nowcasts <- rbind(nowcasts, actual_pred)

    if (!is.null(tims_obj$aggregate_fun)) {
      actual_aggr <- tims_obj$aggregate_fun(timeseries$actual)
      prediction_aggr <- tims_obj$aggregate_fun(prediction)
      actual_pred_aggr <- cbind(actual_aggr, prediction_aggr)
      colnames(actual_pred_aggr) <- c("actual", "prediction")
      nowcasts_aggr <- rbind(nowcasts_aggr, actual_pred_aggr)
    } 
  }
  
  # Calculate the metrics based on the actual value and the prediction
  metrics <- accuracy(nowcasts[, 'actual'], nowcasts[, 'prediction'])
  # The metrics are stored as factors in the data frames. They have to be
  # converted to characters and then to numeric.
  tims_obj$metrics <- add_metrics(tims_obj$model_names[model_idx],
				  tims_obj$metrics, metrics)

  if (!is.null(tims_obj$aggregate_fun)) {
    metrics_aggr <- accuracy(nowcasts_aggr[, 'actual'], 
                             nowcasts_aggr[, 'prediction'])
    tims_obj$metrics_aggr <- add_metrics(tims_obj$model_names[model_idx],
					 tims_obj$metrics_aggr,
					 metrics_aggr)
  }   
  
  return(tims_obj)
}

