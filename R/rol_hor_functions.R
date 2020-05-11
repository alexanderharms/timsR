rol_hor_model_loop <- function(target_series, tims_obj, 
                               regressors = NULL) {
  

  # Loop over the models in MODEL_VECTOR
  for (model_idx in 1:length(tims_obj$model_names)) {
    model_name <- tims_obj$model_names[model_idx]
    print(paste0("Model: ", model_name))
    
    # If the models don't converge, they are added to the error_models vector.
    # In this way the loop can continue.
   # tims_obj <- tryCatch({
      # In rol_hor_list the metrics, metrics_aggr, plot_data and the models are
      # contained.
      tims_obj <- rolling_horizon(target_series, model_idx, 
				  tims_obj, regressors = regressors)
      # rol_hor_list <- rolling_horizon(target_series, FUN = train_fun, 
      #                                 FUN_PRED = pred_fun,
      #                                 start_eval = STARTTEST, 
      #                                 eval_periods = N_TEST,
      #                                 h = H, regressors = regressors)
      
      # plot_rol_hor_list(rol_hor_list, model_name, freq = FREQ, zoom = TRUE, 
      #                   target_series = target_series)
      
      # The metrics are stored as factors in the data frames. They have to be
      # converted to characters and then to numeric.
      # metrics <- rbind(metrics, cbind(model_name, rol_hor_list$metrics), 
      #                  make.row.names = FALSE) %>%
      #   mutate(ME = ME %>% as.character() %>% as.numeric(),
      #          RMSE = RMSE %>% as.character() %>% as.numeric(),
      #          MAE = MAE %>% as.character() %>% as.numeric(),
      #          MPE = MPE %>% as.character() %>% as.numeric(),
      #          MAPE = MAPE %>% as.character() %>% as.numeric())
      # if (check_aggr()) {
      #   metrics_aggr <- rbind(metrics_aggr, cbind(model_name, rol_hor_list$metrics), 
      #                    make.row.names = FALSE) %>%
      #     mutate(ME = ME %>% as.character() %>% as.numeric(),
      #            RMSE = RMSE %>% as.character() %>% as.numeric(),
      #            MAE = MAE %>% as.character() %>% as.numeric(),
      #            MPE = MPE %>% as.character() %>% as.numeric(),
      #            MAPE = MAPE %>% as.character() %>% as.numeric())
      # } 
      # 
      # metrics_list <- list("metrics" = metrics,
      #                      "metrics_aggr" = metrics_aggr,
      #                      "error_models" = error_models)
   #   tims_obj
   #  },
   #  # If the rolling horizon gives an error, add the model name to the vector
   #  # of error_models.
   #  error = function(cond) {
   #    tims_obj$error_models <- c(tims_obj$error_models, model_name)
   #    print(cond) # Print the error
   #    return(tims_obj) 
   #  })
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
rolling_horizon <- function(series, model_idx, tims_obj, regressors = NULL) {
  
  nowcasts <- data.frame("actual" = c(), "prediction" = c())
  if (!is.null(tims_obj$aggregate_fun)) {
    nowcasts_aggr <- data.frame("actual" = c(), "prediction" = c())
  }
  
  # plot_data <- list()
  for (i in 0:(tims_obj$num_tests - 1)) {
    # tsp(series)[3] is the frequency of the series
    enddate <- tims_obj$starttest[1] + 
	    (tims_obj$starttest[2] - 1 + tims_obj$horizon - 1 + i)/tsp(series)[3]
    series_nieuw <- window(series, end = enddate)
    actual <- series_nieuw[(length(series_nieuw) - (tims_obj$horizon - 1))
			   :length(series_nieuw)]
    series_nieuw[(length(series_nieuw) - (tims_obj$horizon - 1))
		 :length(series_nieuw)] <- NA

    if (!is.null(regressors)) { 
        reg_train <- window(regressors, 
			    end = (enddate - tims_obj$horizon/tsp(series)[3]))
        
        reg_pred <- window(regressors, 
			   start = (enddate - (tims_obj$horizon - 1)/tsp(series)[3]),
                           end = enddate)
    }
    
    # Train the model.
    if (!is.null(regressors)){
	tims_obj$trained_model[[model_idx]] <- 
		tims_obj$train_funcs[[model_idx]](series_nieuw, 
						  regressors = reg_train)
    } else {
	tims_obj$trained_model[[model_idx]] <- 
		tims_obj$train_funcs[[model_idx]](series_nieuw)
    }
    
    # FUN_PRED returns a data frame with the prediction and the 95% confidence
    # interval. 
    if (!is.null(regressors)){
        return_df <- tims_obj$trained_model[[model_idx]] %>% 
		tims_obj$pred_funcs[[model_idx]](tims_obj$horizon,
						 regressors = reg_pred)
    } else {
        return_df <- tims_obj$trained_model[[model_idx]] %>% 
		tims_obj$pred_funcs[[model_idx]](tims_obj$horizon)
    }
    prediction <- return_df$prediction
    actual_pred <- cbind(actual, prediction)
    nowcasts <- rbind(nowcasts, actual_pred)

    if (!is.null(tims_obj$aggregate_fun)) {
      actual_aggr <- tims_obj$aggregate_fun(actual)
      prediction_aggr <- tims_obj$aggregate_fun(prediction)
      actual_pred_aggr <- cbind(actual_aggr, prediction_aggr)
      nowcasts_aggr <- rbind(nowcasts_aggr, actual_pred_aggr)
    } 
    
    # Accompany plot_data with time information to later plot it as a time
    # series.
    # date_vector <- enddate +
    #   seq(-(h - 1), 0, length.out = h)/tsp(series)[3]
    # plot_data[[i+1]] <- cbind(actual, return_df, date_vector)
  }
  
  # Calculate the metrics based on the actual value and the prediction
  metrics <- accuracy(nowcasts[, 'actual'], nowcasts[, 'prediction'])
  # The metrics are stored as factors in the data frames. They have to be
  # converted to characters and then to numeric.
  tims_obj$metrics <- add_metrics(tims_obj$model_names[model_idx],
				  tims_obj$metrics, metrics)

  if (!is.null(tims_obj$aggregate_fun)) {
    metrics_aggr <- accuracy(nowcasts_aggr[, 'actual_aggr'], 
                             nowcasts_aggr[, 'prediction_aggr'])
    tims_obj$metrics_aggr <- add_metrics(tims_obj$model_names[model_idx],
					 tims_obj$metrics_aggr,
					 metrics_aggr)
  }   
  
  return(tims_obj)
}

