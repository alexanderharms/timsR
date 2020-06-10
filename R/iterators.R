iterate_over_time <- function(tims_object) {

  nowcasts <- data.frame("actual" = c(), "prediction" = c())
  # if (!is.null(tims_obj$aggregate_fun)) {
  #   nowcasts_aggr <- data.frame("actual" = c(), "prediction" = c())
  # }
  
  # plot_data <- list()
  for (i in 0:(tims_object$num_tests - 1)) {
    # tsp(series)[3] is the frequency of the series
    enddate <- tims_object$start_test[1] + 
	    (tims_object$start_test[2] - 1 
	     + tims_object$horizon - 1 + i)/tsp(tims_object$target_series)[3]
    series_nieuw <- window(tims_object$target_series, end = enddate)
    actual <- series_nieuw[(length(series_nieuw) - (tims_obj$horizon - 1))
			   :length(series_nieuw)]
    series_nieuw[(length(series_nieuw) - (tims_obj$horizon - 1))
		 :length(series_nieuw)] <- NA

    if (!is.null(regressors)) { 
        reg_train <- window(regressors, 
			    end = (enddate - tims_object$horizon/tsp(series)[3]))
        
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
  return(tims_object)
}

iterate_over_models <- function(tims_object) {
  for (model_idx in 1:length(tims_object$model_names)) {
    model_name <- tims_object$model_names[model_idx]
    print(paste0("Model: ", model_name))
    
    # If the models don't converge, they are added to the error_models vector.
    # In this way the loop can continue.
   tims_object <- tryCatch({
      # In rol_hor_list the metrics, metrics_aggr, plot_data and the models are
      # contained.
      tims_object <- iterate_over_time(tims_object, model_idx)
#       tims_obj <- rolling_horizon(target_series, model_idx, 
# 				  tims_obj, regressors = regressors)
      tims_obj
     },
   #  # If the rolling horizon gives an error, add the model name to the vector
   #  # of error_models.
     error = function(cond) {
       tims_object$error_models <- c(tims_object$error_models, model_name)
       print(cond) # Print the error
       return(tims_object) 
     })
  }
  
  return(tims_object)
}
