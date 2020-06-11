train_model <- function(tims_object, model_index=1, hor_num=1) {
  target_train <- tims_object$target_train
  reg_train  <- tims_object$reg_train
  
  # Train the model.
  if (!is.null(tims_object$reg_var)){
    tims_object$trained_models[[model_index]][[hor_num]] <- 
      tims_object$train_funcs[[model_index]](target_train, 
                                             regressors = reg_train)
  } else {
    tims_object$trained_models[[model_index]][[hor_num]] <- 
      tims_object$train_funcs[[model_index]](target_train)
  }
  
  return(tims_object)
}

predict_model <- function(tims_object, model_index=1, hor_num=1){
  reg_pred <- tims_object$reg_test
  
  if (!is.null(tims_object$reg_var)){
    prediction_df <- tims_object$trained_models[[model_index]][[hor_num]] %>% 
      tims_object$pred_funcs[[model_index]](tims_object$horizon, 
                                            regressors = reg_pred)
  } else {
    prediction_df <- tims_object$trained_models[[model_index]][[hor_num]] %>% 
      tims_object$pred_funcs[[model_index]](tims_object$horizon)
  }
  
  tims_object$prediction <- prediction_df
  
  return(tims_object)
}

add_predictions <- function(tims_object) {
  main_prediction <- tims_object$prediction$prediction
  temp_df <- cbind(tims_object$target_test, main_prediction)
  colnames(temp_df) <- c('actual', 'prediction')
  
  tims_object$predictions_over_time <- 
    rbind(tims_object$predictions_over_time, 
          temp_df)
  
  return(tims_object)
}

calculate_metrics <- function(tims_object, model_index) {
  model_name <- tims_object$model_names[model_index]
  result_df <- tims_object$predictions_over_time
  # Calculate the metrics based on the actual value and the prediction
  metrics <- accuracy(result_df[, 'actual'], result_df[, 'prediction'])
  # The metrics are stored as factors in the data frames. They have to be
  # converted to characters and then to numeric.
  # temp_df <- cbind(model_name, metrics) 
  # rownames(temp_df) <- NULL
  tims_object$metrics <- rbind(tims_object$metrics,
                               cbind(model_name, metrics), 
                       make.row.names = FALSE) %>%
    mutate(ME = ME %>% as.character() %>% as.numeric(),
           RMSE = RMSE %>% as.character() %>% as.numeric(),
           MAE = MAE %>% as.character() %>% as.numeric(),
           MPE = MPE %>% as.character() %>% as.numeric(),
           MAPE = MAPE %>% as.character() %>% as.numeric())

  return(tims_object)
}

iterate_over_time <- function(tims_object, model_index = 1) {

  # if (!is.null(tims_obj$aggregate_fun)) {
  #   nowcasts_aggr <- data.frame("actual" = c(), "prediction" = c())
  # }
  
  tims_object$predictions_over_time <- 
    data.frame("actual" = c(), "prediction" = c())
  for (hor_num in 1:tims_object$num_tests) {
    tims_object <- tims_object %>%
      get_train_data(hor_num) %>%
      get_test_data(hor_num) %>%
      train_model(model_index, hor_num) %>%
      predict_model(model_index, hor_num) %>%
      add_predictions()  
  }
  
  tims_object <- calculate_metrics(tims_object, model_index)
  return(tims_object)
}

iterate_over_models_and_time <- function(tims_object) {
  for (model_idx in 1:length(tims_object$model_names)) {
    model_name <- tims_object$model_names[model_idx]
    print(paste0("Model: ", model_name))
    
    # If the models don't converge, they are added to the error_models vector.
    # In this way the loop can continue.
   tims_object <- tryCatch({
      # In rol_hor_list the metrics, metrics_aggr, plot_data and the models are
      # contained.
      tims_object <- iterate_over_time(tims_object, model_idx)
      tims_object
      },
    # If the rolling horizon gives an error, add the model name to the vector
    # of error_models.
      error = function(cond) {
       tims_object$error_models <- c(tims_object$error_models, model_name)
       print(cond) # Print the error
       return(tims_object)
      })
  }
  return(tims_object)
}
