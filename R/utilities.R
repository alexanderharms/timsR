load_settings <- function(source_filename) {
  print("Sourcing settings file...")
  source(source_filename)
  # Place any assertions and checks here
  if (!exists("REGCOLUMNS")) REGCOLUMNS <- NULL
  if (!exists("STARTMODEL")) STARTMODEL <- STARTDATA
  if (!exists("LOGFILE")) LOGFILE <- "./logs/log.txt"
  # if (!exists("aggr_fun")) aggr_fun <- NULL
  settings_list <- list(
    "datafile" = DATAFILE,
    "logfile" = LOGFILE,
    "target_var" = TARGET_VAR,
    "reg_var" = REGCOLUMNS,
    "frequency" = FREQ,
    "start_data" = STARTDATA,
    "start_model" = STARTMODEL,
    "start_test" = STARTTEST,
    "horizon" = H,
    "num_tests" = N_TEST,
    "model_names" = MODEL_VECTOR)
    # "aggregate_fun" = aggr_fun)
  return(settings_list)
}

load_data <- function(settings_list) {
  datafile <- settings_list$datafile
  print(datafile)
  
  # Maybe extend with the possibility to read in a list of datasets.
  df <- read.csv(datafile, sep = ",", stringsAsFactors = FALSE)
  return(df)
}

sort_metrics <- function(tims_object, sort_tag) {
  sort_tag <- rlang::enquo(sort_tag)
  tims_object$metrics <- tims_object$metrics %>% dplyr::arrange(!! sort_tag)
  # if (!is.null(obj$aggregate_fun)) {
  #   obj$metrics_aggr <- obj$metrics_aggr %>% dplyr::arrange(!! sort_tag)
  # }
  return(tims_object)
}

log_experiment <- function(tims_object) {
  log_filename <- tims_object$logfile
  
  sink(log_filename, append = TRUE)
  print("Start of training set:")
  print(tims_object$start_model)
  print("Start of test set:")
  print(tims_object$start_test)
  print("Horizon:")
  print(tims_object$horizon)
  print("Number of tests:")
  print(tims_object$num_tests)
  print("Model vector:")
  print(tims_object$model_names) 
  print("Metrics:")
  print(tims_object$metrics)
  # if (!is.null(obj$aggregate_fun)) {
  #   print("Metrics, aggregated:")
  #   print(obj$metrics_aggr)
  # }
  print("Error models:")
  print(tims_object$error_models)
  sink()
  return(tims_object)
}

export_results <- function(tims_object) {
  log_filename <- tims_object$logfile
  
  print(tims_object$metrics)
  csv_filename <- substr(log_filename, start = 1, 
                         stop = nchar(log_filename) - 4) %>% paste0(".csv")
  # csv_filename_aggr <- substr(log_filename, start = 1, 
  #                             stop = nchar(log_filename) - 4) %>% paste0("_aggr.csv")
  
  write.csv(tims_object$metrics, file = csv_filename, row.names = FALSE)
  # if (!is.null(obj$aggregate_fun)) {
  #   write.csv(obj$metrics_aggr, file = csv_filename_aggr, row.names = FALSE)
  # }
  return(tims_object)
}

plot_experiment <- function(tims_object, model_index=1, hor_num=1) {
  tims_object <- tims_object %>% 
    get_test_data(hor_num) %>%
    predict_model(model_index, hor_num)
  plot_title <- paste0("Model: ", tims_object$model_names[model_index], 
                       ", Horizon: ", hor_num)
  
  ts.plot(tims_object$target_test, 
          tims_object$prediction[, 'prediction'],
          tims_object$prediction[, 'lower_conf'],
          tims_object$prediction[, 'upper_conf'],
          gpars=list(main = plot_title,
                     col= c('black', 'red', 'blue', 'blue')))
}
