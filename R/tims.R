new_tims <- function(model_names = character(), startmodel = NULL, 
		     starttest = NULL, horizon = integer(), 
		     num_tests = integer(), 
		     horizon_spacing = NULL, aggregate_fun = NULL){
  
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
		    "horizon" = horizon,
		    "num_tests" = num_tests,
		    "horizon_spacing" = horizon_spacing,
		    "aggregate_fun" = aggregate_fun,
		    "metrics" = metrics,
		    "metrics_aggr" = metrics_aggr,
		    "error_models" = error_models)
  class(tims_list) <- "tims"
  return(tims_list)
}

tims <- function(model_names, startmodel, starttest, horizon,
		 num_tests = NULL, horizon_spacing = 1, 
		 aggregate_fun = NULL) {
	
  return(new_tims(model_names, startmodel, starttest, horizon,
		  num_tests, horizon_spacing, aggregate_fun))
}

# Train model
train <- function(x) {
  UseMethod("train")
}

train.tims <- function(x) {
}

print.tims <- function(obj) {
  # print("Hello, world!")
}


# Plot a test with a certain index
# Default the last index
plot.tims <- function() {
}

# Logs data and results
logging <- function(obj, log_filename) {
  UseMethod("sort_metrics")
}

logging.default <- function(obj, log_filename) {
  print("Use the 'tims' method")
}

logging.tims <- function(obj, log_filename) {
  sink(log_filename, append = TRUE)
  print("Start of data:")
  print(obj$startdata)
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

