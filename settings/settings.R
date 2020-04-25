# The data file contains the target series and the regressors.
DATAFILE <- "./data/seatbelts.csv"

# During the experiments a number of things are logd.
# The calculated metrics are written to CSV files.
# If LOGFILE is "./logs/log.txt", then the CSV files will get written to 
# "./logs/log.csv".
LOGFILE <- "./logs/log.txt"

# Frequency of the timeseries. For monthly data, FREQ <- 12.
FREQ <- 12

# Column in DATAFILE for the target time series.
TARGET_VAR <- "drivers" 
# Column(s) in DATAFILE that indicate the regressors.
# Enter NULL to not use regressors.
REGCOLUMNS <- c("kms")

# Starting date of the time series.
STARTDATA <- c(1969, 1)

# The starting date of the training set.
STARTMODEL <- c(1969, 1) 

# STARTTEST is the start point of the first rolling horizon test.
STARTTEST <- c(1981, 1)

H <- 12 # Prediction horizon
N_TEST <- 24 # Number of rolling horizons

# Indicate the models that should be tested.
# The script will look for the functions "train_name" to train the model and 
# "pred_naam" to predict with the model, where 'name' is the name that is
# indicated in MODEL_VECTOR.
MODEL_VECTOR <- c("arima1")

# The R files containing the model definitions can be sourced in this file. 
source("./models/models-arima.R")

# A aggregation function can be defined to allow prediction in a different
# pattern than the training time series. The training time series can for
# example be in months, but only the last year is of interest.
# aggr_fun <- function(predictions) {
#     pred_year <- sum(tail(predictions, 12))
#     return(pred_year)
# }
