### Predict by using  time series analysis.
# Authors: Alexander Harms, Pauline Sluijpers

# To run experiments the settings need to be set.
source("./settings/settings.R")

library(dplyr)
library(ggplot2)
library(zoo) 
library(magrittr)
library(lubridate)
library(xts)

source("./R/utilities.R")
source("./R/tims.R")
source("./R/rol_hor_functions.R")

if (!exists("REGCOLUMNS")) REGCOLUMNS <- NULL
if (!exists("STARTMODEL")) STARTMODEL <- STARTDATA
if (!exists("LOGFILE")) LOGFILE <- "./logs/log.txt"
if (!exists("aggr_fun")) aggr_fun <- NULL
if (!exists("REGCOLUMNS")) REGCOLUMNS <- NULL

# Schrijf de settings weg naar een logbestand 
sink(LOGFILE) 
print("Data file:")
print(DATAFILE)
print("Target:")
print(TARGET_VAR)
print("Regressors:")
print(REGCOLUMNS)
print("Start data:")
print(STARTDATA)
sink()

# Read and prepare -----------------------------------------------------------
# Read the dataset and convert the dataset into a list with the target time
# series (singular or plural) and the regressors.
timeseries <- read.csv(DATAFILE, sep = ",", stringsAsFactors = FALSE) %>% 
    prepare_timeseries(STARTDATA, STARTMODEL, STEPSIZE, TARGET_VAR, 
                       REGCOLUMNS = REGCOLUMNS)

# Generate tims object
pred_model_idx <- 1
tims_obj <- tims(MODEL_VECTOR[pred_model_idx], STARTMODEL, STARTTEST, H,
		 num_tests = 1, aggregate_fun = aggr_fun)

hor_dates <- calculate_horizon_dates(tims_obj, timeseries$stepsize, hor_idx=0)
timeseries <- calculate_train_series(timeseries, tims_obj, hor_dates)

tims_obj <- train_model(tims_obj, timeseries, model_idx=pred_model_idx, hor_idx=0)

timeseries <- calculate_test_series(timeseries, tims_obj, hor_dates)
prediction_df <- predict_model(tims_obj, timeseries, model_idx=pred_model_idx, hor_idx=0)

prediction <- prediction_df$prediction

# Plot the prediction in red
plot(tims_obj, timeseries, pred_model_idx, hor_idx=0)
