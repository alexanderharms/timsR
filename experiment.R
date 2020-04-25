### A testing setup for time series analysis.
# Authors: Alexander Harms, Pauline Sluijpers

# To run experiments the settings need to be set.
source("./settings/settings.R")

library(dplyr)
library(zoo) 
library(magrittr)

source("./R/utilities.R")
source("./R/rol_hor_functions.R")

if (!exists("REGCOLUMNS")) REGCOLUMNS <- NULL

if (!exists("STARTMODEL")) STARTMODEL <- STARTDATA

if (!exists("LOGFILE")) LOGFILE <- "./logs/log.txt"

# Schrijf de settings weg naar een logbestand 
sink(LOGFILE) 
print("Data file:")
print(DATAFILE)
print("Target:")
print(TARGET_VAR)
print("Regressors:")
print(REGCOLUMNS)
print("Start of data:")
print(STARTDATA)
print("Start of training set:")
print(STARTMODEL)
print("Start of test set:")
print(STARTTEST)
print("Horizon:")
print(H)
print("N_TEST:")
print(N_TEST)
print("Model vector:")
print(MODEL_VECTOR) 
sink()

# Read and prepare -----------------------------------------------------------
# Read the dataset and convert the dataset into a list with the target time
# series (singular or plural) and the regressors.
timeseries_list <- read.csv(DATAFILE, sep = ",", stringsAsFactors = FALSE) %>%
    prepare_timeseries(STARTDATA, STARTMODEL, FREQ, TARGET_VAR,
                       REGCOLUMNS = REGCOLUMNS)

target_series <- timeseries_list$target_series
extra_target_series <- timeseries_list$extra_target_series
regressors <- timeseries_list$regressors

# Test models -----------------------------------------------------------------
# Perform the rolling horizon test for each of the models in MODEL_VECTOR.
model_loop <- rol_hor_model_loop(target_series, MODEL_VECTOR, STARTTEST, 
                                 N_TEST, H, FREQ, regressors = regressors)

# Metrics --------------------------------------------------------------------
metrics <- model_loop$metrics
if (check_aggr()) {
  metrics_aggr <- model_loop$metrics_aggr
}
# Sort the data frames with the metrics based on the RMSE value.
metrics_sort <- metrics %>% dplyr::arrange(RMSE)
if (check_aggr()) {
  metrics_aggr_sort <- metrics_aggr %>% dplyr::arrange(RMSE)
}

# Write the sorted metrics to the log file and the csv file.
# Schrijf de data frames met metrieken weg naar het log bestand
sink(LOGFILE, append = TRUE)
print("Metrics:")
print(metrics_sort)
if (check_aggr()) {
  print("Metrics aggregated:")
  print(metrics_aggr_sort)
}
print("Error models:")
print(model_loop$error_models)
sink()

csv_filename <- substr(LOGFILE, start = 1, stop = nchar(LOGFILE) - 4) %>%
  paste0(".csv")
csv_filename_aggr <- substr(LOGFILE, start = 1, stop = nchar(LOGFILE) - 4) %>%
   paste0("_aggr.csv")

write.csv(metrics, file = csv_filename, row.names = FALSE)
if (check_aggr()) {
  write.csv(metrics_aggr, file = csv_filename_aggr, row.names = FALSE)
}

