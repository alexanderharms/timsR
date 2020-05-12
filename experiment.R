### A testing setup for time series analysis.
# Authors: Alexander Harms

# To run experiments the settings need to be set.
source("./settings/settings.R")

library(dplyr)
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

# Schrijf de settings weg naar een logbestand 
sink(LOGFILE) 
print("Data file:")
print(DATAFILE)
print("Target:")
print(TARGET_VAR)
print("Regressors:")
print(REGCOLUMNS)
sink()

# Read and prepare -----------------------------------------------------------
# Read the dataset and convert the dataset into a list with the target time
# series (singular or plural) and the regressors.
timeseries <- read.csv(DATAFILE, sep = ",", stringsAsFactors = FALSE) %>% 
    prepare_timeseries(STARTDATA, STARTMODEL, FREQ, TARGET_VAR, 
                       REGCOLUMNS = REGCOLUMNS)

# Test models -----------------------------------------------------------------
# Generate tims object
tims_obj <- tims(MODEL_VECTOR, STARTMODEL, STARTTEST, H,
		 num_tests = N_TEST, aggregate_fun = aggr_fun)

# Perform the rolling horizon test for each of the models in MODEL_VECTOR.
tims_obj <- rol_hor_model_loop(timeseries, tims_obj)

# Metrics --------------------------------------------------------------------
# Sort the data frames with the metrics based on the RMSE value.
tims_obj <- sort_metrics(tims_obj, "RMSE")

# Write the sorted metrics to the log file and the csv file.
# Schrijf de data frames met metrieken weg naar het log bestand
logging(tims_obj, LOGFILE)

export(tims_obj, LOGFILE)

