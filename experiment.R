### A testing setup for time series analysis.
# Authors: Alexander Harms

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
if (!exists("HOR_SPACING")) HOR_SPACING <- c(1, STEPSIZE)

# Write settings to a logfile
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

# Test models -----------------------------------------------------------------
# Generate tims object
tims_obj <- tims(MODEL_VECTOR, STARTMODEL, STARTTEST, H,
		 num_tests = N_TEST, horizon_spacing=HOR_SPACING, 
                 aggregate_fun = aggr_fun)

# Perform the rolling horizon test for each of the models in MODEL_VECTOR.
tims_obj <- rol_hor_model_loop(timeseries, tims_obj)

# Metrics --------------------------------------------------------------------
# Sort the data frames with the metrics based on the RMSE value.
tims_obj <- sort_metrics(tims_obj, "RMSE")

# Write the sorted metrics to the log file and the csv file.
# Schrijf de data frames met metrieken weg naar het log bestand
logging(tims_obj, LOGFILE)

export(tims_obj, LOGFILE)

# Plot ----------------------------------------------------------------------
plot(tims_obj, timeseries, model_idx=1, hor_idx=0)
