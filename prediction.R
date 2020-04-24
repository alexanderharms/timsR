### Predict by using  time series analysis.
# Authors: Alexander Harms, Pauline Sluijpers

# To run experiments the settings need to be set.
source("./settings/settings.R")

library(dplyr)
library(zoo) 
library(timeDate)  
library(forecast)  
library(magrittr)
library(tseries)   

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

# Take the first name that is specified in MODEL_VECTOR
model_name <- MODEL_VECTOR[1]

# Select the train and predict function
train_fun <- paste0("train_", model_name) %>% get()
pred_fun  <- paste0("pred_", model_name) %>% get()

enddate <- STARTTEST[1] + (STARTTEST[2] - 1)/tsp(target_series)[3]
target_series <- na.trim(target_series)

if (is.null(regressors)){
  trained_model <- train_fun(target_series)
  prediction_df <- trained_model %>% pred_fun(H)
} else {
  reg_train <- window(regressors, end = tsp(target_series)[2])
  
  trained_model <- train_fun(target_series, reg_train)

  reg_pred <- window(regressors,
                     start = tsp(target_series)[2] 
                     + 1/tsp(target_series)[3])
  prediction_df <- trained_model %>% pred_fun(H, reg_pred)
}

prediction <- prediction_df$prediction

# Plot the prediction in red
ts.plot(target_series, prediciton, gpars=list(col = c("black", "red")))

# Make a data frame with the year, period and the corresponding prediction
date_vector <- seq(end(target_series)[1] + (end(target_series)[2])/FREQ, 
                   end(target_series)[1] + (end(target_series)[2] + H - 1)/FREQ, 
                   length.out = H)

year_vec <- floor(date_vector) 
period_vec <- round((date_vector - floor(date_vector)) * FREQ) + 1
prediction_df <- data.frame("Year" = jaar_vec,
                              "Period" = period_vec,
                              "Prediction" = as.numeric(prediction))
sink(LOGFILE, append = TRUE)
print(prediction_df)
sink()

# Opslaan van het dataframe met de gemaakte voorspelling
predict_csv <- substr(LOGFILE, start = 1, stop = nchar(LOGFILE) - 4) %>%
  paste0("_prediction.csv")
write.csv2(prediction_df, file = predict_csv, row.names = FALSE)
