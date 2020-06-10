### A testing setup for time series analysis.
# Author: Alexander Harms

library(magrittr)
library(dplyr)
library(zoo) 

source("./R/tims.R")
source("./R/iterators.R")
source("./R/timeseries.R")
source("./R/utilities.R")

# Load settings
settings_list <- load_settings("./settings/settings.R")
# Load data
df <- load_data(settings_list)

# Generate timsR object
tims_object <- tims(settings_list, df) %>%
  prepare_timeseries() %>%  # Prepare time series
  iterate_over_models_and_time() %>%
  sort_metrics("RMSE") %>%
  export_results() 

for (num in 1:5) {
  plot_experiment(tims_object, model_index=1, hor_num=num)
}
