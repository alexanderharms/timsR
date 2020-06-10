library(magrittr)

source("./R/tims.R")
source("./R/timeseries.R")
source("./R/utilities.R")

# Load settings
settings_list <- load_settings("./settings/settings.R")
# Load data
df <- load_data(settings_list)

# Generate timsR object
tims_object <- tims(settings_list, df) %>%
  extract_time_series() %>%  # Extract time series
  iterate_models_and_time() %>%
  sort_metrics("RMSE") %>%
  export_results()

# Iterate over models
# Iterate over time
# Calculate metrics
# Export results
# Plot results
