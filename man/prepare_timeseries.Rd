% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{prepare_timeseries}
\alias{prepare_timeseries}
\title{Prepare time series}
\usage{
prepare_timeseries(data, STARTDATA, STARTMODEL, FREQ, TARGET_VAR = NULL,
  REGCOLUMNS = NULL, plot_timeseries = FALSE)
}
\description{
Reads in a data frame with columns for the target variable(s) and the
regressors; returns a list with the time series for the main target variable,
any extra target variables and the regressors.
}
