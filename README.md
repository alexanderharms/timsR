# timsR
> A testing setup for time series analysis.

This set of scripts allows you to iterate quickly over a number of models you
define. The variables of each test are defined in a settings file which can
easily be exchanged with other people and which allow convenient re-testing.
Results of each test are stored in a log file and are written to CSV.

## Installation

These scripts are written in R and depend on the packages:
* dplyr
* magrittr.

## Usage example

There are two scripts: 
* experimenting.R: Used for testing various defined models.
* predict.R: When the best model is found, the settings file can be used in to
  use this model for predictions.

In this repository an example is shown based on the dataset 'seatbelts.csv' from
the package 'datasets'. The settings file is available in the folder 'settings'
and the resulting log can be found in the 'logs' folder.


## Development and contributing

For development no specific conditions have to be met. One can contribute by
forking the repository and opening a pull request.

## Release History

* 0.1.0
    * The first proper release

## Licence

Distributed under the GPL 3 license. See ``LICENSE`` for more information.

## Meta
Authors: Alexander Harms and Pauline Sluijpers.
