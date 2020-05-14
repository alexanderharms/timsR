# timsR
> A testing setup for time series analysis.

This set of scripts allows you to iterate quickly over a number of models you
define. The variables of each test are defined in a settings file which can
easily be exchanged with other people and which allow convenient re-testing.
Results of each test are stored in a log file and are written to CSV.

## Installation

These scripts are written in R 3.6 and depends on the packages:
* zoo
* xts
* dplyr
* magrittr
* for the 'plot' method of the 'tims' object ggplot2 is needed.

In the example 'auto.arima' is used; for this function the package 'forecast'
needs to be installed.

## Usage example

There are two scripts: 
* experiment.R: Used for testing various defined models.
* prediction.R: When the best model is found, the settings file can be used in to
  use this model for predictions.

To use this testing suite you define an R file with the settings for your
experiment. An example is shown in ./settings/settings.R. To change the settings
file, the source statement in experiment.R or prediction.R must be changed. Based on these
settings the time series, target series and additional variables, are read and
prepared and an S3 object of the new class 'tims' is created. This object will
contain all the information regarding the testing of the models.  

For the models that you want to test a train and prediction function can be
defined in the settings file or in a separate file, sourced in the settings
file. Examples of these models can be found in the 'models' folder. 

In this repository an example is shown based on the dataset 'seatbelts.csv' from
the package 'datasets'. The settings file is available in the folder 'settings'
and the resulting log can be found in the 'logs' folder.

## Development and contributing

For development no specific conditions have to be met. One can contribute by
forking the repository and opening a pull request.

## Release History

* 0.2.0
    * Introduced the 'tims' S3 object. This object contains the settings, the
      trained models and the metrics of the tested time series models.
* 0.1.0
    * The first proper release

## Licence

Distributed under the GPL 3 license. See ``LICENSE`` for more information.

## Meta
Authors: Alexander Harms.
Based on a project by Pauline Sluijpers and myself. The code for that project
(in Dutch) can be found at
[https://github.com/alexanderharms/timsR-dutch](https://github.com/alexanderharms/timsR-dutch).
