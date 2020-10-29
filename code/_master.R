#######################################################################################
###
###   This script runs three separate scripts in sequence to generate all results.
###   The user should make any input changes to this script.
###   
###   Author : Raghav Saraogi
########################################################################################


# Setup -------------------------------------------------------------------

# clear environment
rm(list = ls())
options(warn=-1)

# load required packages
install.packages("pacman")
p_load(tidyverse, glue, janitor, readxl, scales, lubridate,
       fredr, zoo, sandwich, broom, lmtest)

# set working directory
setwd("//SET WORKING DIRECTORY//")

# FRED API Key
FRED_KEY = '//ENTER FRED API KEY//'

# FRED series to use for macro variables
FRED_POPULATION = 'CNP16OV'
FRED_GDP        = 'GDP'
FRED_DEFLATOR   = 'GDPDEF'
FRED_HOURS      = 'HOANBS'
FRED_CONSUMPTION = 'PCE'
FRED_INVESTMENT  = 'PNFI'

# Input list of shock variables to use
SHOCKS <- c('ford', 'fernald')

# Select confidence level to use for plots
CI = 0.667

# Select number of horizons to plot responses
H = 20


# Process Data and Run Regressions ----------------------------------------

# Load and process data
source("./code/01_data_processing.R")

# Load a custom function to compute impulse responses
source("./code/02_run_regressions.R")

# Plot results for Ford and Fernald Shocks
source("./code/03_plot_impulse_response.R")


