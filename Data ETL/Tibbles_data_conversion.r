#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Tibbles data conversion
#==============================================================================
# Title          : Tibbles_data_conversion.r
# Description    : Converts data.frame object into Tibbles data formart. 
#                  Important to run series decomposition using 'anomalize'
#                  package.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180419
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on 
#                  Tibbles documentation.
#                  
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
if(!require("devtools")) install.packages("devtools")
if(!require("devtools")) install.packages("tibble")
devtools::install_github("twitter/AnomalyDetection") 
devtools::install_github("business-science/anomalize")

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(devtools)
library(tibble)
library(AnomalyDetection)
library(anomalize)

#==============================================================================
# Code
#==============================================================================

# Converting a data.frame to a `tbl_time`
# Using Date index
ex1 <- data.frame(date = Sys.Date(), value = 1)
ex1_tbl_time <- as_tbl_time(ex1, date)
class(ex1_tbl_time)
attributes(ex1_tbl_time)

# Converting a tibble to a `tbl_time`
# Using POSIXct index
ex2 <- tibble::tibble(
  time  = as.POSIXct(c("2017-01-01 10:12:01", "2017-01-02 12:12:01")),
  value = c(1, 2)
)

# EOF