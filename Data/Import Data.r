#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Import Data from SPSS, Stata, SAS, CSV or TXT
#==============================================================================
# Title          : Import Data.r
# Description    : Import Data from SPSS, Stata, SAS, CSV or TXT.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180408
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on 
#                  https://datascienceplus.com/get-your-data-into-r/
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
install.packages("foreign")
install.packages("Hmisc")

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(foreign)
library(Hmisc)

#------------------------------------------------------------------------------
# Data generation/preparation and pattern
#------------------------------------------------------------------------------


# Import data from SPSS -------------------------------------------------------
# R can import datasets from SPSS with the function read.spss() from the 
# package foreign. Alternatively, the function spss.get() from Hmisc package 
# can be used. While foreign is a default package in R, the Hmisc package need 
# to be installed.
library(foreign)
df <- read.spss("dataset.sav", use.value.label=TRUE, to.data.frame=TRUE)

# Import data from Stata
# To import a dataset from Stata into R, the function read.dta() from foreign 
# package is used. More specifically look the code below:

library(foreign)
df <- read.dta("dataset.dta")

# Import data from SAS --------------------------------------------------------
# To import a dataset from SAS into R there are different methods, but most 
# recommended is to export first the dataset from SAS into CSV and then to 
# import in R.

# First use the code below in SAS (not R) to export data:
proc export data=dataset # run in SAS
outfile="datast.csv"
dbms=csv;
run;
# Now that your data is exported you can import in R by using the code below:
df <- read.csv("dataset.csv",header=T,as.is=T)
# Another way to upload SAS files (.XPT) direct to R environment is by using 
# Hmisc package.
library(Hmisc)
df <- sasxport.get("/filename.xpt") 

# Import data from CSV --------------------------------------------------------
# You can import data from csv into R by using read.table() function similarly 
# as we used with importing txt files. Basically, this function reads a csv 
# file in table format and saves it as a data frame.

# This is the code we use to import csv file into R
df <- read.table("dataset.csv", header=TRUE, sep=",")

# Often I use the function read.csv() to import csv file into R
df <- read.csv("dataset.csv",header=T,as.is=T)

# Import data from TXT --------------------------------------------------------
# To import text file into R use the function read.table(). This function 
# reads a file in table format and saves it as a data frame. The code used to 
# import text file is in example below.

df <- read.table("dataset.txt", as.is=TRUE, header=T)
# In some cases I find this code useful for importing the data:
df <- read.table("dataset.txt", header=TRUE, sep=",")
# Or this code:
df <- read.table("dataset.txt", header=T, strings=F)

# Load data in R --------------------------------------------------------------
# To load Rdata in R is easy and straightforward method. However, first we 
# need to know how to save the dataframe in R. The function used for saving the 
# dataframe is save(objectlist, file="myfile"), where objectlist is the name of 
# your current dataframe and myfile is the filename of RDATA you will save on 
# your computer. The function to upload the Rdata in R is load().

# Save the dataset:
save(df, file="mydata.Rdata")
# Load data in R:
load("mydata.Rdata")