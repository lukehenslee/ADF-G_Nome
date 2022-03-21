#===============================================================================
# PPA interpolation for pink salmon
#
# Date: March 4, 2022
# 
# Creator: Luke Henslee
#
# Purpose: This code takes hourly escapement data for pink salmon tower counts
# and interpolates missed hourly counts and missed days using PPA methods
#===============================================================================
# NOTES: Input data files must be raw hourly count data. 
#
# The file must be in .csv format and the columns must be labeled in the 
# following order:
#
# 'date', 'X0000', 'X0100', 'X0200'... etc.
#
# The 'date' column must be in mm/dd/yyyy format. 
#
# The hour label must be preceded by an 'X' or the code cannot order data 
# chronologically. 
#
# If the data is not eligible for PPA interpolation (according to the 1% rule),
# the code will show an error.
#===============================================================================

# Set working directory
setwd("Q:/RESEARCH/Escapement Estimation/Estimations/PPA/Kwiniuk Tower/Updated Data/PPA Analysis - 2001-2020/PPA Automated")

# Source the PPA function
source("fun/ppa_fun.R")

# Import raw data
pink <- read.csv('data/pink_2019.csv')

# Interpolate data
pink.int <- ppa.fun(pink)

# Save .xlsx workbook
saveWorkbook(pink.int[[2]], 'out/pink_int_2019_v2.xlsx')
