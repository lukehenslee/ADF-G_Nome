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
setwd("C:/Users/lhhenslee/Desktop/Git_repos/ADF-G_Nome/PPA_auto")

# Source the PPA function
source("code/ppa_fun.R")

# Import raw data
data <- read.csv('data/pink_2019.csv')

# Interpolate data
data.int <- ppa.fun(data)

# Save .xlsx workbook
saveWorkbook(data.int[[2]], 'out/pink_int_2019_v2.xlsx')
