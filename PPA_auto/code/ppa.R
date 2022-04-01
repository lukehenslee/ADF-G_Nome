#===============================================================================
# PPA interpolation for tower counts
#
# Last updated: March 23, 2022
# 
# Creator: Luke Henslee
#
# Purpose: This code takes hourly escapement data for tower counts
# and interpolates missed counts and missed days using PPA methods
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
setwd("Q:/RESEARCH/Escapement Estimation/Estimations/PPA/PPA_auto")

# Source the PPA function
source("fun/ppa_fun.R")

# Import raw data
data <- read.csv('data/kwiniuk/chum_1995.csv', 
                 colClasses = c('character', rep('numeric', length.out = 24)))

# Interpolate data
data.int <- ppa.fun(data)

# Save .xlsx workbook in the 'out' folder
## Name the file: 'species_project_ppa_year.xlsx'
saveWorkbook(data.int[[2]], 'out/chum_kwiniuk_ppa_1995.xlsx')
