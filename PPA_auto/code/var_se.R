#===============================================================================
# Calculate variance for tower counts
#
# Date: March 25, 2022
# 
# Creator: Luke Henslee
#
# Purpose: This code takes hourly escapement data for tower counts
# and calculates hourly, daily, and seasonal error
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
setwd()

# Source the PPA function
source("fun/var_fun.R")

# Import raw data
data <- read.csv('data/pink_kwiniuk_2020.csv', 
                 colClasses = c('character', rep('numeric', length.out = 24)))

# Interpolate data
data.int <- var.fun(data)

# Save .xlsx workbook in the 'out' folder
## Name the file: 'species_project_var_year.xlsx'
saveWorkbook(data.int[[2]], 'out/pink_kwiniuk_var_2020.xlsx')
