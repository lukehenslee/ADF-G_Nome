#===============================================================================
# PPA interpolation for tower counts
#
# Last updated: March 23, 2022
# 
# Creator: Luke Henslee, ADF&G
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
#
# Simply change the name of the file in the read.csv() function to the name of 
# the file you want to calculate missed passage. The name should be:
#
# 'species_project_year.csv'
#
# And then change the name of the output file to:
#
# 'species_project_ppa_year.xlsx'
#
#===============================================================================

