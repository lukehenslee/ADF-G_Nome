#===============================================================================
# Caculate error
#
# Date: March 23, 2022
# 
# Creator: Luke Henslee
#
# Purpose: This code calculates error for escapement towers
#===============================================================================
# NOTES: 
#===============================================================================

# Set working directory

# Load packages ####
require(tidyverse)
require(openxlsx)
require(data.table)

# Function ####

var.fun <- function(data) {
  
  # Manipulate data
  data1 <- pivot_longer(data, cols = c(2:25), names_to = "hour")
  data1$hour <- substr(data1$hour, 2, 5)
  data1 <- data1 %>% 
    slice(which.max(is.na(value) == F) : n())
  data1 <- data1[order(nrow(data1):1), ]
  data1 <- data1 %>% 
    slice(which.max(is.na(value) == F) : n())
  data1 <- data1[order(nrow(data1):1), ]
  data1$date <- lubridate::mdy(data1$date)
  
  # Daily totals
  d.total <- data1 %>% group_by(date) %>% summarize_at(vars(value), sum, na.rm = T)
  
  # Season total
  total <- sum(data1$value, na.rm = T)
  
  # Hourly totals
  h.seasonal <- data1 %>% group_by(hour) %>% 
    summarize_at(vars(value), sum, na.rm = T) 
  
  # Hourly proportions
  h.seasonal$prop <- h.seasonal$value/total 
  
  # Split by date into lists
  d.list <- split(data1, data1$date)
  
  # Iterate through days- if there are NA's and they are eligible, they are 
  # interpolated 
  
  # Counter for missed counts
  missed <- matrix(data = NA, nrow = length(d.list), ncol = 24)
  
  for(i in 1:length(d.list)) {
    for(j in 1:length(d.list[[i]][[3]]))
      if(is.na(d.list[[i]][[3]][j]) == T) {
        missed[i,j] <- NA
      } else {
        missed[i,j] <- 1
      }
  }
  
  # Convert list to a data1 frame
  h.total <- do.call(rbind.data.frame, d.list)
  
  # Calculate hourly variance
  var <- vector()
  
  # We don't calculate var for 0000
  for(i in 2:nrow(h.total)) {
    if(h.total[i,2] == '0000') {
      var[i] <- NA
    } else {
      var[i] <- sum(as.numeric((h.total[(i-1),3]/3 - h.total[i,3]/3)^2), na.rm = TRUE) 
      # Need to code this so if value to the left is NA, then that day gets an NA, otherwise calculate
    }
  }
  
  # Turn into a matrix
  var.mat <- cbind(h.total, var)
  
  var.mat <- pivot_wider(var.mat[,c(1:2, 4)], names_from = 'hour', values_from = 'var')
  
  var.mat <- var.mat %>% 
    select(order(colnames(var.mat))) %>% 
    select(date, everything())
  
  # Total hourly var
  # First calculate md for each day
  md <- vector()
  
  for(i in 1:(nrow(missed))) {
    md[i] <- sum(!is.na(missed[i,])) 
  }
  
  var.mat$md <- md
  
  s2d <- vector()
  
  for(i in 1:nrow(missed)) {
    s2d[i] <- sum(var.mat[i,c(3:25)])/(2 * (md[i] - 1))
  }
  
  var.mat$s2d <- s2d
  
  # Calculate total daily variance 
  var.day <- vector()
  
  for(i in 1:nrow(var.mat)) {
    var.day[i] <- (1 - md[i]/72)*72^2*(s2d[i]/md[i])
  }
  
  var.mat$var.day <- var.day
  
  var.mat <- var.mat[,c(1, 28,26, 27, 3:25)]
  
  # Calculate total nhat and variance and se
  var.se <- data.frame(nhat = sum(d.total$value),
                       var = sum(var.mat$var.day, na.rm = T),
                       se = sqrt(sum(var.mat$var.day, na.rm = T)))
  
  d.total$var <- var.mat$var.day
  d.total$var[is.nan(d.total$var)] <- NA
  
  # Write .csv
  out <- createWorkbook()
  
  addWorksheet(out, 'daily variance')
  addWorksheet(out, 'hourly variance')
  addWorksheet(out, 'total variance and se')
  addWorksheet(out, 'raw data')
  
  writeData(out, sheet = 'daily variance', x = d.total)
  writeData(out, sheet = 'hourly variance', x = var.mat)
  writeData(out, sheet = 'total variance and se', x = var.se)
  writeData(out, sheet = 'raw data', x = data)
  
  return(list(h.total, out))
  
}
