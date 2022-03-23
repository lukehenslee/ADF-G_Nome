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
setwd("C:/Users/lhhenslee/Desktop/Git_repos/ADF-G_Nome/PPA_auto")

# Function ####

ppa.fun <- function(data) {
  
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
  
  # I DON'T THINK I NEED THIS ####
  # Test for interpolation eligibility 
  if(d.total[1,2]/total> 0.01 |
     d.total[2,2]/total> 0.01 |
     d.total[3,2]/total> 0.01) {
    stop("The begining of the run is not fully monitored and the data1 is not eligible for interpolation and quartile reporting")
  } else print("The begining of the run is fully monitored and the data1 is eligible for interpolation and quartile reporting")
  
  if(d.total[nrow(d.total)-2,2]/sum(d.total[c(1:(nrow(d.total)-2)),2]) > 0.01 |
     d.total[nrow(d.total)-1,2]/sum(d.total[c(1:(nrow(d.total)-1)),2]) > 0.01 |
     d.total[nrow(d.total),2]/sum(d.total[c(1:(nrow(d.total))),2]) > 0.01) {
    stop("The end of the run is not fully monitored and the data1 is not eligible for interpolation and quartile reporting")
  } else print("The end of the run is fully monitored and the data1 is eligible for interpolation and quartile reporting")
  
  # Find continuous counts that cover 80% of hourly proportions
  f <- function(df, thresh) {
    v <- df$prop
    idxStart <- which.max(v)
    if (v[idxStart] < thresh) {
      len <- length(v)
      len1 <- len - 1L
      vcumsum <- cumsum(c(v, v[1:len1]))
      v1 <- tail(vcumsum, -1)
      v0 <- c(0, vcumsum[1:len1])
      for (span in 1:len1) {
        isum <- v1[span:(len1 + span)] - v0
        idxStart <- which.max(isum)
        if (isum[idxStart] > thresh) break
      }
      idxEnd <- idxStart + span
      if (idxEnd > len) {
        return(df[c(idxStart:len, seq(idxEnd %% len)),])
      } else {
        return(df[idxStart:idxEnd,])
      }
    } else {
      return(df[idxStart,]) 
    }
  }
  
  h.range <- f(h.seasonal, 0.8)
  # I DON'T THINK I NEED THIS ####
  
  
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
  
  # REMOVE? ##################
  # Now we iterate through the count in each day and interpolate
  
  for(i in 2:(length(d.list)-1)) {
    for(j in 1:length(d.list[[i]][[3]]))
      if(is.na(d.list[[i]][[3]][j]) == T &&
         sum(h.seasonal[!is.na(missed[i,]), 3]) > 0.25 &&
         nrow(intersect(h.range[,1], h.seasonal[!is.na(missed[i,]), 1]))/nrow(h.range) > 0.25) {
        d.list[[i]][[3]][j] <- as.numeric((sum(d.list[[i]][[3]][!is.na(missed[i,])]) * h.seasonal[j, 3])/
                                            (sum(h.seasonal[!is.na(missed[i,]),3])))
      } else {
        if(is.na(d.list[[i]][[3]][j]) == T &&
           sum(h.seasonal[!is.na(missed[i,]), 3]) < 0.25 |
           nrow(intersect(h.range[,1], h.seasonal[!is.na(missed[i,]), 1]))/nrow(h.range) < 0.25) {
          d.list[[i]][[3]] <- NA
        } else {
          d.list[[i]][[3]][j] <- d.list[[i]][[3]][j]
        }
      }
  }
  # REMOVE? ##################
  
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
    md[i] <- length(missed[i,]) * sum(h.seasonal[!is.na(missed[i,]),3]) # This takes proportional periods missed, but
    # it really needs to take actual number of periods missed
  }
  
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
  
    # REMOVE ############
  # Sum daily totals
  d.total2 <- h.total %>% 
    group_by(date) %>% 
    summarize(value = sum(value))
  

  # Interpolate missed days  
  
  # Make a vector of days that are missed
  missed.day <- vector()
  
  for(i in 1:length(d.list)) {
    if(sum(is.na(d.list[[i]][[3]])) == length(d.list[[i]][[3]])) {
      missed.day[i] <- 0
    } else {
      missed.day[i] <- 1
    }
  }
  
  # Convert to data1.table to create group ID
  missed.day <- as.data1.table(missed.day) 
  
  # Create group ID
  missed.day[, id := rleid(missed.day)]
  
  # Record number of observations in each group
  missed.day <- missed.day %>% 
    group_by(id) %>% 
    mutate(count = n())
  
  # Convert back to a data1 frame
  missed.day <- as.data1.frame(missed.day) 
  
  # Iterate through all days and interpolate total missed days if eligible
  
  d.total.int <- vector()
  
  for(i in 1:nrow(d.total)) {
    if(
      # Test if the whole day was missed
      missed.day[i,1] == 0 && 
      # Test it is less than the number of consecutive days counted after missed period
      rle(missed.day[c(i:nrow(missed.day)), 1])[[1]][1] <= 
      rle(missed.day[c(i:nrow(missed.day)), 1])[[1]][2] && 
      # Test it is less than the number of consecutive days counted before missed period
      rle(missed.day[c(i:nrow(missed.day)), 1])[[1]][1] <= 
      rle(missed.day[c(i:1), 1])[[1]][2]) {
      # And if it passes both criteria....
      d.total.int[i] <- (
        # Interpolate using rolling average
        sum(d.total2[c((i - (missed.day[i,3])) : i), 2], na.rm = T) + 
          sum(d.total2[c(i : (i + (missed.day[i,3]))), 2], na.rm = T)
      ) / (missed.day[i,3] + 1)
      # We will update the daily variance while we are here
      var.day[i] <- max(var.day[c( ((i - (missed.day[i,3])) : i), (i : (i + (missed.day[i,3]))))], na.rm = T)
    } else 
      # If criteria are not met then daily total stays the same
    {
      d.total.int[i] <- as.numeric(d.total2[i,2])
      # Note that if the full day is missing and there are not enough days before
      # and/or after, the daily total will remain 'NA'
    }
  }
  
  # Add interpolations to daily totals
  d.total2$int <- d.total.int
  
  # Add daily variance to var.mat
  var.mat$var.day <- var.day
  # REMOVE ############
  
  # Calculate total nhat and variance and se
  var.se <- data1.frame(nhat = sum(d.total$value),
                       var = sum(var.mat$var.day, na.rm = T),
                       se = sqrt(sum(var.mat$var.day, na.rm = T)))
  
  # REMOVE ####
  # Now create .csv output
  h.total <- pivot_wider(h.total, names_from = 'hour', values_from = 'value')
  
  h.total <- h.total %>% 
    select(order(colnames(h.total))) %>% 
    select(date, everything())
  
  # Add daily totals
  h.total$daily.totals <- d.total2$int
  
  # Add hourly totals
  h.total$date <- as.character(h.total$date)
  h.total[(nrow(h.total) + 1),] <- summarise_all(h.total, ~if(is.numeric(.)) { sum(., na.rm = T)} else "total")
  # REMOVE ####
  
  d.total$var <- var.mat$var.day
  d.total$var[is.nan(d.total$var)] <- NA
  
  # Write .csv
  out <- createWorkbook()
  
  addWorksheet(out, 'daily variance')
  addWorksheet(out, 'hourly variance')
  addWorksheet(out, 'total variance and se')
  addWorksheet(out, 'raw data')
  
  writedata1(out, sheet = 'daily variance', x = d.total)
  writedata1(out, sheet = 'hourly variance', x = var.mat)
  writedata1(out, sheet = 'total variance and se', x = var.se)
  writedata1(out, sheet = 'raw data', x = data)
  
  return(list(h.total, out))
  
}
