#############################################################################
# File name: data_features.R
# Author: Rama Thelagathoti
# Description: feature extraction
############################################################################

############################################################################
# Function: deprs_extract_features
# Description: extract features from depression data
#############################################################################
library(dplyr)

deprs_extract_features <-function(df.deprs, scores)
{
  
  feature_matrix <- NULL
  for (c in 1:length(df.deprs)) {  # loop over 23 cond group
    
    # average activity count for every hour and for each day
    cond1_hourly <- df.deprs[[c]] %>%
      dplyr::mutate(hour = lubridate::hour(timestamp) %/% 1) %>%
      dplyr::group_by(date, hour) %>%
      dplyr::summarise(id = "c1",
                       sum_act = sum(activity),
                       avg_act = round(mean(activity)),
                       sd_act = round(sd(activity)))
    
    cond1_hourly <- as.data.frame(cond1_hourly)
    #glimpse(cond1_hourly)
    
    # overall mean, median and sd for each day
    cond1_daily <- df.deprs[[c]] %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(day_mean_act = round(mean(activity)),
                       day_sd_act = round(sd(activity)),
                       day_med_act = median(activity))
    
    #glimpse(cond1_daily)
    
    #count no of hours in each day
    cond1_hours <- cond1_hourly %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(hr_count = n())
    
    #glimpse(cond1_hours)
    
    # id, day, week
    # h0 -h23 - hourly activity average for each hour. 24 hours (0-23 hours) so f0 to f23
    # sd0 -sd23 - hourly activity SD for each hour. 24 hours (0-23 hours) so f24 to f47
    # mean - mean of whole day activity
    # med - median of whole day activity
    # sd - SD of of whole day activity
    # target label for each observation either 1 or 2. 1- control group, 2-condition group
    feature_names <- c("id","day", "week", 
                       "h0", "h1", "h2", "h3", "h4", "h5","h6", "h7", "h8", "h9", "h10", 
                       "h11", "h12", "h13","h14", "h15", "h16", "h17", "h18", "h19", "h20", 
                       "h21", "h22", "h23",
                       "sd0", "sd1", "sd2", "sd3", "sd4", "sd5","sd6", "sd7", "sd8", "sd9", "sd10", 
                       "sd11", "sd12", "sd13","sd14", "sd15", "sd16", "sd17", "sd18", "sd19", "sd20", 
                       "sd21", "sd22", "sd23",
                       "mean", "med", "sd","group"
    )
    
    #initialize before loop
    ncols <- 55 # id + date + day + 51 features from 0 to 50 + group
    nrows <- scores[c,]$days
    features <- matrix( ,nrow = nrows,ncol = ncols)
    colnames(features) <- feature_names
    
    #head(features)
    
    counter <- 0
    for (i in 1:scores[c,]$days) { # loop over number of days for each subject
      
      features[i,1] <- c
      features[i,2] <-  i  #as.character(cond1_daily[i,]$date)
      features[i,3] <-  weekdays(cond1_daily[i,]$date)
      #print(i)
      for (j in 1:cond1_hours$hr_count[i]) {
        
        #avg activity count for each hour f0-f23
        features[i,(4 + cond1_hourly[j + counter,]$hour)] <- cond1_hourly[j + counter,]$avg_act  
        
        #SD of each hour activity f24-f47
        features[i,(4 + 24 + cond1_hourly[j + counter,]$hour)] <- cond1_hourly[j + counter,]$sd_act  
        
        #print(i)
        #print( 4 + cond1_hourly[j + counter,]$hour )
        #print( 4 + 24 + cond1_hourly[j + counter,]$hour )
        #print(j+counter)
      }    
      counter <- counter + cond1_hours$hr_count[i]
      #print(counter)
      
      features[i,52] <- cond1_daily[i,]$day_mean_act  
      features[i,53] <- cond1_daily[i,]$day_med_act
      features[i,54] <- cond1_daily[i,]$day_sd_act  
      
      #id 1-23 condition group
      #id 24-55 control group
      #create target label for each id
      # labels:condition group - 1, control group - 2
      
      features[i,55] <- 1  
    }
    
    feature_matrix <- rbind(feature_matrix,features)
    
  }
  
  deprs_grp <- as.data.frame(feature_matrix)
  #str(deprs_grp)
  #deprs_grp [6:15,]
  #colSums(is.na(deprs_grp))
  #glimpse of condition group with all features
  #dplyr::sample_n(deprs_grp, 10)
  
  deprs_grp$id <- as.numeric(deprs_grp$id)
  deprs_grp$week  <- as.factor(deprs_grp$week)
  deprs_grp$week <- factor(deprs_grp$week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  deprs_grp <- deprs_grp %>% mutate_if(is.character, as.numeric)
  
  return(deprs_grp)
}


############################################################################
# Function: cont_extract_features
# Description: extract features from control group
#############################################################################

cont_extract_features <- function(df.cont, scores)
{
  
  feature_matrix <- NULL
  for (c in 1:length(df.cont)) {
    
    # average activity count for every hour and for each day
    cont_hourly <- df.cont[[c]] %>%
      dplyr::mutate(hour = lubridate::hour(timestamp) %/% 1) %>%
      dplyr::group_by(date, hour) %>%
      dplyr::summarise(id = "c1",
                       sum_act = sum(activity),
                       avg_act = round(mean(activity)),
                       sd_act = round(sd(activity)))
    
    cont_hourly <- as.data.frame(cont_hourly)
    #glimpse(cond1_hourly)
    
    # overall mean, median and sd for each day
    cont_daily <- df.cont[[c]] %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(day_mean_act = round(mean(activity)),
                       day_sd_act = round(sd(activity)),
                       day_med_act = median(activity))
    
    #glimpse(cond1_daily)
    
    #count no of hours in each day
    cont_hours <- cont_hourly %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(hr_count = n())
    
    #glimpse(cond1_hours)
    
    # id, day, week
    # h0 -h23 - hourly activity average for each hour. 24 hours (0-23 hours) so f0 to f23
    # sd0 -sd23 - hourly activity SD for each hour. 24 hours (0-23 hours) so f24 to f47
    # mean - mean of whole day activity
    # med - median of whole day activity
    # sd - SD of of whole day activity
    # target label for each observation either 1 or 2. 1- control group, 2-condition group
    feature_names <- c("id","day", "week", 
                       "h0", "h1", "h2", "h3", "h4", "h5","h6", "h7", "h8", "h9", "h10", 
                       "h11", "h12", "h13","h14", "h15", "h16", "h17", "h18", "h19", "h20", 
                       "h21", "h22", "h23",
                       "sd0", "sd1", "sd2", "sd3", "sd4", "sd5","sd6", "sd7", "sd8", "sd9", "sd10", 
                       "sd11", "sd12", "sd13","sd14", "sd15", "sd16", "sd17", "sd18", "sd19", "sd20", 
                       "sd21", "sd22", "sd23",
                       "mean", "med", "sd","group"
    )
    
    #initialize before loop
    ncols <- 55 # id + date + day + 51 features from 0 to 50 + group
    nrows <- scores[(c + 23),]$days
    features <- matrix( ,nrow = nrows,ncol = ncols)
    colnames(features) <- feature_names
    
    #head(features)
    
    counter <- 0
    for (i in 1:scores[(c + 23),]$days) {
      
      #change control group ids from 1,2,3 etc to 24,25,26 etc
      #1-23 condition group, 24-55 control group
      features[i,1] <- (c+23)
      features[i,2] <-  i   #as.character(cont_daily[i,]$date)
      features[i,3] <-  weekdays(cont_daily[i,]$date)
      #print(i)
      for (j in 1:cont_hours$hr_count[i]) {
        
        #avg activity count for each hour f0-f23
        features[i,(4 + cont_hourly[j + counter,]$hour)] <- cont_hourly[j + counter,]$avg_act  
        
        #SD of each hour activity f24-f47
        features[i,(4 + 24 + cont_hourly[j + counter,]$hour)] <- cont_hourly[j + counter,]$sd_act  
        
        #print(i)
        #print( 4 + cond1_hourly[j + counter,]$hour )
        #print( 4 + 24 + cond1_hourly[j + counter,]$hour )
        #print(j+counter)
      }    
      counter <- counter + cont_hours$hr_count[i]
      #print(counter)
      
      features[i,52] <- cont_daily[i,]$day_mean_act  
      features[i,53] <- cont_daily[i,]$day_med_act
      features[i,54] <- cont_daily[i,]$day_sd_act
      
      #id 1-23 condition group
      #id 24-55 control group
      #create target label for each id
      # labels:condition group - 1, control group - 2
      
      features[i,55] <- 2  
    }
    
    feature_matrix <- rbind(feature_matrix,features)
    
  }
  
  
  cont_grp <- as.data.frame(feature_matrix)
  #str(cont_grp)
  #cont_grp [6:15,]
  #colSums(is.na(cont_grp))
  
  cont_grp$id <- as.numeric(cont_grp$id)
  cont_grp$week  <- as.factor(cont_grp$week)
  cont_grp$week <- factor(cont_grp$week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  cont_grp <- cont_grp %>% mutate_if(is.character, as.numeric)
  return(cont_grp)
  
}


############################################################################
# Function: schz_extract_features
# Description: extract features from schz group
#############################################################################
schz_extract_features <- function(df.schz,df.schz.info)
{
  feature_matrix <- NULL
  for (c in 1:length(df.schz)) {

        # average activity count for every hour and for each day
    cont_hourly <- df.schz[[c]] %>%
      dplyr::mutate(hour = lubridate::hour(timestamp) %/% 1) %>%
      dplyr::group_by(date, hour) %>%
      dplyr::summarise(id = "c1",
                       sum_act = sum(activity),
                       avg_act = round(mean(activity)),
                       sd_act = round(sd(activity)))
    
    cont_hourly <- as.data.frame(cont_hourly)
    #glimpse(cond1_hourly)
    
    # overall mean, median and sd for each day
    cont_daily <- df.schz[[c]] %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(day_mean_act = round(mean(activity)),
                       day_sd_act = round(sd(activity)),
                       day_med_act = median(activity))
    
    #glimpse(cond1_daily)
    
    #count no of hours in each day
    cont_hours <- cont_hourly %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(hr_count = n())
    
    #glimpse(cond1_hours)
    
    # id, day, week
    # h0 -h23 - hourly activity average for each hour. 24 hours (0-23 hours) so f0 to f23
    # sd0 -sd23 - hourly activity SD for each hour. 24 hours (0-23 hours) so f24 to f47
    # mean - mean of whole day activity
    # med - median of whole day activity
    # sd - SD of of whole day activity
    # target label for each observation either 1 or 2. 1- control group, 2-condition group
    feature_names <- c("id","day", "week", 
                       "h0", "h1", "h2", "h3", "h4", "h5","h6", "h7", "h8", "h9", "h10", 
                       "h11", "h12", "h13","h14", "h15", "h16", "h17", "h18", "h19", "h20", 
                       "h21", "h22", "h23",
                       "sd0", "sd1", "sd2", "sd3", "sd4", "sd5","sd6", "sd7", "sd8", "sd9", "sd10", 
                       "sd11", "sd12", "sd13","sd14", "sd15", "sd16", "sd17", "sd18", "sd19", "sd20", 
                       "sd21", "sd22", "sd23",
                       "mean", "med", "sd","group"
    )
    
    #initialize before loop
    ncols <- 55 # id + date + day + 51 features from 0 to 50 + group
    nrows <- df.schz.info[c ,]$days
    features <- matrix( ,nrow = nrows,ncol = ncols)
    colnames(features) <- feature_names
    
    #head(features)
    
    counter <- 0
    for (i in 1:df.schz.info[c,]$days) {
      
      #1-23 condition group, 24-55 control group, 56-77 schizophrenia
      features[i,1] <- (c+55)
      features[i,2] <-  i   #as.character(cont_daily[i,]$date)
      features[i,3] <-  weekdays(cont_daily[i,]$date)
      #print(i)
      for (j in 1:cont_hours$hr_count[i]) {
        
        #avg activity count for each hour f0-f23
        features[i,(4 + cont_hourly[j + counter,]$hour)] <- cont_hourly[j + counter,]$avg_act  
        
        #SD of each hour activity f24-f47
        features[i,(4 + 24 + cont_hourly[j + counter,]$hour)] <- cont_hourly[j + counter,]$sd_act  
        
        #print(i)
        #print( 4 + cond1_hourly[j + counter,]$hour )
        #print( 4 + 24 + cond1_hourly[j + counter,]$hour )
        #print(j+counter)
      }    
      counter <- counter + cont_hours$hr_count[i]
      #print(counter)
      
      features[i,52] <- cont_daily[i,]$day_mean_act  
      features[i,53] <- cont_daily[i,]$day_med_act
      features[i,54] <- cont_daily[i,]$day_sd_act
      
      #id 1-23 condition group
      #id 24-55 control group
      #create target label for each id
      # labels:condition group - 1, control group - 2
      
      features[i,55] <- 2  
    }
    
    feature_matrix <- rbind(feature_matrix,features)
    
  }
  
  
  schz_grp <- as.data.frame(feature_matrix)
  #str(cont_grp)
  #cont_grp [6:15,]
  #colSums(is.na(cont_grp))
  
  schz_grp$id <- as.numeric(schz_grp$id)
  schz_grp$week  <- as.factor(schz_grp$week)
  schz_grp$week <- factor(schz_grp$week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  schz_grp <- schz_grp %>% dplyr::mutate_if(is.character, as.numeric)
  
  return(schz_grp)
  
}