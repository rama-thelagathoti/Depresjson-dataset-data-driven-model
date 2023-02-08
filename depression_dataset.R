
##################
# File name: depression_dataset.R
# Author: Rama Thelagathoti
# Description: Depression dataset processing
##################


##################################################################
# Function: 
# Description: outlier detection and normalizing
# Input: 
# Output: 
##################################################################

prepare_ddepression_data <- function()
{
  #prepare datasets path
  score_file <- list.files(path="data", pattern="*.csv", full.names=TRUE)
  score_file
  
  cond_path <- "data/condition"
  cond_files_list = list.files(path=cond_path, pattern="*.csv", full.names=TRUE)
  cond_files_list <- mixedsort(cond_files_list)
  cond_files_list
  
  cont_path <- "data/control"
  cont_files_list = list.files(path=cont_path, pattern="*.csv", full.names=TRUE)
  cont_files_list <- mixedsort(cont_files_list)
  cont_files_list
  
  #read scores dataset
  scores <- read_csv("data/scores.csv")
  #process scores dataset
  scores <- prepare_scores(scores)
  
  dplyr::sample_n(scores,10)
  str(scores)
  colSums(is.na(scores))
  
  
  cond_scores <- scores[1:23, ]
  cont_scores <- scores[23:55, ]
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(3)
  
  print("------------------ madrs comparision before and after data recording-----------------")
  
  #plot madrs before and after assessment
  cond_data <- t(data.frame(cond_scores$madrs1,cond_scores$madrs2))
  barplot(cond_data, 
          legend = c("madrs1", "madrs2"), 
          names.arg=cond_scores$number, 
          beside=TRUE,
          col=pal,
          xlab="number", 
          ylab="madrs score", 
          main="madrs scores comparision", 
          ylim=c(0,40)
  )
  
  print("-------------- plot scores dataset -----------------")
  
  plot_scores(scores)
  
  
  scores$type <- NA
  scores$type[1:23] <- "condition"
  scores$type[24:55] <- "control"
  scores$type <- as.factor(scores$type)
  
  print("------------------ read condition and control data sets -----------------")
  
  ###### condition and control dataset ##########
  cond_group = "condition"
  cond_total = 23
  cond_path <- "data/condition"
  cond_files_list = list.files(path=cond_path, pattern="*.csv", full.names=TRUE)
  cond_files_list <- mixedsort(cond_files_list)
  cond_files <- list()
  for (i in 1:length(cond_files_list))
  {
    cond_files[[i]]<-read.csv(cond_files_list[i])
    
  }
  dplyr::sample_n(cond_files[[1]],10)
  #str(cond_files[[1]])
  #summary(cond_files[[1]])
  #glimpse(cond_files[[2]])
  
  cont_group = "control"
  cont_total = 32
  cont_path <- "data/control"
  cont_files_list = list.files(path=cont_path, pattern="*.csv", full.names=TRUE)
  cont_files_list <- mixedsort(cont_files_list)
  cont_files <- list()
  for (i in 1:length(cont_files_list))
  {
    cont_files[[i]]<-read.csv(cont_files_list[i])
  }
  
  
  print("------------------  prepare conditon and control data sets -----------------")
  
  #convert date variable of type char to date type
  
  for (i in 1:length(cond_files_list))
  {
    cond_files[[i]]$date<- as.Date(cond_files[[i]]$date)
  }
  
  for (i in 1:length(cont_files_list))
  {
    cont_files[[i]]$date<- as.Date(cont_files[[i]]$date)
  }
  
  # In each data set in condition & control,
  # we will consider the data only for the number of days mentioned in scores dataset
  
  for (i in 1:length(cond_files_list))
  {
    no_of_days <- scores[i,]$days 
    cond_files[[i]] <- cond_files[[i]] %>% filter( date <= (min(cond_files[[i]]$date) + no_of_days) )
    cond_files[[i]]$time <- as.POSIXct(cond_files[[i]]$timestamp) 
    cond_files[[i]]$time = parse_date_time(cond_files[[i]]$time, orders = "%Y-/%m-/%d %H:%M:%S")
    cond_files[[i]]$time <- format(cond_files[[i]]$time, format = "%H:%M:%S")
    #cond_files[[i]]$time <- as.numeric(hms(cond_files[[i]]$time))
    #print(colSums(is.na(cond_files[[i]])))
    
  }
  
  
  for (i in 1:length(cont_files_list))
  {
    no_of_days <- scores[i + cond_total,]$days 
    cont_files[[i]] <- cont_files[[i]] %>% filter( date <= (min(cont_files[[i]]$date) + no_of_days) )
    cont_files[[i]]$time <- as.POSIXct(cont_files[[i]]$timestamp) 
    cont_files[[i]]$time = parse_date_time(cont_files[[i]]$time, orders = "%Y-/%m-/%d %H:%M:%S")
    cont_files[[i]]$time <- format(cont_files[[i]]$time, format = "%H:%M:%S")
    #cont_files[[i]]$time <- as.numeric(hms(cont_files[[i]]$time))
    #print(colSums(is.na(cont_files[[i]])))
  }
  
  #glimpse(cond_files[[1]])
  
  print("------------------  visualize condition patient 1 activity -----------------")
  plot_cond1_activity(cond_files[[1]])
  
  
  print("------------------  remove first day activity from both datasets -----------------")
  ########## remove first day activity from all the datasets ##########
  # on first activity started during mid of the day, so remove first day activity
  
  for (i in 1:length(cond_files_list)) {
    
    cond_files[[i]] <- subset(cond_files[[i]], cond_files[[i]]$date > min(cond_files[[i]]$date))
  }
  
  for (i in 1:length(cont_files_list)) {
    
    cont_files[[i]] <- subset(cont_files[[i]], cont_files[[i]]$date > min(cont_files[[i]]$date))
  }
  scores$days <- scores$days -1
  
  
  #condition / control group variables
  dplyr::sample_n(cond_files[[1]],10)
  
  
}