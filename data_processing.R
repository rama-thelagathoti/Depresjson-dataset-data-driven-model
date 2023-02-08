#############################################################################
# File name: data_processing.R
# Author: Rama Thelagathoti
# Description: datasets processing
############################################################################

############################################################################
# Function: Prepare_scores
# Description: process scores dataset and extract relevant information
#############################################################################

prepare_scores <- function()
{
  
  scores <- read_csv("data/scores.csv")
  
  #fill all NAs with "missing"
  scores <- scores %>% mutate_all(~replace(., is.na(.), "missing"))
  
  #convert each participant as factor variable
  scores$number <- as.factor(scores$number)
  
  scores$days <- as.numeric(scores$days)
  
  #convert gender to be a factor variable
  scores$gender[scores$gender == 1] <- "female"
  scores$gender[scores$gender == 2] <- "male"
  scores$gender <- as.factor(scores$gender)
  
  #age
  scores$age <- as.factor(scores$age)
  
  #afftype
  scores$afftype[scores$afftype == 1] <- "bipolar"
  scores$afftype[scores$afftype == 2] <- "unipolar"
  scores$afftype[scores$afftype == 2] <- "unipolar"
  scores$afftype <- as.factor(scores$afftype)
  
  #melanch
  scores$melanch[scores$melanch == 1] <- "yes"
  scores$melanch[scores$melanch == 2] <- "no"
  scores$melanch <- as.factor(scores$melanch)
  
  #inpatient 
  scores$inpatient[scores$inpatient == 1] <- "inpatient"
  scores$inpatient[scores$inpatient == 2] <- "outpatient"
  scores$inpatient <- as.factor(scores$inpatient)
  
  #education
  scores$edu <- as.factor(scores$edu)
  
  #marriage
  scores$marriage[scores$marriage == 1] <- "married"
  scores$marriage[scores$marriage == 2] <- "single"
  scores$marriage <- as.factor(scores$marriage)
  
  #work
  scores$work[scores$work == 1] <- "working"
  scores$work[scores$work == 2] <- "notworking"
  scores$work <- as.factor(scores$work)
  
  #madras_delta = madrs1 - madrs2
  scores$madrs1 <- as.numeric(scores$madrs1)
  scores$madrs2 <- as.numeric(scores$madrs2)
  
  #fill all NAs with "missing"
  scores <- scores %>% mutate_all(~replace(., is.na(.), 0))
  
  scores$madrs_delta <- abs(scores$madrs1 - scores$madrs2)
  
  scores$type <- NA
  scores$type[1:23] <- "condition"
  scores$type[24:55] <- "control"
  scores$type <- as.factor(scores$type)
  
  scores <- as.data.frame(scores)
  
  return(scores)
  
}


############################################################################
# Function: prepare_schz_info
# Description: process deprs group in depression dataset
#############################################################################
prepare_schz_info <- function()
{
  
  schz_dat <- read_csv("data/patients_info.csv")
  schz_dat$gender <- as.factor(schz_dat$gender)
  
  return(schz_dat)
}

merge_scores <- function()
{
  
  scored.merged <- read_csv("data/scores_merged.csv")
  
  #fill all NAs with "missing"
  scored.merged <- scored.merged %>% mutate_all(~replace(., is.na(.), "missing"))
  
  #convert each participant as factor variable
  scored.merged$number <- as.factor(scored.merged$number)
  
  scored.merged$days <- as.numeric(scored.merged$days)
  
  #convert gender to be a factor variable
  scored.merged$gender[scored.merged$gender == 1] <- "female"
  scored.merged$gender[scored.merged$gender == 2] <- "male"
  scored.merged$gender <- as.factor(scored.merged$gender)
  
  #age
  scored.merged$age <- as.factor(scored.merged$age)
  
  #afftype
  scored.merged$afftype[scored.merged$afftype == 1] <- "bipolar"
  scored.merged$afftype[scored.merged$afftype == 2] <- "unipolar"
  scored.merged$afftype[scored.merged$afftype == 2] <- "unipolar"
  scored.merged$afftype <- as.factor(scored.merged$afftype)
  
  #melanch
  scored.merged$melanch[scored.merged$melanch == 1] <- "yes"
  scored.merged$melanch[scored.merged$melanch == 2] <- "no"
  scored.merged$melanch <- as.factor(scored.merged$melanch)
  
  #inpatient 
  scored.merged$inpatient[scored.merged$inpatient == 1] <- "inpatient"
  scored.merged$inpatient[scored.merged$inpatient == 2] <- "outpatient"
  scored.merged$inpatient <- as.factor(scored.merged$inpatient)
  
  #education
  scored.merged$edu <- as.factor(scored.merged$edu)
  
  #marriage
  scored.merged$marriage[scored.merged$marriage == 1] <- "married"
  scored.merged$marriage[scored.merged$marriage == 2] <- "single"
  scored.merged$marriage <- as.factor(scored.merged$marriage)
  
  #work
  scored.merged$work[scored.merged$work == 1] <- "working"
  scored.merged$work[scored.merged$work == 2] <- "notworking"
  scored.merged$work <- as.factor(scored.merged$work)
  
  #madras_delta = madrs1 - madrs2
  scored.merged$madrs1 <- as.numeric(scored.merged$madrs1)
  scored.merged$madrs2 <- as.numeric(scored.merged$madrs2)
  
  #fill all NAs with "missing"
  scored.merged <- scored.merged %>% mutate_all(~replace(., is.na(.), 0))
  
  scored.merged$madrs_delta <- abs(scored.merged$madrs1 - scored.merged$madrs2)
  
  scored.merged$type <- NA
  scored.merged$type[1:23] <- "depres"
  scored.merged$type[24:55] <- "control"
  scored.merged$type[56:77] <- "schz"
  scored.merged$type <- as.factor(scored.merged$type)
  
  scored.merged$schtype <- as.factor(scored.merged$schtype)
  scored.merged$migraine <- as.factor(scored.merged$migraine)
  scored.merged$cloz <- as.factor(scored.merged$cloz)
  scored.merged$trad <- as.factor(scored.merged$trad)
  scored.merged$moodst <- as.factor(scored.merged$moodst)
  scored.merged$bprs <- as.numeric(scored.merged$bprs)
  scored.merged$agehosp <- as.numeric(scored.merged$agehosp)
  
  scored.merged <- as.data.frame(scored.merged)
  
  return(scored.merged)
  
}
############################################################################
# Function: prepare_depr_data
# Description: process deprs group in depression dataset
#############################################################################

prepare_deprs_data <- function(scores)
{
  
  deprs_group = "depression"
  deprs_total = 23
  deprs_path <- "data/depression"
  deprs_files_list = list.files(path=deprs_path, pattern="*.csv", full.names=TRUE)
  deprs_files_list <- mixedsort(deprs_files_list)
  deprs_files <- list()
  for (i in 1:length(deprs_files_list))
  {
    deprs_files[[i]]<-read.csv(deprs_files_list[i])
    
  }
  #convert date variable of type char to date type
  for (i in 1:length(deprs_files_list))
  {
    deprs_files[[i]]$date<- as.Date(deprs_files[[i]]$date)
  }
  # consider the data only for the number of days mentioned in scores data set
  # ignore other days
  for (i in 1:length(deprs_files_list))
  {
    no_of_days <- scores[i,]$days 
    deprs_files[[i]] <- deprs_files[[i]] %>% filter( date <= (min(deprs_files[[i]]$date) + no_of_days) )
    deprs_files[[i]]$time <- as.POSIXct(deprs_files[[i]]$timestamp) 
    deprs_files[[i]]$time = parse_date_time(deprs_files[[i]]$time, orders = "%Y-/%m-/%d %H:%M:%S")
    deprs_files[[i]]$time <- format(deprs_files[[i]]$time, format = "%H:%M:%S")

  }
  
  # on first activity started during mid of the day, so remove first day activity
  for (i in 1:length(deprs_files_list)) {
    
    deprs_files[[i]] <- subset(deprs_files[[i]], deprs_files[[i]]$date > min(deprs_files[[i]]$date))
  }
  
  
  
  return(deprs_files)  
}

############################################################################
# Function: prepare_schz_data
# Description: process deprs group in depression dataset
#############################################################################
prepare_schz_data <- function(df.schz.info)
{
  prev_total = 55
  schz_group = "schizophrenia"
  schz_total = 22
  schz_path <- "data/schizophrenia"
  schz_files_list = list.files(path=schz_path, pattern="*.csv", full.names=TRUE)
  schz_files_list <- mixedsort(schz_files_list)
  schz_files <- list()
  for (i in 1:length(schz_files_list))
  {
    schz_files[[i]]<-read.csv(schz_files_list[i])
  }
  
  #convert date variable of type char to date type
  for (i in 1:length(schz_files_list))
  {
    schz_files[[i]]$date<- as.Date(schz_files[[i]]$date)
  }
  
  # consider the data only for the number of days mentioned in scores dataset
  for (i in 1:length(schz_files_list))
  {
    no_of_days <- df.schz.info[i,]$days 
    schz_files[[i]] <- schz_files[[i]] %>% filter( date <= (min(schz_files[[i]]$date) + no_of_days) )
    schz_files[[i]]$time <- as.POSIXct(schz_files[[i]]$timestamp) 
    schz_files[[i]]$time = parse_date_time(schz_files[[i]]$time, orders = "%Y-/%m-/%d %H:%M:%S")
    schz_files[[i]]$time <- format(schz_files[[i]]$time, format = "%H:%M:%S")
  }
  # on first activity started during mid of the day, so remove first day activity
  for (i in 1:length(schz_files_list)) {
    
    schz_files[[i]] <- subset(schz_files[[i]], schz_files[[i]]$date > min(schz_files[[i]]$date))
  }
  return(schz_files)
  
}

############################################################################
# Function: prepare_cont_data
# Description: process deprs group in depression dataset
#############################################################################
prepare_cont_data <- function(scores)
{
  deprs_total = 23
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
  #convert date variable of type char to date type
  
  for (i in 1:length(cont_files_list))
  {
    cont_files[[i]]$date<- as.Date(cont_files[[i]]$date)
  }
  
  # consider the data only for the number of days mentioned in scores dataset
  
    for (i in 1:length(cont_files_list))
  {
    no_of_days <- scores[i + deprs_total,]$days 
    cont_files[[i]] <- cont_files[[i]] %>% filter( date <= (min(cont_files[[i]]$date) + no_of_days) )
    cont_files[[i]]$time <- as.POSIXct(cont_files[[i]]$timestamp) 
    cont_files[[i]]$time = parse_date_time(cont_files[[i]]$time, orders = "%Y-/%m-/%d %H:%M:%S")
    cont_files[[i]]$time <- format(cont_files[[i]]$time, format = "%H:%M:%S")
  }
  # on first activity started during mid of the day, so remove first day activity
  for (i in 1:length(cont_files_list)) {
    
    cont_files[[i]] <- subset(cont_files[[i]], cont_files[[i]]$date > min(cont_files[[i]]$date))
  }
  return(cont_files)
}
