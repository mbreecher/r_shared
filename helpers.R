is_psm <- function(user_vector = NULL, date_vector = NULL, title_vector = NULL){
  if(is.null(user_vector) | is.null(date_vector) | is.null(title_vector)){
    print ("name, date, and title vectors required for role determination")
    break
  }
  if(!length(user_vector) == length(date_vector) |
       !length(user_vector) == length(title_vector)){
    print ("name, date, and title vectors must be the same length")
    break
  }
  setwd('C:/R/workspace/source')
  start_dates <- read.csv("ps_start_dates.csv", header = T , stringsAsFactors=F)
  print(paste("ps_start_dates.csv", "last updated", round(difftime(Sys.time(), file.info("ps_start_dates.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  
  start_dates$Start.Date <- as.Date(start_dates$Start.Date, format = "%m/%d/%Y")
  start_dates$End.Date <- as.Date(start_dates$End.Date, format = "%m/%d/%Y")
  
  result <- data.frame(User = user_vector, Date = date_vector, User.Title = title_vector, is_psm = 0)

  result[result$User %in% start_dates[is.na(start_dates$Start.Date) & is.na(start_dates$End.Date), ]$Full.Name, ]$is_psm <- 1 #psms who are still in PS
  for (psm in start_dates[!is.na(start_dates$Start.Date) | !is.na(start_dates$End.Date), ]$Full.Name) {#need to subset for each psm
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$Start.Date)){
      if(length(result[result$User %in% psm & !is.na(result$User) & result$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm) > 0){
        result[result$User %in% psm & !is.na(result$User) & result$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm <- 1
      }
      if(length(result[result$User %in% psm & !is.na(result$User) & result$Date < start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm) > 0){
        result[result$User %in% psm & !is.na(result$User) & result$Date < start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm <- 0
      }
    }
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$End.Date)){
      if(length(result[result$User %in% psm & !is.na(result$User) & result$Date <= start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm) > 0){
        result[result$User %in% psm & !is.na(result$User) & result$Date <= start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm <- 1 
      }
      if(length(result[result$User %in% psm & !is.na(result$User) & result$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm) > 0){
        result[result$User %in% psm & !is.na(result$User) & result$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm <- 0
      }
    }
  }
  
  #case 2: unknown PSMs
  ps_titles <- c("Professional Services Managers", "PSM", "Sr. PSM", "PSM Team Manager")
  if(length(result[result$User.Title %in% ps_titles, ]$is_psm) > 0){
    result[result$User.Title %in% ps_titles, ]$is_psm <- 1
  }
  
  result$is_psm
}

role <- function(user_vector = NULL, date_vector = NULL, title_vector = NULL, is_psm_vector = NULL){
  if(is.null(user_vector) | is.null(date_vector) | is.null(title_vector) | is.null(is_psm_vector)){
    print ("name, date, and title vectors required for role determination")
    break
  }
  if(!length(user_vector) == length(date_vector) |
      !length(user_vector) == length(title_vector) |
       !length(user_vector) == length(is_psm_vector)){
    print ("name, date, and title vectors must be the same length")
    break
  }
  
  result = data.frame(User = user_vector, Date = date_vector, User.Title = title_vector, is_psm = is_psm_vector)
  
  #get role dates
  setwd("C:/R/workspace/source")
  role_dates <- read.csv("ps_start_dates.csv", header = T, stringsAsFactors = F)
  print(paste("ps_start_dates.csv", "last updated", round(difftime(Sys.time(), file.info("ps_start_dates.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))] <- 
    lapply(role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))],FUN = as.Date, format = "%m/%d/%Y")
  
  #set all time to PSM when the title says psm or sr psm
  result$role <- NA
  if(length(result[result$User.Title %in% unique(result$User.Title)[grep("Professional", unique(result$User.Title))],]$role) > 0){
    result[result$User.Title %in% unique(result$User.Title)[grep("Professional", unique(result$User.Title))],]$role <- "PSM"  
  }
  if(length(result[result$User.Title %in% unique(result$User.Title)[grep("PSM", unique(result$User.Title))],]$role) > 0){
    result[result$User.Title %in% unique(result$User.Title)[grep("PSM", unique(result$User.Title))],]$role <- "PSM"  
  }
  result[result$is_psm %in% 1,]$role <- "PSM"
  
  #for those with a start date, set time before to NA
  for (i in 1:length(role_dates[!is.na(role_dates$Start.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$Start.Date),]$Full.Name[i]
    start <- role_dates[!is.na(role_dates$Start.Date),]$Start.Date[i]
    if(length(result[result$User %in% psm & result$Date < start, ]$role) > 0){
      result[result$User %in% psm & result$Date < start, ]$role <- NA
    }
  }
  
  #for psms promoted to senior, set time forward to Sr PSM
  for (i in 1:length(role_dates[!is.na(role_dates$to_senior),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_senior),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_senior),]$to_senior[i]
    if(length(result[result$User %in% psm & result$Date >= promotion_date, ]$role) > 0){
      result[result$User %in% psm & result$Date >= promotion_date, ]$role <- "Sr PSM"
    }
  }
  
  #for srs promoted to tms, set time forward to TM
  for (i in 1:length(role_dates[!is.na(role_dates$to_tm),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_tm),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_tm),]$to_tm[i]
    if(length(result[result$User %in% psm & result$Date >= promotion_date, ]$role) > 0 ){
      result[result$User %in% psm & result$Date >= promotion_date, ]$role <- "TM"
    }
  }
  
  #for tms promoted to director, set time forward to director
  for (i in 1:length(role_dates[!is.na(role_dates$to_director),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_director),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_director),]$to_director[i]
    if(length(result[result$User %in% psm & result$Date >= promotion_date, ]$role) > 0){
      result[result$User %in% psm & result$Date >= promotion_date, ]$role <- "Director"
    }
  }
  
  #for people demoted to PSM, set time forward to psm
  for (i in 1:length(role_dates[!is.na(role_dates$back_to_psm),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$back_to_psm),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$back_to_psm),]$back_to_psm[i]
    if(length(result[result$User %in% psm & result$Date >= promotion_date, ]$role) > 0){
      result[result$User %in% psm & result$Date >= promotion_date, ]$role <- "PSM"
    }
  }
  
  #for psms who left PS, set time forward to NA
  for (i in 1:length(role_dates[!is.na(role_dates$End.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$End.Date),]$Full.Name[i]
    term_date <- role_dates[!is.na(role_dates$End.Date),]$End.Date[i]
    if(length(result[result$User %in% psm & result$Date >= term_date, ]$role) > 0){
      result[result$User %in% psm & result$Date >= term_date, ]$role <- NA
    }
  }
  
  result$role
}