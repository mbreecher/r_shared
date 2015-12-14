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
  start_dates <- readRDS("ps_start_dates.Rda")
  
  # add 
  
  result <- data.frame(User = user_vector, Date = date_vector, User.Title = title_vector, is_psm = 0)

  ## result[result$User %in% start_dates[is.na(start_dates$Start.Date) & is.na(start_dates$End.Date), ]$Full.Name, ]$is_psm <- 1 #psms who are still in PS
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
  ps_titles <- c("Professional Services Managers", "PSM", "Senior Team Manager, Professional Services", "PSM Team Manager")
  if(length(result[result$User.Title %in% ps_titles & result$is_psm %in% "0", ]$is_psm) > 0){
    result[result$User.Title %in% ps_titles, ]$is_psm <- 1
  }
  
  #add unknown PSMs to start dates dataframe
  if(length(unique(result[result$User.Title %in% ps_titles & !result$User %in% start_dates$Full.Name, ]$User)) > 0){
    print("new psms detected. Importing Timelog and updating ps start dates...")
    setwd("C:/R/workspace/shared")
    source("import_functions.r")
    timelog <- import_timelog(skip_role = T)
    
    for(name in unique(result[result$User.Title %in% ps_titles & !result$User %in% start_dates$Full.Name, ]$User)){
      print(paste("name added to start dates: ", name), sep = "")
      if(dim(timelog[timelog$User %in% name,])[1] > 0){
        start_dates <- rbind(start_dates, 
                                  data.frame(Full.Name = name, 
                                             Start.Date = min(timelog[timelog$User %in% name & !is.na(timelog$Hours),]$Date), 
                                             End.Date = NA))  
      }else{
        print("no service time logged")
      }
    }
    setwd("c:/r/workspace/source")
    saveRDS(start_dates, file = "ps_start_dates.Rda")
  }
  
  
  result$is_psm
}

unify_alias <- function(user_vector = NULL){
  if(is.null(user_vector)){
    print ("user vector required")
    break
  }
  setwd("C:/R/workspace/source")
  alias <- readRDS("alias.Rda")
  alias <- alias[order(alias$Date),]
  for(row in 1:dim(alias)[1]){
    user_vector[user_vector %in% alias[row, 1]] <- alias[row, 2]
  }
  user_vector
}

role <- function(user_vector = NULL, date_vector = NULL, title_vector = NULL, is_psm_vector = NULL){
  if(is.null(user_vector) | is.null(date_vector) | is.null(title_vector) | is.null(is_psm_vector)){
    print ("name, date, title, and is_psm vectors required for role determination")
    break
  }
  if(!length(user_vector) == length(date_vector) |
      !length(user_vector) == length(title_vector) |
       !length(user_vector) == length(is_psm_vector)){
    print ("name, date, title, and is_psm vectors must be the same length")
    break
  }
  
  result = data.frame(User = user_vector, Date = date_vector, User.Title = title_vector, is_psm = is_psm_vector)
  
  #get role dates
  setwd("C:/R/workspace/source")
  promotions <- readRDS("promotions.Rda")
  promotions <- promotions[order(promotions$date),]
  
  #set all time to PSM when the title says psm or sr psm
  result$role <- NA
  if(length(result[result$User.Title %in% unique(result$User.Title)[grep("Professional", unique(result$User.Title))],]$role) > 0){
    result[result$User.Title %in% unique(result$User.Title)[grep("Professional", unique(result$User.Title))],]$role <- "PSM"  
  }
  if(length(result[result$User.Title %in% unique(result$User.Title)[grep("PSM", unique(result$User.Title))],]$role) > 0){
    result[result$User.Title %in% unique(result$User.Title)[grep("PSM", unique(result$User.Title))],]$role <- "PSM"  
  }
  result[result$is_psm %in% 1,]$role <- "PSM"
  
  #for each promotion event, set role forward to new role
  for (i in 1:length(promotions$Full.Name)){
    psm <- promotions$Full.Name[i]
    effective_date <- promotions$date[i]
    if(length(result[result$User %in% psm & result$Date >= effective_date, ]$role) > 0){
      result[result$User %in% psm & result$Date >= effective_date, ]$role <- promotions$role[i]
    }
  }
  
  result$role
}

sequence_yearweeks <- function(min, max, by){
  if(min <= max){
    loop = min
    result <- c()
    repeat{
      result <- c(result, loop)
      if(as.numeric(substr(loop, 4,5)) + by >= 52){
        loop <- paste(as.numeric(substr(loop, 1,2))+1, sprintf("%02d", (as.numeric("00") + by)%% 52),  sep = "-")
      }else{
        loop <- paste(substr(loop, 1,2),sprintf("%02d", as.numeric(substr(loop, 4,5))+by), sep = "-")
      }
      if(loop > max){
        return(result)
        break
      }
    }
  }else{
    print('arguments aren\'t properly ordered')
    return (result)
    break
  }
}

sequence_yearquarters <- function(min, max, by){
  if(min <= max){
    loop = min
    result <- c()
    repeat{
      result <- c(result, loop)
      if((as.numeric(substr(loop, 5,5)) + by) > 4){
        loop <- paste(as.numeric(substr(loop, 1,4))+1, by %% 4,  sep = "")
      }else{
        loop <- paste(substr(loop, 1,4),as.numeric(substr(loop, 5,5))+by, sep = "")
      }
      if(loop > max){
        return(result)
        break
      }
    }
  }else{
    print('arguments aren\'t properly ordered')
    return (result)
    break
  }
}