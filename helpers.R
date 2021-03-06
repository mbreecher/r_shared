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
  
  #case 1: unknown PSMs
  if(dim(result[grepl("Professional", result$User.Title, ignore.case = T), ])[1] >0){
    result[grepl("Professional", result$User.Title, ignore.case = T), ]$is_psm <- 1    
  }
  if(dim(result[grepl("PSM", result$User.Title, ignore.case = T), ])[1] > 0){
    result[grepl("PSM", result$User.Title, ignore.case = T), ]$is_psm <- 1  
  }

  #case 2: known PSMs
  ## result[result$User %in% start_dates[is.na(start_dates$Start.Date) & is.na(start_dates$End.Date), ]$Full.Name, ]$is_psm <- 1 #psms who are still in PS
  for (psm in start_dates$Full.Name) {#need to subset for each psm
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$Start.Date)){
      if(length(result[result$User %in% psm & !is.na(result$Date) & result$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm) > 0){
        result[result$User %in% psm & !is.na(result$Date) & result$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm <- 1
      }
    }
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$End.Date)){
      if(length(result[result$User %in% psm & !is.na(result$Date) & result$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm) > 0){
        result[result$User %in% psm & !is.na(result$Date) & result$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm <- 0
      }
    }
  }
  
  #add unknown PSMs to start dates dataframe
  ps_titles <- unique(result[unique(c(grep("Professional", result$User.Title, ignore.case = T), 
                                      grep("PSM", result$User.Title, ignore.case = T))), ]$User.Title)
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
  if(length(result[result$is_psm %in% "1",]$role) >0){
    result[result$is_psm %in% "1",]$role <- "PSM"  
  }
  if(length(result[result$is_psm %in% "0",]$role) >0){
    result[result$is_psm %in% "0",]$role <- NA
  }
  
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

sequence_yearweeks <- function(min, max){
  if(min <= max){
    start = as.Date(paste0("20", substr(min,1,2), "-01-01"))
    end = as.Date(paste0("20", substr(max,1,2), "-12-31"))
    result <- unique(format(seq(start, end, 1), format = "%y-%U"))
    result <- result[result >= min & result <= max]
    return (result)
  }else{
    print('arguments aren\'t properly ordered')
    return (result)
    break
  }
}

sequence_yearquarters <- function(min, max){
  if(min <= max){
    loop = min
    result <- c()
    repeat{
      result <- c(result, loop)
      if((as.numeric(substr(loop, 5,5)) + 1) > 4){
        loop <- paste(as.numeric(substr(loop, 1,4)) + 1, 1,  sep = "")
      }else{
        loop <- paste(substr(loop, 1,4),as.numeric(substr(loop, 5,5))+1, sep = "")
      }
      if(loop > max){
        return(result)
        break
      }
    }
  }else{
      loop = min
      result <- c()
      repeat{
        result <- c(result, loop)
        if((as.numeric(substr(loop, 5,5)) - 1) < 1){
          loop <- paste(as.numeric(substr(loop, 1,4)) - 1, 4,  sep = "")
        }else{
          loop <- paste(substr(loop, 1,4),as.numeric(substr(loop, 5,5))-1, sep = "")
        }
        if(loop < max){
          return(result)
          break
        }
      }
    }
  }


merge_check <- function(x, y, ...){
  # wrap and mask merge to print source and result sizes and check for expansion
  result <- merge(x, y, ...)
  if(dim(x)[1] == dim(result)[1]){
    print("source and destination are the same size")
  }else{
    print(paste0("sourceA is: ", dim(x)[1], " sourceB is: ", dim(y)[1], " and result is: ", dim(result)[1], " which is a difference of ", dim(result)[1] - dim(x)[1]))
  }
  result
}

clean_up_openair_names <- function(names){
  names <- gsub("- ","",names)
  names <- gsub("[[:punct:]]"," ",names)
  names <- gsub("[ ]{1,}",".",names)
  names <- gsub("[.]$","",names, perl = T)
  names
}

import_and_merge_updated_time <- function(source_df, update_file_name){
  setwd("C:/R/workspace/shared")
  source("import_functions.r")
  update_df <- import_openair_time(name = update_file_name, include_cs = T)
  update_df <- aggregate(Time.Hours ~ ., data = update_df, sum)
  print(paste("Adding ",  dim(update_df)[1], " rows to ", dim(source_df)[1], "rows in existing dataframe. Expect ", dim(update_df)[1] + dim(source_df)[1] ))
  result <- rbind(source_df, update_df)
  source_df <- aggregate(Time.Hours ~ ., data = result, max)
  print(paste("result is ", dim(source_df)[1]))
  source_df
}