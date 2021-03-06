weekyear_to_written <- function(old_names){
  new_names <- c()
  all_dates <- seq(as.Date("2012-01-01"), Sys.Date() + 90, by = 1)
  all_dates <- data.frame(date = as.Date(all_dates), weekyear = format(all_dates, format = "%y-%U"))
  for (name in old_names){
    if(grepl("-",name)){
      
      loop <- all_dates[all_dates$weekyear %in% substr(name,1,5),]
      
      new_names = c(new_names, 
                    paste(format(as.Date(unique(min(loop$date))), format = "%m/%d"),
                          "-",
                          format(as.Date(unique(max(loop$date))), format = "%m/%d"),
                          substr(name,6,nchar(name)),sep = "")
                    )
    }else{
      new_names = c(new_names, name)
    }
  }
  new_names
}

weekyear_to_written_with_year <- function(old_names){
  new_names <- c()
  all_dates <- seq(as.Date("2012-01-01"), Sys.Date() + 30, 1)
  all_dates <- data.frame(date = as.Date(all_dates), weekyear = format(all_dates, format = "%y-%U"))
  for (name in old_names){
    if(grepl("-",name)){
      
      loop <- all_dates[all_dates$weekyear %in% substr(name,1,5),]
      
      new_names = c(new_names, 
                    paste(format(as.Date(unique(min(loop$date))), format = "%m/%d"),
                          "-",
                          format(as.Date(unique(max(loop$date))), format = "%m/%d/%Y"),
                          substr(name,6,nchar(name)),sep = "")
      )
    }else{
      new_names = c(new_names, name)
    }
  }
  new_names
}

monthyear_to_written <- function(old_names){
  new_names <- c()
  for (name in old_names){
    if(grepl("-",name)){
      new_names = c(new_names, paste(format(as.Date(paste(substr(name,1,5),"-01", sep = "")), format = "%b-%y"),
                                     substr(name,6,nchar(name)), sep = ""))
    }else{
      new_names = c(new_names, name)
    }
  }
  new_names
}

weekyear_to_written_with_weekyear_and_long_year_min_max <- function(old_names){
  new_names <- c()
  all_dates <- seq(as.Date("2012-01-01"), Sys.Date() + 300, 1)
  all_dates <- data.frame(date = as.Date(all_dates), weekyear = format(all_dates, format = "%y-%U"))
  for (name in old_names){
    if(grepl("-",name)){
      
      loop <- all_dates[all_dates$weekyear %in% substr(name,1,5),]
      
      new_names = c(new_names, 
                    paste(name,
                          "\n",
                          as.Date(unique(min(loop$date))),
                          "\n",
                          as.Date(unique(max(loop$date))),
                          sep = "")
      )
    }else{
      new_names = c(new_names, name)
    }
  }
  new_names
}
