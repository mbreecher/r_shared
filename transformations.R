collapsed_opps <- function(){
  setwd("C:/R/workspace/shared")
  source("import_functions.r")
  source("transformations.r")
  
  opps <- import_opportunities()
  collapsed_time <- collapsed_time()
  
  result <- merge(opps[!is.na(opps$Line.Item.18.Digit.Id) & !opps$Line.Item.18.Digit.Id %in% c(""),], 
                  collapsed_time[,!names(collapsed_time) %in% names(opps)], 
                  by.x = c("Line.Item.18.Digit.Id"), by.y = c("OpportunityLineItem.Id"))
  result
  
} 

collapsed_time <- function(){
  library(reshape2)
  library(plyr)
  library(RecordLinkage)
  
  # Pull in import functions
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  setwd('C:/R/workspace/collapsed_time')
  source("diy_periods.R")
  
  #import services and include customer status = none
  services <- import_services()
  timelog <- import_timelog()
  diy_time <- import_billable() 
  
  #initial exclusions. pre-Q2 2012 time and in-progress or not started services
  services <- services[services$Status %in% "Completed",]
  timelog <- timelog[timelog$Date <= Sys.Date() & timelog$Date >= as.Date("2012-06-30"),]
  
  collapsed_time <- aggregate(Hours ~ Services.ID, FUN = sum, data = timelog)
  collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)
  
  collapsed_history <- collapsed_history[collapsed_history$filing.estimate >= as.Date("2012-06-30"),]
  
  
  pts <- proc.time()
  count.unique <- function(x) { length(unique(x[!is.na(x)])) } #function to count unique non-NA values
  
  #I need customer period to merge diy time
  customer_period <- ddply(collapsed_history,
                           .var = c("Services.ID","OpportunityLineItem.Id", "CIK", "Account.Name", "filing.estimate", "Service.Name", "Service.Type", "Form.Type", "Quarter.End"),
                           .fun = function(x) {
                             
                             # Grab the year end from the services
                             x_ye <- unique(x$Year.End)
                             x_ye <- as.Date(paste(year(unique(x$filing.estimate)),x_ye, sep = "/"), format = "%Y/%m/%d")
                             
                             qd <- as.numeric((unique(x$filing.estimate)-x_ye)/90)%%4 #quarter difference from year end
                             pqd <- as.numeric((unique(x$filing.estimate)-x_ye - 90)/90)%%4 #quarter difference from year end (prior quarter)
                             #if(abs(qd > 4)){qd <- qd%%4} #get a mod 4 quarter difference
                             if(!is.na(qd)){if(qd > 0){cq <- ceiling(qd)}else{cq <- floor(qd)}}else{cq <- NA}
                             if(!is.na(pqd)){if(pqd > 0){pcq <- ceiling(pqd)}else{pcq <- floor(pqd)}}else{pcq <- NA}
                             if(cq %in% 0){cq = 4}
                             if(pcq %in% 0){pcq = 4}
                             
                             #actual quarter
                             aq <- paste(year(unique(x$filing.estimate)),"Q", ceiling(as.numeric(month(unique(x$filing.estimate))/3)),  sep = "")
                             #actual reporting quarter
                             arq <- paste(year(unique(x$filing.estimate)-90),"Q", ceiling(as.numeric(month(unique(x$filing.estimate)-90)/3)),  sep = "")
                             
                             if(!is.na(cq) & !(x_ye %in% c("     ")) & !is.na(x_ye)){
                               if(unique(x$filing.estimate) < x_ye){
                                 data.frame(customer_quarter_work_done = paste(year(unique(x$filing.estimate)),"Q",cq, sep = ""), 
                                            customer_quarter_reported = paste(year(unique(x$filing.estimate) - 90),"Q",pcq, sep = ""), 
                                            calendar_quarter_work_done = aq,
                                            calendar_quarter_reported = arq,
                                            year_end = x_ye,
                                            Hours = sum(x$Hours))  
                               }else{
                                 data.frame(customer_quarter_work_done = paste(year(unique(x$filing.estimate))+1,"Q",abs(cq), sep = ""), 
                                            customer_quarter_reported = paste(year(unique(x$filing.estimate)-90)+1,"Q",abs(pcq), sep = ""), 
                                            calendar_quarter_work_done = aq,
                                            calendar_quarter_reported = arq,
                                            year_end = x_ye,
                                            Hours = sum(x$Hours))
                               }
                             }else{
                               data.frame(customer_quarter_work_done = NA, 
                                          customer_quarter_reported = NA,
                                          calendar_quarter_work_done = aq,
                                          calendar_quarter_reported = arq,
                                          year_end = NA,
                                          Hours = sum(x$Hours))
                             }
                             
                             #browser()
                           }
  )
  
  start <- c("Account.Name", "calendar_quarter_reported", "calendar_quarter_work_done", "customer_quarter_reported", "customer_quarter_work_done")
  
  diy_time_simple <- diy_time[,c(start, "Hours")]
  names(diy_time_simple) <- c(start, "Billable.Hours")
  
  export <- merge(customer_period, diy_time_simple, by = start, all = T)
  export <- export[,c(start, names(export)[!(names(export) %in% start)])]
  
  #merge duplicated billable hour counts for customers with more then one concurrent service in a period. Need to correct.
  export$Billable.Hours <- as.numeric(export$Billable.Hours)
  normalized_time <- ddply(export, .var = start,
                           .fun = function(x){
                             count <- length(unique(x$Services.ID))
                             if(count > 1 & !is.na(count)){normalized_time <- x$Billable.Hours/count}else{normalized_time <- x$Billable.Hours}
                             data.frame(services_count = count,
                                        normalized_time <- normalized_time)
                           })
  
  export <- merge(export, normalized_time, by = start, all.x = T )
  names(export)[names(export) %in% c("normalized_time....normalized_time")] <- "normalized_time"
  #some cleanup
  names(export[c("Hours")]) <- c("Project.Hours")
  export <- unique(export)

  export
}

weekly_time <- function(){
  library(reshape2)
  library(plyr)
  library(RecordLinkage)
  
  # Pull in import functions
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  
  #import services and include customer status = none
  services <- import_services()
  timelog <- import_timelog()
  
  timelog$logged_week_num <- week(timelog$Date)
  timelog$filing_week_num <- week(timelog$Filing.Date)
  timelog$relative_week_num <- timelog$logged_week_num - timelog$filing_week_num
  
  result <- merge(timelog, services[,!names(services) %in% names(timelog)[!names(timelog) %in% c("Services.ID")]], by = c("Services.ID"))
}

weekly_time_detail <- function(){
  library(plyr)
  library(RecordLinkage)
  
  # Pull in import functions
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  source("transformations.R")
  
  weekly_time <- weekly_time() #bring in merged time
  weekly_time <- weekly_time[weekly_time$Date >= "2013-06-30",] #remove unreliable time
  
  #get role dates
  setwd("C:/R/workspace/source")
  role_dates <- read.csv("ps_start_dates.csv", header = T, stringsAsFactors = F)
  role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))] <- 
    lapply(role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))],FUN = as.Date, format = "%m/%d/%Y")
  
  #set all time to PSM when the title says psm or sr psm
  weekly_time$role <- NA
  #daily_hours$role <- NA
  weekly_time[weekly_time$User.Title %in% unique(weekly_time$User.Title)[grep("Professional", unique(weekly_time$User.Title))],]$role <- "PSM"
  #daily_hours$role <- "PSM"
  
  # time_started <- proc.time()
  #for those with a start date, set time before to NA
  for (i in 1:length(role_dates[!is.na(role_dates$Start.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$Start.Date),]$Full.Name[i]
    start <- role_dates[!is.na(role_dates$Start.Date),]$Start.Date[i]
    if(length(weekly_time[weekly_time$User %in% psm & weekly_time$Date < start, ]$role) > 0){
      weekly_time[weekly_time$User %in% psm & weekly_time$Date < start, ]$role <- NA
    }
  }
  
  #for psms promoted to senior, set time forward to Sr PSM
  for (i in 1:length(role_dates[!is.na(role_dates$to_senior),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_senior),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_senior),]$to_senior[i]
    if(length(weekly_time[weekly_time$User %in% psm & weekly_time$Date >= promotion_date, ]$role) > 0){
      weekly_time[weekly_time$User %in% psm & weekly_time$Date >= promotion_date, ]$role <- "Sr PSM"
    }
  }
  
  #for srs promoted to tms, set time forward to TM
  for (i in 1:length(role_dates[!is.na(role_dates$to_tm),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_tm),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_tm),]$to_tm[i]
    if(length(weekly_time[weekly_time$User %in% psm & weekly_time$Date >= promotion_date, ]$role) > 0 ){
      weekly_time[weekly_time$User %in% psm & weekly_time$Date >= promotion_date, ]$role <- "TM"
    }
  }
  
  #for tms promoted to director, set time forward to director
  for (i in 1:length(role_dates[!is.na(role_dates$to_director),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_director),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_director),]$to_director[i]
    if(length(weekly_time[weekly_time$User %in% psm & weekly_time$Date >= promotion_date, ]$role) > 0){
      weekly_time[weekly_time$User %in% psm & weekly_time$Date >= promotion_date, ]$role <- "Director"
    }
  }
  
  #for psms who left PS, set time forward to NA
  for (i in 1:length(role_dates[!is.na(role_dates$End.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$End.Date),]$Full.Name[i]
    term_date <- role_dates[!is.na(role_dates$End.Date),]$End.Date[i]
    if(length(weekly_time[weekly_time$User %in% psm & weekly_time$Date >= term_date, ]$role) > 0){
      weekly_time[weekly_time$User %in% psm & weekly_time$Date >= term_date, ]$role <- NA
    }
  }
  
  #set week integer
  weekly_time$week <- paste(year(weekly_time$Date),sprintf("%02d", week(weekly_time$Date)), sep = "-")
  labels <- ddply(weekly_time, .var = c("week"), function(x){
    min <- min(x$Date)
    max <- max(x$Date)
    count <- unique(x$User)
    label <- paste(strftime(min, '%m/%d')," - ", strftime(max, '%m/%d'), sep = "")
    data.frame(week = x$week, 
               label = label)
  })
  labels <- unique(labels)
  weekly_time <- merge(weekly_time, labels, by = c("week"), all.x = T)

}