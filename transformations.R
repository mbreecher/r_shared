collapsed_opportunities <- function(most_active = F){
  setwd("C:/R/workspace/shared")
  source("import_functions.r")
  source("transformations.r")
  source("helpers.R")
  
  opps <- import_opportunities()
  services <- import_services()
  if(most_active){
    collapsed_time_df <- collapsed_time_with_most_active(complete = F)
  }else{
    collapsed_time_df <- collapsed_time_simple(complete = F)  
  }
  opps <- opps[,!names(opps) %in% names(collapsed_time_df)[!names(collapsed_time_df) %in% "Account.Name"]]
  
  result <- merge_check(collapsed_time_df[!is.na(collapsed_time_df$OpportunityLineItem.Id),], 
                        opps[!is.na(opps$Line.Item.18.Digit.Id) & !opps$Line.Item.18.Digit.Id %in% c(""),], 
                        by.x = c("Opportunity..Opportunity.18.Digit.Id", "OpportunityLineItem.Id", "Account.Name"), 
                        by.y = c("Opportunity.18.Digit.Id", "Line.Item.18.Digit.Id", "Account.Name"), all.x = T)
  
  #temp abigail changes
  setwd("C:/R/workspace/archive/Ali")
  price_update <- read.csv("abigail_price_updates.csv", header = T, stringsAsFactors = F)
  print(paste("abigail_price_updates.csv", "last updated", round(difftime(Sys.time(), file.info("abigail_price_updates.csv")$ctime, units = "days"), digits = 1), "days ago", sep = " "))
  check <- c()
  for (id in unique(price_update$Services.ID)){
    if(length(result[result$Services.ID %in% id,]$Services.ID) > 0){
      result[result$Services.ID %in% id,]$List.Price <- price_update[price_update$Services.ID %in% id,]$list_price_updated
      result[result$Services.ID %in% id,]$Sales.Price <- price_update[price_update$Services.ID %in% id,]$sales_price_updated 
    }
  }
  
  names(result)[names(result) %in% "Account.Name"] <- "Opportunity.Account.Name"
  service_target <- services[,names(services) %in% c("Account.Name", "Services.ID")]
  result <- merge_check(result, service_target, by = "Services.ID")
  
  result$monthyear <- format(result$filing.estimate, format = "%y-%m")
  
  result
  
}  

collapsed_time_with_billable <- function(include_incomplete = F){
  # include_incomplete filters to include only "Completed" services
  library(reshape2)
  library(plyr)
  
  # Pull in import functions
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  setwd('C:/R/workspace/collapsed_time')
  source("diy_periods.R")
  
  #import services and include customer status = none
  services <- import_services()
  timelog <- import_timelog()
  diy_time <- import_billable() 
  
  #initial exclusions. pre-Q2 2013 time and in-progress or not started services
  if(include_incomplete == F){
    services <- services[services$Status %in% "Completed",]  
  }
  timelog <- timelog[timelog$Date <= Sys.Date() & timelog$Date > as.Date("2013-06-30"),]
  
  collapsed_time <- aggregate(Hours ~ Services.ID, FUN = sum, data = timelog)
  collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)
  
  collapsed_history <- collapsed_history[collapsed_history$filing.estimate > as.Date("2013-06-30"),]
  collapsed_history
  
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

collapsed_time_simple <- function(complete = T){
  
  # Pull in import functions
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  
  #import services and include customer status = none
  services <- import_services(include_hourly = T)
  timelog <- import_timelog()
  
  #initial exclusions. pre-Q2 2012 time and in-progress or not started services
  if(complete){
    services <- services[services$Status %in% "Completed",]  
  }
  
  timelog <- timelog[timelog$Date <= Sys.Date() & timelog$Date >= as.Date("2012-06-30"),]
  
  collapsed_time <- aggregate(Hours ~ Services.ID + role, FUN = sum, data = timelog)
  collapsed_time <- collapsed_time[!collapsed_time$Services.ID %in% "",]
  collapsed_time[collapsed_time$role %in% "PSM",]$role <- "PSM.Hours"
  collapsed_time[collapsed_time$role %in% "TM",]$role <- "TM.Hours"
  collapsed_time <- dcast(collapsed_time, Services.ID ~ role, sum, value.var = "Hours")
  collapsed_time$Hours <- rowSums(collapsed_time[,!names(collapsed_time) %in% c("Services.ID")])
  collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)
  
  collapsed_history <- collapsed_history[collapsed_history$filing.estimate >= as.Date("2012-06-30") &
                                           !is.na(collapsed_history$filing.estimate),]
  collapsed_history
}

collapsed_time_with_most_active <- function(complete = T){
  library(reshape2)
  library(plyr)
  
  # Pull in import functions
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  source("helpers.R")
  
  #import services and include customer status = none
  services <- import_services()
  timelog <- import_timelog()
  
  Q22014_to_date <- seq(as.Date("2014/06/30"), Sys.Date(), by = "day")
  services <- services[services$Filing.Date %in% Q22014_to_date | (services$filing.estimate %in% Q22014_to_date & 
                                                                     is.na(services$Filing.Date)),]
  services <- services[services$filing.estimate <= Sys.Date() | services$Filing.Date <= Sys.Date(),]
  
  if(complete){
    services <- services[services$Status %in% "Completed",]  
  }
  
  #####################
  #  Collapsed Time
  #####################
  agg_time <- aggregate(Hours ~ Services.ID, data = timelog, FUN = sum)
  
  agg_time_by_user <- aggregate(Hours ~ Services.ID + User + role, data = timelog, FUN = sum)
  
  most_active <- ddply(agg_time_by_user[!agg_time_by_user$Services.ID %in% "",], .var = "Services.ID", .fun = function(x){
    highest <- x[x$Hours == max(x$Hours),]$User
    data.frame(Services.ID = unique(x$Services.ID),
               most_active = highest[1],
               psm_time = sum(x[x$role %in% "PSM",]$Hours),
               tm_time = sum(x[x$role %in% "TM",]$Hours)
    )
  })
  
  agg_time <- merge_check(agg_time, most_active, by = "Services.ID", all.x = T)
  
  billable_time <- aggregate(Hours ~ Related.Service.Id, data = timelog, FUN = sum)
  billable_time <- billable_time[!billable_time$Related.Service.Id %in% "",]
  names(billable_time) <- c("Services.ID", "Billable.Hours")
  
  collapsed_time <- merge_check(services, agg_time, by = c("Services.ID"), all.x = T)
  collapsed_time <- merge_check(collapsed_time, billable_time, by = c("Services.ID"), all.x = T)
  
  # computer priors and attach to k_time
  averages <- ddply(collapsed_time[!is.na(collapsed_time$Hours),], 
                    .var = c("Service.Type", "Form.Type", "reportingPeriod"), .fun = function(x){
                      # compute statistics by service and form type
                      data.frame( n = length(x$Hours),
                                  mean = mean(x$Hours),
                                  sd = sd(x$Hours))
                    })
  collapsed_time <- merge_check(collapsed_time, averages, by = c("Service.Type", "Form.Type", "reportingPeriod"), all.x = T)
  collapsed_time$project_normalized_time <- NA
  collapsed_time[!is.na(collapsed_time$sd),]$project_normalized_time <- 
    (collapsed_time[!is.na(collapsed_time$sd),]$Hours - collapsed_time[!is.na(collapsed_time$sd),]$mean)/
    collapsed_time[!is.na(collapsed_time$sd),]$sd
  
  collapsed_time[is.na(collapsed_time$Billable.Hours),]$Billable.Hours <- 0
  collapsed_time[is.na(collapsed_time$Hours),]$Hours <- 0
  collapsed_time
}

timelog_by_week <- function(){
  library(reshape2)
  library(plyr)
  
  # Pull in import functions
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  
  #import services and include customer status = none
  services <- import_services()
  timelog <- import_timelog()
  
  timelog$logged_week_num <- as.numeric(format(timelog$Date, format = "%U"))
  timelog$filing_week_num <- as.numeric(format(timelog$Filing.Date, format = "%U"))
  timelog$yearvar <- as.numeric(format(timelog$Filing.Date, format = "%Y")) -  as.numeric(format(timelog$Date, format = "%Y"))
  timelog$relative_week_num <- timelog$logged_week_num - timelog$filing_week_num - 52*timelog$yearvar
  
  result <- merge(timelog, services[,!names(services) %in% names(timelog)[!names(timelog) %in% c("Services.ID")]], by = c("Services.ID"))
}