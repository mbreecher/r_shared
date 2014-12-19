timelog_with_status <- function(){
  library(reshape2)
  library(plyr)
  library(RecordLinkage)
  
  # Pull in import functions
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  
  #import services and include customer status = none
  services <- import_services()
  timelog <- import_timelog()
  
  #initial exclusions. pre-Q2 2013 time and in-progress or not started services
  services <- services[services$Status %in% "Completed",]
  timelog <- timelog[timelog$Date <= Sys.Date() & timelog$Date >= as.Date("2013-06-30"),]
  
  timelog$monthyear <- format(timelog$Date, format = "%y-%m")
  
  full_service_types <- c("Standard Import","Full Service Roll Forward", "Roll Forward", "Detail Tagging", "Full Service Standard Import")
  
  service_status <- ddply(timelog, .var = c("Account.Name", "Date"), .fun = function(x){
                                    type <- "DIY"
                                    current_types <- unique(services[services$Account.Name %in% unique(x$Account.Name) & 
                                                          services$Quarter.End <= unique(x$Date) &
                                                          services$filing.estimate >= unique(x$Date) &
                                                          !is.na(unique(x$Date)),]$Service.Type)
                                    #browser()
                                    if(length(current_types) > 0){
                                      for (i in 1:length(current_types)){
                                        if(TRUE %in% (current_types %in% full_service_types)){
                                          type <- "Full Service"
                                        }else if(TRUE %in% (current_types %in% c("Maintenance"))){
                                          type <- "Basic"
                                        }
                                      }  
                                    }
                                    
                                    data.frame(xbrl_status = type)
                                })
  
  export <- merge(timelog, service_status, by = c("Account.Name", "Date"))
  export <- aggregate(Hours ~ Account.Name + monthyear +  xbrl_status, data = export, FUN = sum)
  
  export
}