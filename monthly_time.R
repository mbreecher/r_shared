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
  
  timelog$xbrl_status <- ""
  timelog$xbrl_form <- ""
  timelog[timelog$Billable %in% 1,]$xbrl_status <- "DIY"
  ptm <- proc.time()
  service_status <- ddply(timelog, .var = c("Account.Name", "Date"), .fun = function(x){
                                    type <- "DIY"
                                    form <- ""
                                    current_types <- unique(services[services$Account.Name %in% unique(x$Account.Name) & 
                                                          services$Quarter.End <= unique(x$Date) &
                                                          services$filing.estimate >= unique(x$Date) &
                                                          !is.na(unique(x$Date)),]$Service.Type)
                                    current_forms <- unique(services[services$Account.Name %in% unique(x$Account.Name) & 
                                                                       services$Quarter.End <= unique(x$Date) &
                                                                       services$filing.estimate >= unique(x$Date) &
                                                                       !is.na(unique(x$Date)),]$Form.Type)
                                    
                                    if(length(current_types) > 0){
                                      if(TRUE %in% (current_types %in% full_service_types)){
                                        type <- "Full Service"
                                      }else if(TRUE %in% (current_types %in% c("Maintenance"))){
                                        type <- "Basic"
                                      }
                                      if(TRUE %in% (current_forms %in% c("10-K", "K-K", "Q-K"))){
                                        form <- "K"
                                      }else if(TRUE %in% (current_forms %in% c("10-Q", "Q-Q", "K-Q"))){
                                        form <- "Q"
                                      }
                                    }else{
                                      if(TRUE %in% (x$Service.Type %in% full_service_types)){
                                        type <- "Full Service"
                                      }else if(TRUE %in% (x$Service.Type %in% c("Maintenance"))){
                                        type <- "Basic"
                                      }
                                      if(TRUE %in% (x$Form.Type %in% c("10-K", "K-K", "Q-K"))){
                                        form <- "K"
                                      }else if(TRUE %in% (x$Form.Type %in% c("10-Q", "Q-Q", "K-Q"))){
                                        form <- "Q"
                                      }
                                    }
                                    
                                    data.frame(xbrl_status = type, form_type = form)
                                })
  proc.time() - ptm
  
  export <- merge(timelog, service_status, by = c("Account.Name", "Date"))
  export <- aggregate(Hours ~ monthyear +  xbrl_status + Billable + , data = export, FUN = sum)
  
  export
}