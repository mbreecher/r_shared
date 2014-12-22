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
  
  timelog$xbrl_status <- c("")
  timelog[timelog$Billable %in% c("1"),]$xbrl_status <- c("DIY")
  ptm <- proc.time()
  service_status <- ddply(timelog, .var = c("Account.Name", "Date"), .fun = function(x){
                                    type <- "DIY"
                                    form <- ""
                                    year_end <- NA
                                    #for account-date set, carve out services whose 'filing window' would line up with this time
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
                                        form <- c("K")
                                      }else if(TRUE %in% (current_forms %in% c("10-Q", "Q-Q", "K-Q"))){
                                        form <- c("Q")
                                      }
                                    }else{
                                      if(TRUE %in% (x$Service.Type %in% full_service_types)){
                                        type <- "Full Service"
                                      }else if(TRUE %in% (x$Service.Type %in% c("Maintenance"))){
                                        type <- "Basic"
                                      }
                                      if(TRUE %in% (x$Form.Type %in% c("10-K", "K-K", "Q-K"))){
                                        form <- c("K")
                                      }else if(TRUE %in% (x$Form.Type %in% c("10-Q", "Q-Q", "K-Q"))){
                                        form <- c("Q")
                                      }
                                    }
                                    if(form %in% c("")){
                                      if (length(unique(services[services$Account.Name %in% x$Account.Name & !(services$Year.End %in% c("     ")),]$Year.End)) > 0){
                                            year_end <- as.Date(unique(services[services$Account.Name %in% x$Account.Name & !(services$Year.End %in% c("     ")),]$Year.End), format = "%m/%d")
                                            form <- c("Q")
                                             if(as.numeric(unique(x$Date) - year_end)%%365 >= 360 & as.numeric(unique(x$Date) - year_end)%%365 <= 95 & 
                                                  !is.na(unique(x$Date)) & !is.na(year_end)){
                                               form <- c("K")
                                             }
                                      }
                                    }
                                    
                                    data.frame(xbrl_status = type, form_type = form)
                                })
  proc.time() - ptm
  
  export <- merge(timelog, service_status, by = c("Account.Name", "Date"))
  #export <- aggregate(Hours ~ monthyear +  xbrl_status + Billable + form_type, data = export, FUN = sum)
  
  export
}