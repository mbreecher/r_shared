summary_stats <- function(startDate = seq(Sys.Date(), by = "-1 year", length = 2)[2], endDate = Sys.Date()){
  # default value is the prior 12 months
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  source("transformations.R")
  source("helpers.R")
  
  collapsed_opps <- collapsed_opportunities(most_active = T)
  timelog <- import_timelog()
  #Construct the Period Identifiers for service grouping
  timelog$expectedFilingPeriod <- paste(as.numeric(format(timelog$Date, "%Y")), ceiling(as.numeric(format(timelog$Date, "%m"))/3), sep = "")
  timelog$expectedReportingPeriod <- ifelse(substr(timelog$expectedFilingPeriod, nchar(timelog$expectedFilingPeriod ), nchar(timelog$expectedFilingPeriod )) == 1,
                                            paste(as.numeric(format(timelog$Date, "%Y")) -1, 4, sep = ""),
                                            paste(as.numeric(format(timelog$Date, "%Y")), ceiling(as.numeric(format(timelog$Date, "%m"))/3) - 1, sep = ""))
  
  setwd("C:/R/workspace/source/")
  pricebook <- read.csv("pricebook.csv", stringsAsFactors = F)
  print(paste("pricebook.csv", "last updated", round(difftime(Sys.time(), file.info("pricebook.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  pricebook <- pricebook[pricebook$Price.Book.Name %in% "Cloud Suite Workiva Price Book",]
  pricebook <- pricebook[,names(pricebook) %in% c("Service.Type", "Form.Type", "List.Price")]
  collapsed_opps <- merge_check(collapsed_opps, pricebook, by = c("Service.Type", "Form.Type"), all.x = T)
  
  collapsed_opps$List.Price <- pmax(collapsed_opps$List.Price.x, collapsed_opps$List.Price.y, na.rm = T)
  co2015 <- collapsed_opps[collapsed_opps$Filing.Date %in% seq(startDate, endDate,1) |
                             collapsed_opps$filing.estimate %in% seq(startDate, endDate,1) |
                             collapsed_opps$Filing.Deadline %in% seq(startDate, endDate,1),]
  
  # value generation index
  # core assumption 1: person with the most activity was the most responsible for the success of the service.
  agg_value <- aggregate(List.Price ~ most_active + Account.Name + reportingPeriod + Services.ID + Service.Type + Form.Type, co2015, sum)
  agg_value <- agg_value[!agg_value$Service.Type %in% "Migration",]
  agg_time <- aggregate(Hours ~ User + reportingPeriod, timelog, sum)
  agg_billable <- aggregate(Hours ~ User + reportingPeriod, 
                            timelog[timelog$Service.Type %in% c("Fixed Fee Overage", "Hourly Service", "Reserve Hours") &
                                      timelog$Billable %in% 1,], sum)
  
  K_season_dollars <- ddply(agg_value, .var = c("most_active","reportingPeriod"), .fun  = function(x){
    data.frame(count = dim(x)[1],
               dollars = sum(x$List.Price))
  })
  
  K_season_billable <- ddply(agg_billable, .var = c("User","reportingPeriod"), .fun  = function(x){
    data.frame(count = dim(x)[1],
               dollars = sum(x$Hours) * 250)
  })

  roles <- timelog[timelog$Date %in% seq(seq(startDate, by = "-3 months", length = 2)[2], endDate,1),]
  roles <- unique(roles[,names(roles) %in% c("User","role", "reportingPeriod")])
  
  K_season_dollars <- K_season_dollars[rev(order(K_season_dollars$dollars)),]
  
  K_season_dollars_wide <- dcast(K_season_dollars, most_active ~ reportingPeriod, sum, value.var = "dollars")
  K_season_billable_wide <- dcast(K_season_billable, User ~ reportingPeriod, sum, value.var = "dollars")
  
  K_season_stats <- ddply(K_season_dollars, .var = c("most_active", "reportingPeriod"), .fun = function(x){
    data.frame(service_count = x$count,
               maintenance_count = dim(agg_value[agg_value$reportingPeriod %in% x$reportingPeriod &
                                                   agg_value$most_active %in% x$most_active &
                                                   agg_value$Service.Type %in% "Maintenance",])[1],
               full_service_count = dim(agg_value[agg_value$reportingPeriod %in% x$reportingPeriod &
                                                    agg_value$most_active %in% x$most_active &
                                                    !agg_value$Service.Type %in% c("Maintenance", "Migration"),])[1],
               service_dollars = sum(x$dollars),
               hourly_dollars = sum(K_season_billable[K_season_billable$User %in% x$most_active &
                                                        K_season_billable$reportingPeriod %in% x$reportingPeriod ,]$dollars),
               booked_dollars = sum(x$dollars, K_season_billable[K_season_billable$User %in% x$most_active &
                                                                   K_season_billable$reportingPeriod %in% x$reportingPeriod ,]$dollars),
               client_time = sum(timelog[timelog$User %in% x$most_active &
                                           timelog$expectedReportingPeriod %in% x$reportingPeriod ,]$Hours))  
  })
  
  K_season_stats
}
