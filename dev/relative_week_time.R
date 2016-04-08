import_relative_week_time <- function(startDate = seq(Sys.Date(), by = "-1 year", length = 2)[2], endDate = Sys.Date()){
  setwd("C:/r/workspace/shared/")
  require(plyr)
  source("import_functions.R")
  source("headings.R")
  services <- import_services()
  timelog <- import_timelog()
  
  K_services <- services[services$Quarter.End %in% seq(startDate, endDate, 1),]
  K_services <- K_services[,!names(K_services) %in% names(timelog[,!names(timelog) %in% "Services.ID",])]
  
  k_time <- merge(timelog, K_services, by = "Services.ID")
  k_time$yearweek <- format(k_time$Date, format = "%y-%U")
  
  k_time$yearvar <- as.numeric(format(k_time$filing.estimate, format = "%Y")) -  as.numeric(format(k_time$Date, format = "%Y"))
  k_time$filing_week_num <- as.numeric(format(k_time$filing.estimate, format = "%U"))
  k_time$logged_week_num <- as.numeric(format(k_time$Date, format = "%U"))
  k_time$relative_week_num <- k_time$logged_week_num - k_time$filing_week_num - 52*k_time$yearvar
  
  
  result <- ddply(k_time, .var = "Services.ID", .fun = function(x){
    loop <- c()
    if(min(x$yearweek) < format(Sys.Date(), format = "%y-%U")){
      for(week in seq(-15, 0, 1)){
        loop <- rbind(loop, data.frame(
          Account.Name = unique(x$Account.Name),
          Service.Name = unique(x[x$Date %in% max(x$Date),]$Service)[1],
          Service.Type = unique(x$Service.Type),
          Form.Type = unique(x$Form.Type),
          Quarter.End = unique(x$Quarter.End),
          reportingPeriod = unique(x$reportingPeriod),
          Filing.Date = unique(x[x$Date %in% max(x$Date),]$Filing.Date),
          Status = unique(x$Status),
          Hours.Estimate = unique(x$Hours.Estimate),
          week = week,
          PSM = unique(x$PSM),
          PS.TM = unique(x$PS.TM),
          PS.Sr.TM = unique(x$PS.Sr.TM),
          Services.ID = unique(x$Services.ID),
          Cumulative.PSM.Hours = sum(x[x$relative_week_num <= week & x$role %in% 'PSM' & x$role %in% 'PSM',]$Hours),
          Cumulative.TM.Hours = sum(x[x$relative_week_num <= week & x$role %in% 'TM' & x$role %in% 'TM',]$Hours),
          Cumulative.Total.Hours = sum(x[x$relative_week_num <= week,]$Hours),
          Week.PSM.Hours = sum(x[x$relative_week_num %in% week & x$role %in% 'PSM' & x$role %in% 'PSM',]$Hours),
          Week.TM.Hours = sum(x[x$relative_week_num %in% week & x$role %in% 'TM' & x$role %in% 'TM',]$Hours),
          Week.Total.Hours = sum(x[x$relative_week_num %in% week,]$Hours)
        ))
      }  
    }
    loop
  })
  result$week <- as.numeric(result$week)
  result
}
