#I wanted to separate import and cleanup functions to minimize the noise in the aggregation


import_timelog <- function(sf_name = "timelog_for_R.csv", oa_name = "time_entry_detail_report__complete_report.csv",include_cs=F, ...){
  library(plyr)
  library(reshape2)
  setwd("C:/R/workspace/shared")
  source("import_functions.r")
  
  # Import salesforce time if necessary, otherwise, read from stored rda file
  setwd('C:/R/workspace/source')
  if(file.info(sf_name)$mtime > file.info('sf_time.Rda')$mtime){
    print("salesforce timelog report has changed, importing salesforce timelog data...")
    sf_timelog <- import_salesforce_timelog()
    saveRDS(sf_timelog, file = "sf_time.Rda")  
  }else{
    sf_timelog <- readRDS(file = "sf_time.Rda")
    print("no change to salesforce time, loading timelog data...")
  }
  if(include_cs == F){
    sf_timelog <- sf_timelog[sf_timelog$is_psm == 1 & !is.na(sf_timelog$is_psm), ]    
  }
  
  #import openair time
  oa_timelog <- import_openair_time(...)
  
  # change names to match salesforce convention
  original_names <- c("services_id_15","User.Job.code","User.Department.level.within.User.Department.hierarchy","Account",
                      "Project", "Project.Form.Type", "Project.Project.Type", "Time.Hours","Project.Filing.Date","Project.Filing.Deadline.Date")
  new_names <- c("Services.ID","User.Title","CS.PS", "Account.Name", "Service", "Form.Type", "Service.Type", "Hours", "Filing.Date", "Filing.Deadline")
  for(i in 1:length(original_names)){
    names(oa_timelog)[names(oa_timelog) %in% original_names[i]] <- new_names[i]
  }
  
  #exclude openair projects that relate to non-billable time (e.g. TEC or admin)
  oa_timelog <- oa_timelog[oa_timelog$Service %in% oa_timelog[grep("Fixed", oa_timelog$Service) ,]$Service | !oa_timelog$Service.Type %in% "",]
  
  #reduce oa timelog and merge the two dataframes
  timelog <- rbind.fill(oa_timelog, sf_timelog)
  timelog <- timelog[,names(timelog) %in% c(names(oa_timelog), "Related.Service.Id") & names(timelog) %in% names(sf_timelog)]
  
  #make name corrections to match salesforce
  setwd('C:/R/workspace/source')
  name_changes <- read.csv("sf_oa_name_changes.csv", header = T, stringsAsFactors = F)
  for (i in 1:dim(name_changes)[1]){
    loop <- timelog[timelog$User %in% name_changes[i,2],]
    if(dim(loop)[1] > 0){
      timelog[timelog$User %in% name_changes[i,2],]$User <- name_changes[i,1]  
    }
  }
  
  #add cik from services data
  services <- import_services()
  pairs <- unique(services[,names(services) %in% c("CIK", "Account.Name")])
  timelog <- merge(timelog, pairs, by = "Account.Name", all.x = T)
  
}

import_services <- function(name = "services_for_ps_history_R.csv", wd = 'C:/R/workspace/source', output = 'simple', include_hourly = F){
  #output can be 'simple' (default) or 'psh' (wide format)
  #include_hourly will have the effect of including hourly service types
  
  ##import services report
    setwd(wd)
    services <- read.csv(name, header = T , stringsAsFactors=F)
    print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
    
    setwd("C:/R/workspace/shared")
    source("import_functions.r")
    
    #trim footer information
    services <- services[1:(dim(services)[1] - 5),]
    
    #cleanup names and data values
    names(services)[names(services) %in% c("Account..Account.Name")] <- "Account.Name"
    names(services)[names(services) %in% c("Account..18.Digit.ID")] <- "Account.ID"
    names(services)[names(services) %in% c("PSM..Full.Name")] <- "PSM"
    names(services)[names(services) %in% c("Sr.PSM..Full.Name")] <- "Sr.PSM"
    names(services)[names(services) %in% c("CSM..Full.Name")] <- "CSM"
    names(services)[names(services) %in% c("Sr.CSM..Full.Name")] <- "Sr.CSM"
    names(services)[names(services) %in% c("Sr.Team.Mgr.CS..Full.Name")] <- "CS.TM"
    names(services)[names(services) %in% c("Sr.Team.Mgr.PS..Full.Name")] <- "PS.TM"
    names(services)[names(services) %in% c("Churned.Effective.Date")] <- "Churn.Date"
    services$Form.Type[services$Form.Type == 'N/A' & !is.na(services$Form.Type)] <- NA
    #services$Goodwill.Hours.Available[services$Goodwill.Hours.Available %in% c("0")] <- NA  
    services$Quarter.End <- as.Date(services$Quarter.End, format = "%m/%d/%Y")
    services$Filing.Date <- as.Date(services$Filing.Date, format = "%m/%d/%Y")
    services$Next.Filing.Date <- as.Date(services$Next.Filing.Date, format = "%m/%d/%Y")
    services$Filing.Deadline <- as.Date(services$Filing.Deadline, format = "%m/%d/%Y")
    services$Created.Date <- as.Date(services$Created.Date, format = "%m/%d/%Y")
    services$Date.Completed <- as.Date(services$Date.Completed, format = "%m/%d/%Y")
    services$service_duration <- as.numeric(services$Date.Completed - services$Created.Date)
    services[services$service_duration < 0 & !is.na(services$service_duration),]$service_duration <- NA
    services$Year.End <- format(services$Year.End, format = "%Y-%U")
    
    #update estimated hours manually 
    setwd("C:/R/workspace/source")
    estimates <- read.csv("estimated_hours.csv", header = T, stringsAsFactors = F)
    print(paste("estimated_hours.csv", "last updated", round(difftime(Sys.time(), file.info("estimated_hours.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
    for(i in 1:dim(estimates)[1]){
      if(length(services[services$Service.Type %in% estimates$Service.Type[i] & services$Form.Type %in% estimates$Form.Type[i],]$Hours.Estimate) > 0){
        services[services$Service.Type %in% estimates$Service.Type[i] & services$Form.Type %in% estimates$Form.Type[i],]$Hours.Estimate <- estimates$Estimated[i]  
      }
    }
    
    #build a lits of unique customers and customer data
    base_info <- c("Account.Name", "Account.ID", "CIK", "CSM", "Sr.CSM", "PSM", "Sr.PSM", "Churn.Date", "Year.End", "XBRL.Status")
    #certain customer service lin items don't populate account info. We need to do that manually to prevent data duplication
    account_data <- services[,colnames(services) %in% base_info]
    account_data <- unique(account_data)
    unique_customers <- as.data.frame(unique(services[ ,colnames(services) %in% c('Account.Name')])) #a complete list of customers.
    names(unique_customers)[1] <- "Account.Name"
    
    #merge selectively to favor more information
    mergePS <- merge(unique_customers, account_data[!is.na(account_data$PSM) & !(account_data$PSM %in% c('')) ,], by = "Account.Name")
    customers_left <- as.data.frame(unique_customers[!(unique_customers$Account.Name %in% mergePS$Account.Name), 1 ])
    names(customers_left)[1] <- "Account.Name"
    mergeCS <- merge(customers_left, account_data[!is.na(account_data$CSM) & !(account_data$CSM %in% c('')),], by = "Account.Name")
    customers_left <- as.data.frame(unique_customers[!(unique_customers$Account.Name %in% mergePS$Account.Name | unique_customers$Account.Name %in% mergeCS$Account.Name), 1])
    names(customers_left)[1] <- "Account.Name"
    mergeNO <- merge(customers_left, account_data, by = "Account.Name")
    
    unique_customers <- rbind(mergePS, rbind(mergeCS, mergeNO))
    unique_customers <- unique_customers[,base_info]
    
    # logic to calculate filing period and reporting period
    if(include_hourly == F){
      services <- services[!is.na(services$Quarter.End), ] #remove services without quarter ends (can't place them)  
      services <- services[!(services$Service.Type %in% c('Reserve Hours', 'Other', 'Training', '')), ] #Remove Reserve Projects
    }else{
      services <- services[!(services$Service.Type %in% c('Training')), ] #Remove Training Projects
    }
    
    #remove CS migrations for PSH, but not general case
    if(output %in% c("psh")){
      services <- services[!(services$CS.PS %in% c('CS')),] #remove all CS services.
    }else if(output %in% c("simple")){
      services <- services[!(services$CS.PS %in% c('CS') & !(services$Service.Type %in% "Migration")),] #remove all CS services.
    }
    
    #calculate filing deadline estimate for all projects
    #for services with form type and registrant type, set reporting offset, then calculate filing.estimate 
    services$reportingOffset <- paste(services$Registrant.Type, services$Form.Type, Sep = "") #placeholder lookup value
    #set offsets for Qs
    services$reportingOffset[services$reportingOffset %in% c("Smaller Reporting Company K-Q ", "Smaller Reporting Company 10-Q ", "Smaller Reporting Company Q-Q ")] <- 45
    services$reportingOffset[services$reportingOffset %in% c("Non-Accelerated Filer K-Q ", "Non-Accelerated Filer 10-Q ", "Non-Accelerated Filer Q-Q ")] <- 45
    services$reportingOffset[services$reportingOffset %in% c("Accelerated Filer K-Q ", "Accelerated Filer 10-Q ", "Accelerated Filer Q-Q ")] <- 40
    services$reportingOffset[services$reportingOffset %in% c("Large Accelerated Filer K-Q ", "Large Accelerated Filer 10-Q ", "Large Accelerated Filer Q-Q ")] <- 40
    #set offsets for Ks
    services$reportingOffset[services$reportingOffset %in% c("Smaller Reporting Company Q-K ", "Smaller Reporting Company 10-K ", "Smaller Reporting Company K-K ")] <- 90
    services$reportingOffset[services$reportingOffset %in% c("Non-Accelerated Filer Q-K ", "Non-Accelerated Filer 10-K ", "Non-Accelerated Filer K-K ")] <- 90
    services$reportingOffset[services$reportingOffset %in% c("Accelerated Filer Q-K ", "Accelerated Filer 10-K ", "Accelerated Filer K-K ")] <- 75
    services$reportingOffset[services$reportingOffset %in% c("Large Accelerated Filer Q-K ", "Large Accelerated Filer 10-K ", "Large Accelerated Filer K-K ", "Large Accelerated Filer 20-F - 20-F ")] <- 60
    #
    services$reportingOffset[!(services$reportingOffset %in% c(40, 45, 60, 75, 90))] = NA
    
    #with report offset, calculate filing deadline estimate
    services$filing.estimate <- NA
    services$filing.estimate <- as.Date(services$filing.estimate)
    services[services$reportingOffset %in% c(40, 45, 60, 75, 90), ]$filing.estimate <- 
      as.Date(services[services$reportingOffset %in% c(40, 45, 60, 75, 90),]$Quarter.End) + 
      as.numeric(services[services$reportingOffset %in% c(40, 45, 60, 75, 90),]$reportingOffset)
    #for services without both form type and registrant type, use the filing date reported (if available)
    services[is.na(services$filing.estimate) & !is.na(services$Filing.Date), ]$filing.estimate <- services[is.na(services$filing.estimate) & !is.na(services$Filing.Date), ]$Filing.Date
    #handle foreign issuer cases
    #case for 6-Ks and 10-Qs
    services[services$Registrant.Type %in% c("Foreign Issuer") & services$Form.Type %in% c("10-Q", "6-K"), ]$filing.estimate <- 
      as.Date(services[services$Registrant.Type %in% c("Foreign Issuer") & services$Form.Type %in% c("10-Q", "6-K"),]$Quarter.End) + 45
    #case for 10-Ks
    services[services$Registrant.Type %in% c("Foreign Issuer") & services$Form.Type %in% c("10-K"), ]$filing.estimate <- 
      as.Date(services[services$Registrant.Type %in% c("Foreign Issuer") & services$Form.Type %in% c("10-K"),]$Quarter.End) + 90
    #take the filing deadline in the system at this point (if available).
    services[is.na(services$filing.estimate) & !is.na(services$Filing.Deadline), ]$filing.estimate <- 
      as.Date(services[is.na(services$filing.estimate) & !is.na(services$Filing.Deadline), ]$Filing.Deadline)
    #anyone with no registrant type set, estimate 40 days for 10-Qs and 60 for a 10-Ks and 20-Fs
    services[services$Registrant.Type %in% c("") & services$Form.Type %in% c("10-K", "Q-K", "K-K", "20-F"), ]$filing.estimate <- 
      as.Date(services[services$Registrant.Type %in% c("") & services$Form.Type %in% c("10-K", "Q-K", "K-K", "20-F", "20-F - 20-F"), ]$Quarter.End) + 60
    services[services$Registrant.Type %in% c("") & services$Form.Type %in% c("10-Q", "K-Q", "Q-Q"), ]$filing.estimate <- 
      as.Date(services[services$Registrant.Type %in% c("") & services$Form.Type %in% c("10-Q", "K-Q", "Q-Q"), ]$Quarter.End) + 40
    #for remaining services, estimate 40 days
    services[is.na(services$filing.estimate), ]$filing.estimate <- 
      as.Date(services[is.na(services$filing.estimate), ]$Quarter.End) + 40
    
    #Construct the Period Identifiers for service grouping
    services$filingPeriod <- paste(as.numeric(format(services$filing.estimate, "%Y")), ceiling(as.numeric(format(services$filing.estimate, "%m"))/3), sep = "")
    services$reportingPeriod <- ifelse(substr(services$filingPeriod, nchar(services$filingPeriod), nchar(services$filingPeriod)) == 1,
                                      paste(as.numeric(format(services$filing.estimate, "%Y")) -1, 4, sep = ""),
                                      paste(as.numeric(format(services$filing.estimate, "%Y")), ceiling(as.numeric(format(services$filing.estimate, "%m"))/3) - 1, sep = ""))
    services$CIK <- as.numeric(services$CIK)
    services$Churn.Date <- as.Date(services$Churn.Date, format = "%m/%d/%Y")
    
    
	if(output %in% c("psh")){
	  svc_by_qtr <- aggregate(services$Service.Name, by=list(services$Account.Name, services$reportingPeriod), paste, collapse = "\n")
	  names(svc_by_qtr) <- c("Account.Name", "reportingPeriod", "Services")
	  svc_by_qtr <- dcast(svc_by_qtr, Account.Name ~ reportingPeriod)
	  #remove?
	  svc_by_qtr <- sapply(svc_by_qtr, function(x) ifelse(x %in% c("NULL"), NA, x)) #change NULLs produced by dcast into NAs
	  
	  #join unique customers with dcast services    
	  merged <- merge(unique_customers, svc_by_qtr, by = "Account.Name", all.x = T)
	  merged <- merged[order(merged$Account.Name), ]
		merged[!(merged$XBRL.Status %in% c("None")),]
	}else if(output %in% c("simple", "expanded")){
		services
	}
    
}

import_sec <- function(name = "filing_data.csv", wd ="C:/R/workspace/source"  ){
  setwd(wd)
  facts <- read.csv(name, header = T , stringsAsFactors=F)
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  
  #remove extra fields to remove potential for incomplete cases due to something extraneous
  valid_list <- c("accession_number", "name", "cik", "sic", "form", "form_group","period_end_date", 
             "filing_date", "filing_qtr", "filing_month", "facts", "xbrl_software", "filing_category")
  facts <- facts[, names(facts) %in% valid_list]
  #facts <- facts[facts$xbrl_software %in% c("WebFilings"), ]
  #remove incomplete cases
  facts <- facts[complete.cases(facts),]
  #cast columns properly
  facts$filing_date <- as.Date(facts$filing_date, format = "%m/%d/%Y")
  facts$period_end_date <- as.Date(facts$period_end_date, format = "%m/%d/%Y")
  facts$facts <- as.numeric(facts$facts)
  facts <- facts[facts$form_group %in% c("tens", "amend"),]
  facts <- facts[rev(order(facts$filing_date)), ]
  
  #calculate filing deadline estimate for all projects
  #for facts with form type and registrant type, set reporting offset
  facts$reporting_offset <- paste(facts$filing_category, facts$form, Sep = "") #placeholder lookup value
  #set offsets for Qs
  facts$reporting_offset[facts$reporting_offset %in% c("Smaller Reporting Company 10-Q ", "Smaller Reporting Company 10-Q/A ")] <- 45
  facts$reporting_offset[facts$reporting_offset %in% c("Non-accelerated Filer 10-Q ", "Non-accelerated Filer 10-Q/A ")] <- 45
  facts$reporting_offset[facts$reporting_offset %in% c("Smaller Reporting Accelerated Filer 10-Q ", "Smaller Reporting Accelerated Filer 10-Q/A ", "Accelerated Filer 10-Q ", "Accelerated Filer 10-Q/A ")] <- 40
  facts$reporting_offset[facts$reporting_offset %in% c("Large Accelerated Filer 10-Q ", "Large Accelerated Filer 10-Q/A ")] <- 40
  #set offsets for Ks
  facts$reporting_offset[facts$reporting_offset %in% c("Smaller Reporting Company 10-K ", "Smaller Reporting Company 10-K/A ")] <- 90
  facts$reporting_offset[facts$reporting_offset %in% c("Non-accelerated Filer 10-K ", "Non-accelerated Filer 10-K/A ")] <- 90
  facts$reporting_offset[facts$reporting_offset %in% c("Smaller Reporting Accelerated Filer 10-K ", "Smaller Reporting Accelerated Filer 10-K/A ","Accelerated Filer 10-K ", "Accelerated Filer 10-K/A ")] <- 75
  facts$reporting_offset[facts$reporting_offset %in% c("Large Accelerated Filer 10-K ", "Large Accelerated Filer 10-K/A ")] <- 60
  #
  facts$reporting_offset[!(facts$reporting_offset %in% c(40, 45, 60, 75, 90))] = NA
  

  #with reporting offset, calculate filing.deadline 
  facts$filing.deadline <- NA
  facts$filing.deadline <- as.Date(facts$filing.deadline)
  facts[facts$reporting_offset %in% c(40, 45, 60, 75, 90), ]$filing.deadline <- 
    as.Date(facts[facts$reporting_offset %in% c(40, 45, 60, 75, 90),]$period_end_date) + 
    as.numeric(facts[facts$reporting_offset %in% c(40, 45, 60, 75, 90),]$reporting_offset)
  
  facts
}

import_sales_recommendations <- function(name = "sales_recommendations_for_r.csv", include_closed = T, wide = F ){
  setwd('C:/R/workspace/source')
  sales_rec <- read.csv(name, header = T , stringsAsFactors=F)
  
  #trim footer information
  sales_rec <- sales_rec[1:(dim(sales_rec)[1] - 5),]
  
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  sales_rec$Service.QED <- as.Date(sales_rec$Service.QED, format = "%m/%d/%Y")
  sales_rec <- sales_rec[!is.na(sales_rec$Service.QED),]
  sales_rec$period <- paste(as.numeric(format(sales_rec$Service.QED, "%Y")), ceiling(as.numeric(format(sales_rec$Service.QED, "%m"))/3) , sep = "")
  if(include_closed == F){
    sales_rec <- sales_rec[sales_rec$Recomendation.Status %in% unique(sales_rec$Recomendation.Status)[grep('Closed', unique(sales_rec$Recomendation.Status), fixed = T)],]
  }
  if(wide == T){
    svc_by_qtr <- aggregate(sales_rec$Product, by=list(sales_rec$Account.Name, sales_rec$period), paste, collapse = "\n")
    names(svc_by_qtr) <- c("Account.Name", "period", "Services")
    result <- dcast(svc_by_qtr, Account.Name ~ period)
    for (i in 1:length(names(result)[grep('[1-9]+', names(result), perl = T)])) {
      names(result)[grep('[1-9]+', names(result), perl = T)][i] <- 
        paste(names(result)[grep('[1-9]+', names(result), perl = T)][i] , "Sales Rec", collapse = "\n")
    }  
  }else{
    result <- sales_rec
  }
    
  result
}

import_opportunities <- function(name = "opportunities_for_R.csv"){
  setwd('C:/R/workspace/source')
  opps <- read.csv(name, header = T, stringsAsFactors = F)
  
  #trim footer information
  opps <- opps[1:(dim(opps)[1] - 5),]
  
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  legacy_pricing <- read.csv("legacy_list_pricing.csv", header = T, stringsAsFactors = F)
  print(paste("legacy_list_pricing.csv", "last updated", round(difftime(Sys.time(), file.info("legacy_list_pricing.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  opps$Close.Date <- as.Date(opps$Close.Date, format = "%m/%d/%Y")
  
  #update weird names
  old <- c("List.Price..converted..Currency", "List.Price..converted.", "Sales.Price..converted..Currency", "Sales.Price..converted.", "Total.Price..converted..Currency", "Total.Price..converted.")
  new <- c("List.Price.Currency", "List.Price", "Sales.Price.Currency", "Sales.Price", "Total.Price.Currency", "Total.Price")
  for (i in 1:length(old)){
    names(opps)[names(opps) %in% old[i]] <- new[i]    
  }
  #update legacy pricing
  for (code in unique(legacy_pricing$Product.Code)){
    if(dim(opps[opps$Product.Code %in% code & opps$Close.Date <= "2013-05-31" & !is.na(opps$Close.Date),])[1] > 0){
      opps[opps$Product.Code %in% code & opps$Close.Date <= "2013-05-31" & !is.na(opps$Close.Date),]$List.Price <- as.numeric(legacy_pricing[legacy_pricing$Product.Code %in% code,]$List.Price)
    }
  }
  opps$Sales.Price <- opps$Total.Price/opps$Quantity
  opps
}

import_contracts <- function(name = "contracts_for_pshistory.csv"){
  setwd('C:/R/workspace/source')
  contracts <- read.csv(name, header = T, stringsAsFactors = F)
  #trim footer information
  contracts <- contracts[1:(dim(contracts)[1] - 5),]
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  contracts$Contract.Start.Date <- as.Date(contracts$Contract.Start.Date, format = "%m/%d/%Y")
  contracts$CIK <- as.numeric(contracts$CIK)
  contracts
}

import_hierarchy <- function(name = "hierarchy.csv"){
  setwd('C:/R/workspace/source')
  hierarchy <- read.csv(name, header = T, stringsAsFactors = F)
  #trim footer information
  hierarchy <- hierarchy[1:(dim(hierarchy)[1] - 5),]
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  hierarchy$CIK <- as.numeric(hierarchy$CIK)

  hierarchy
}

import_app_filing_data <- function(name = "app_filing_data.csv", wd = "C:/R/workspace/source"){
  print ("this function has been deprecated")
  print ("please use 'load_app_filing_data'")
  setwd(wd)
  app_data <- read.csv(name, header = T, stringsAsFactors = F)
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  app_data$Filing.Date <- as.Date(app_data$Filing.Date, format = "%m/%d/%Y")
  app_data$monthyear <- format(app_data$Filing.Date, format = "%y-%m")
  app_data$Fact.Cnt <- as.numeric(app_data$Fact.Cnt)
  app_data$EXT.Fact.Cnt <- as.numeric(app_data$EXT.Fact.Cnt)
  app_data
}

load_app_filing_data <- function(){
  setwd("c:/r/workspace/source")
  test <- readRDS("app_filings.Rda")  
}

import_daily_hours <- function(){
  library(plyr)
  library(reshape2)
  setwd("C:/R/workspace/shared")
  source("import_functions.r")
  
  # Import salesforce time if necessary, otherwise, read from stored rda file
  setwd('C:/R/workspace/source')
  if(file.info("daily_hours.csv")$mtime > file.info('sf_time.Rda')$mtime){
    print("salesforce timelog report has changed, importing salesforce timelog data...")
    sf_daily <- import_salesforce_daily_hours()
    saveRDS(sf_daily, file = "sf_daily.Rda")  
  }else{
    sf_daily <- readRDS(file = "sf_daily.Rda")
    print("no change to salesforce daily hours, loading timelog data...")
  }
  
  #import openair time
  oa_timelog <- import_openair_time()
  
  # change names to match salesforce convention
  original_names <- c("services_id_15","User.Job.code","User.Department.level.within.User.Department.hierarchy","Account",
                      "Project", "Project.Form.Type", "Project.Project.Type", "Time.Hours","Project.Filing.Date","Project.Filing.Deadline.Date")
  new_names <- c("Services.ID","User.Title","CS.PS", "Account.Name", "Service", "Form.Type", "Service.Type", "Hours", "Filing.Date", "Filing.Deadline")
  for(i in 1:length(original_names)){
    names(oa_timelog)[names(oa_timelog) %in% original_names[i]] <- new_names[i]
  }
  
  #aggregate then reduce oa timelog and merge the two dataframes
  oa_timelog_agg <- aggregate(Hours ~ Date + User + role + User.Title + is_psm, data = oa_timelog, FUN = sum)
  timelog <- rbind(oa_timelog_agg, sf_daily)
  
  #make name corrections to match salesforce
  setwd('C:/R/workspace/source')
  name_changes <- read.csv("sf_oa_name_changes.csv", header = T, stringsAsFactors = F)
  for (i in 1:dim(name_changes)[1]){
    loop <- timelog[timelog$User %in% name_changes[i,2],]
    if(dim(loop)[1] > 0){
      timelog[timelog$User %in% name_changes[i,2],]$User <- name_changes[i,1]  
    }
  }
  
  timelog
}

import_salesforce_daily_hours <- function(name = "daily_hours.csv", wd = 'C:/R/workspace/source'){
  #import and cleanup timelog
  setwd(wd)
  daily <- read.csv(name, header = T , stringsAsFactors=F)
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  setwd('C:/R/workspace/source')
  start_dates <- read.csv("ps_start_dates.csv", header = T , stringsAsFactors=F)
  print(paste("ps_start_dates.csv", "last updated", round(difftime(Sys.time(), file.info("ps_start_dates.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  
  #trim footer information
  daily <- daily[1:(dim(daily)[1] - 5),]
  
  #cleanup names and data values
  daily$Date <- as.Date(daily$Date, format = "%m/%d/%Y")
  daily <- daily[!is.na(daily$Hours) && !is.na(daily$Date),]
  names(daily)[names(daily) %in% "Daily.Log..Owner.Role"] <- "User.Title"
  start_dates$Start.Date <- as.Date(start_dates$Start.Date, format = "%m/%d/%Y")
  start_dates$End.Date <- as.Date(start_dates$End.Date, format = "%m/%d/%Y")
  
  #case 1: Known PSMs
  daily$is_psm <- 0
  daily[daily$User %in% start_dates[is.na(start_dates$Start.Date) & is.na(start_dates$End.Date), ]$Full.Name, ]$is_psm <- 1 #psms who are still in PS
  for (psm in start_dates[!is.na(start_dates$Start.Date) | !is.na(start_dates$End.Date), ]$Full.Name) {#need to subset for each psm
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$Start.Date)){
      if(length(daily[daily$User %in% psm & !is.na(daily$User) & daily$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm) > 0){
        daily[daily$User %in% psm & !is.na(daily$User) & daily$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm <- 1
      }
      if(length(daily[daily$User %in% psm & !is.na(daily$User) & daily$Date < start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm) > 0){
        daily[daily$User %in% psm & !is.na(daily$User) & daily$Date < start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm <- 0
      }
    }
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$End.Date)){
      if(length(daily[daily$User %in% psm & !is.na(daily$User) & daily$Date <= start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm) > 0){
        daily[daily$User %in% psm & !is.na(daily$User) & daily$Date <= start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm <- 1 
      }
      if(length(daily[daily$User %in% psm & !is.na(daily$User) & daily$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm) > 0){
        daily[daily$User %in% psm & !is.na(daily$User) & daily$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm <- 0
      }
    }
  }
  #case 2: unknown PSMs
  # *******************stop
  ps_titles <- c("Professional Services Managers")
  if(length(daily[daily$User.Title %in% ps_titles, ]$is_psm) > 0){
    daily[daily$User.Title %in% ps_titles, ]$is_psm <- 1
  }
  
  #now all relevant time is marked, remove 0 and na time from timelog
  daily <- daily[daily$is_psm %in% "1", ] # ~ 1%
  
  #****************************** import role dates
  #get role dates
  setwd("C:/R/workspace/source")
  role_dates <- read.csv("ps_start_dates.csv", header = T, stringsAsFactors = F)
  print(paste("ps_start_dates.csv", "last updated", round(difftime(Sys.time(), file.info("ps_start_dates.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))] <- 
    lapply(role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))],FUN = as.Date, format = "%m/%d/%Y")
  
  #set all time to PSM when the title says psm or sr psm
  daily$role <- NA
  daily[daily$User.Title %in% unique(daily$User.Title)[grep("Professional", unique(daily$User.Title))],]$role <- "PSM"
  daily[daily$is_psm %in% 1,]$role <- "PSM"
  
  #for those with a start date, set time before to NA
  for (i in 1:length(role_dates[!is.na(role_dates$Start.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$Start.Date),]$Full.Name[i]
    start <- role_dates[!is.na(role_dates$Start.Date),]$Start.Date[i]
    if(length(daily[daily$User %in% psm & daily$Date < start, ]$role) > 0){
      daily[daily$User %in% psm & daily$Date < start, ]$role <- NA
    }
  }
  
  #for psms promoted to senior, set time forward to Sr PSM
  for (i in 1:length(role_dates[!is.na(role_dates$to_senior),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_senior),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_senior),]$to_senior[i]
    if(length(daily[daily$User %in% psm & daily$Date >= promotion_date, ]$role) > 0){
      daily[daily$User %in% psm & daily$Date >= promotion_date, ]$role <- "Sr PSM"
    }
  }
  
  #for srs promoted to tms, set time forward to TM
  for (i in 1:length(role_dates[!is.na(role_dates$to_tm),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_tm),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_tm),]$to_tm[i]
    if(length(daily[daily$User %in% psm & daily$Date >= promotion_date, ]$role) > 0 ){
      daily[daily$User %in% psm & daily$Date >= promotion_date, ]$role <- "TM"
    }
  }
  
  #for tms promoted to director, set time forward to director
  for (i in 1:length(role_dates[!is.na(role_dates$to_director),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_director),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_director),]$to_director[i]
    if(length(daily[daily$User %in% psm & daily$Date >= promotion_date, ]$role) > 0){
      daily[daily$User %in% psm & daily$Date >= promotion_date, ]$role <- "Director"
    }
  }
  
  #for psms who left PS, set time forward to NA
  for (i in 1:length(role_dates[!is.na(role_dates$End.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$End.Date),]$Full.Name[i]
    term_date <- role_dates[!is.na(role_dates$End.Date),]$End.Date[i]
    if(length(daily[daily$User %in% psm & daily$Date >= term_date, ]$role) > 0){
      daily[daily$User %in% psm & daily$Date >= term_date, ]$role <- NA
    }
  }
  
daily
  
}

import_salesforce_timelog <- function(name = "timelog_for_R.csv", wd = 'C:/R/workspace/source', include_cs = T){
  #import and cleanup timelog
  setwd(wd)
  timelog <- read.csv(name, header = T , stringsAsFactors=F)
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  setwd('C:/R/workspace/source')
  start_dates <- read.csv("ps_start_dates.csv", header = T , stringsAsFactors=F)
  print(paste("ps_start_dates.csv", "last updated", round(difftime(Sys.time(), file.info("ps_start_dates.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  
  #trim footer information
  timelog <- timelog[1:(dim(timelog)[1] - 5),]
  
  #cleanup names and data values
  names(timelog)[names(timelog) %in% c("User..Full.Name")] <- "User"
  names(timelog)[names(timelog) %in% c("Services..Service.Name")] <- "Service"
  timelog$CIK <- as.numeric(timelog$CIK)
  timelog$Hours <- as.numeric(timelog$Hours)
  timelog$Date <- as.Date(timelog$Date, format = "%m/%d/%Y")
  #timelog$Week <- format(timelog$Date, format = "%Y-%U")
  timelog$Filing.Date <- as.Date(timelog$Filing.Date, format = "%m/%d/%Y")
  timelog$Filing.Deadline <- as.Date(timelog$Filing.Deadline, format = "%m/%d/%Y")
  timelog <- timelog[!is.na(timelog$Hours) && !is.na(timelog$Date),]
  start_dates$Start.Date <- as.Date(start_dates$Start.Date, format = "%m/%d/%Y")
  start_dates$End.Date <- as.Date(start_dates$End.Date, format = "%m/%d/%Y")
  
  #need to remove non-psm time. 
  timelog$is_psm <- 0
  #case 1: Known PSMs
  timelog[timelog$User %in% start_dates[is.na(start_dates$Start.Date) & is.na(start_dates$End.Date), ]$Full.Name, ]$is_psm <- 1 #with no movement in position
  for (psm in start_dates[!is.na(start_dates$Start.Date) | !is.na(start_dates$End.Date), ]$Full.Name) {#need to subset for each psm
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$Start.Date)){
      if(length(timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm) > 0){
        timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm <- 1
      }
      if(length(timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date < start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm) > 0){
        timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date < start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm <- 0
      }
    }
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$End.Date)){
      if(length(timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date <= start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm) > 0){
        timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date <= start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm <- 1 
      }
      if(length(timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm) > 0){
        timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm <- 0
      }
    }
  }
  #case 2: unknown PSMs
  ps_titles <- c("Senior Professional Services Manager", "Professional Services Manager")
  if(length(timelog[timelog$User.Title %in% ps_titles, ]$is_psm) > 0){
    timelog[timelog$User.Title %in% ps_titles, ]$is_psm <- 1
  }
  
  #now all relevant time is marked, remove 0 and na time from timelog
  if(include_cs == F){
    timelog <- timelog[timelog$is_psm == 1 & !is.na(timelog$is_psm), ]    
  }
  
  #Construct the Period Identifiers for service grouping
  timelog$filingPeriod <- paste(as.numeric(format(timelog$Date, "%Y")), ceiling(as.numeric(format(timelog$Date, "%m"))/3), sep = "")
  timelog$reportingPeriod <- ifelse(substr(timelog$filingPeriod, nchar(timelog$filingPeriod), nchar(timelog$filingPeriod)) == 1,
                                    paste(as.numeric(format(timelog$Date, "%Y")) -1, 4, sep = ""),
                                    paste(as.numeric(format(timelog$Date, "%Y")), ceiling(as.numeric(format(timelog$Date, "%m"))/3) - 1, sep = ""))
  
  #****************************** import role dates
  #get role dates
  setwd("C:/R/workspace/source")
  role_dates <- read.csv("ps_start_dates.csv", header = T, stringsAsFactors = F)
  print(paste("ps_start_dates.csv", "last updated", round(difftime(Sys.time(), file.info("ps_start_dates.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))] <- 
    lapply(role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))],FUN = as.Date, format = "%m/%d/%Y")
  
  #set all time to PSM when the title says psm or sr psm or if the is_psm boolean is 1
  timelog$role <- NA
  timelog[timelog$User.Title %in% unique(timelog$User.Title)[grep("Professional", unique(timelog$User.Title))],]$role <- "PSM"
  timelog[timelog$is_psm %in% 1,]$role <- "PSM"
  
  #for those with a start date, set time before to NA
  for (i in 1:length(role_dates[!is.na(role_dates$Start.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$Start.Date),]$Full.Name[i]
    start <- role_dates[!is.na(role_dates$Start.Date),]$Start.Date[i]
    if(length(timelog[timelog$User %in% psm & timelog$Date < start, ]$role) > 0){
      timelog[timelog$User %in% psm & timelog$Date < start, ]$role <- NA
    }
  }
  
  #for psms promoted to senior, set time forward to Sr PSM
  for (i in 1:length(role_dates[!is.na(role_dates$to_senior),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_senior),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_senior),]$to_senior[i]
    if(length(timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role) > 0){
      timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role <- "Sr PSM"
    }
  }
  
  #for srs promoted to tms, set time forward to TM
  for (i in 1:length(role_dates[!is.na(role_dates$to_tm),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_tm),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_tm),]$to_tm[i]
    if(length(timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role) > 0 ){
      timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role <- "TM"
    }
  }
  
  #for tms promoted to director, set time forward to director
  for (i in 1:length(role_dates[!is.na(role_dates$to_director),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_director),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_director),]$to_director[i]
    if(length(timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role) > 0){
      timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role <- "Director"
    }
  }
  
  #for psms who left PS, set time forward to NA
  for (i in 1:length(role_dates[!is.na(role_dates$End.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$End.Date),]$Full.Name[i]
    term_date <- role_dates[!is.na(role_dates$End.Date),]$End.Date[i]
    if(length(timelog[timelog$User %in% psm & timelog$Date >= term_date, ]$role) > 0){
      timelog[timelog$User %in% psm & timelog$Date >= term_date, ]$role <- NA
    }
  }
  
  timelog
  
  
}

import_openair_time <- function(name = "time_entry_detail_report__complete_report.csv", wd = "C:/R/workspace/source", include_cs = F){
  
  library(plyr)
  library(reshape2)
  setwd("C:/R/workspace/shared")
  source("import_functions.r")
  
  setwd(wd)
  openair <- read.csv(name, header = F, stringsAsFactors = F)
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days")
                    , digits = 1), "days ago", sep = " "))
  openair <- openair[-1,]
  
  #populate psm and job to columns a and b
  for (row in 1:dim(openair)[1]){
    if (openair[row, 1] %in% " "){
      
    }else{
      job = openair[row, 1]
    }
    if (openair[row, 2] %in% " "){
      
    }else{
      psm = openair[row, 2]
    }
    if (!openair[row, 3] %in% " "){
      openair[row, 1] <- job
      openair[row, 2] <- psm
    }
  }
  #grab row 1 for header and remove
  names(openair) <- openair[1,]
  openair <- openair[-1,]
  #trim leading psm and job rows
  names(openair) <- gsub("- ","",names(openair))
  names(openair) <- gsub("[[:punct:]]","",names(openair))
  names(openair) <- gsub(" ",".",names(openair))
  openair$services_id_15 <- substr(openair$"Project.SFDC.Project.ID",1,15)
  
  #cast data values
  openair$"Project.Quarter.End.Date.QED" <- as.Date(openair$"Project.Quarter.End.Date.QED", format = "%m/%d/%Y")
  openair$"Project.Filing.Date" <- as.Date(openair$"Project.Filing.Date", format = "%m/%d/%Y")
  openair$"Project.Filing.Deadline.Date" <- as.Date(openair$"Project.Filing.Deadline.Date", format = "%m/%d/%Y")
  openair$"Project..of.Facts" <- as.numeric(openair$"Project..of.Facts")
  openair$Time.Hours <- as.numeric(openair$Time.Hours)
  openair$Date <- as.Date(openair$Date, format = "%m/%d/%Y")
  
  if(include_cs == F){
    openair <- openair[openair$User.Department.level.within.User.Department.hierarchy %in% c("PS"),] #remove header rows and non-project related time from report  
  }else{
    openair <- openair[openair$User.Department.level.within.User.Department.hierarchy %in% c("PS", "CS"),] #remove header rows and non-project related time from report  
  }
  
  
  #Construct the Period Identifiers for service grouping
  openair$filingPeriod <- paste(as.numeric(format(openair$Project.Filing.Date, "%Y")), ceiling(as.numeric(format(openair$Project.Filing.Date, "%m"))/3), sep = "")
  openair$reportingPeriod <- paste(as.numeric(format(openair$Project.Quarter.End.Date.QED, "%Y")), ceiling(as.numeric(format(openair$Project.Quarter.End.Date.QED, "%m"))/3), sep = "")
  
  #reverse User names from "last, first" to "first last"
  resources <- read.csv(textConnection(openair$User), header = F, strip.white=T)
  openair$User <- paste(resources[,2], resources[,1], sep = " ")
  
  #****************************** construct is_psm
  setwd('C:/R/workspace/source')
  start_dates <- read.csv("ps_start_dates.csv", header = T , stringsAsFactors=F)
  print(paste("ps_start_dates.csv", "last updated", round(difftime(Sys.time(), file.info("ps_start_dates.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  start_dates$Start.Date <- as.Date(start_dates$Start.Date, format = "%m/%d/%Y")
  start_dates$End.Date <- as.Date(start_dates$End.Date, format = "%m/%d/%Y")
  
  #need to remove non-psm time. 
  openair$is_psm <- 0
  #case 1: Known PSMs
  openair[openair$User %in% start_dates[is.na(start_dates$Start.Date) & is.na(start_dates$End.Date), ]$Full.Name, ]$is_psm <- 1 #with no movement in position
  for (psm in start_dates[!is.na(start_dates$Start.Date) | !is.na(start_dates$End.Date), ]$Full.Name) {#need to subset for each psm
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$Start.Date)){
      if(length(openair[openair$User %in% psm & !is.na(openair$User) & openair$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm) > 0){
        openair[openair$User %in% psm & !is.na(openair$User) & openair$Date >= start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm <- 1
      }
      if(length(openair[openair$User %in% psm & !is.na(openair$User) & openair$Date < start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm) > 0){
        openair[openair$User %in% psm & !is.na(openair$User) & openair$Date < start_dates[start_dates$Full.Name %in% psm, ]$Start.Date, ]$is_psm <- 0
      }
    }
    if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$End.Date)){
      if(length(openair[openair$User %in% psm & !is.na(openair$User) & openair$Date <= start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm) > 0){
        openair[openair$User %in% psm & !is.na(openair$User) & openair$Date <= start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm <- 1 
      }
      if(length(openair[openair$User %in% psm & !is.na(openair$User) & openair$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm) > 0){
        openair[openair$User %in% psm & !is.na(openair$User) & openair$Date > start_dates[start_dates$Full.Name %in% psm, ]$End.Date, ]$is_psm <- 0
      }
    }
  }
  #case 2: unknown PSMs
  ps_titles <- unique(openair$User.Job.code)[grep("PSM", unique(openair$User.Job.code))]
  if(length(openair[openair$User.Job.code %in% ps_titles, ]$is_psm) > 0){
    openair[openair$User.Job.code %in% ps_titles, ]$is_psm <- 1
  } 
  
  
  #****************************** import role dates
  #get role dates
  setwd("C:/R/workspace/source")
  role_dates <- read.csv("ps_start_dates.csv", header = T, stringsAsFactors = F)
  print(paste("ps_start_dates.csv", "last updated", round(difftime(Sys.time(), file.info("ps_start_dates.csv")$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))] <- 
    lapply(role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))],FUN = as.Date, format = "%m/%d/%Y")
  
  #set all time to PSM when the title says psm or sr psm or if the is_psm boolean is 1
  openair$role <- NA
  openair[openair$User.Job.code %in% unique(openair$User.Job.code)[grep("PSM", unique(openair$User.Job.code))],]$role <- "PSM"
  openair[openair$is_psm %in% 1,]$role <- "PSM"
  
  #for those with a start date, set time before to NA
  for (i in 1:length(role_dates[!is.na(role_dates$Start.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$Start.Date),]$Full.Name[i]
    start <- role_dates[!is.na(role_dates$Start.Date),]$Start.Date[i]
    if(length(openair[openair$User %in% psm & openair$Date < start, ]$role) > 0){
      openair[openair$User %in% psm & openair$Date < start, ]$role <- NA
    }
  }
  
  #for psms promoted to senior, set time forward to Sr PSM
  for (i in 1:length(role_dates[!is.na(role_dates$to_senior),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_senior),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_senior),]$to_senior[i]
    if(length(openair[openair$User %in% psm & openair$Date >= promotion_date, ]$role) > 0){
      openair[openair$User %in% psm & openair$Date >= promotion_date, ]$role <- "Sr PSM"
    }
  }
  
  #for srs promoted to tms, set time forward to TM
  for (i in 1:length(role_dates[!is.na(role_dates$to_tm),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_tm),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_tm),]$to_tm[i]
    if(length(openair[openair$User %in% psm & openair$Date >= promotion_date, ]$role) > 0 ){
      openair[openair$User %in% psm & openair$Date >= promotion_date, ]$role <- "TM"
    }
  }
  
  #for tms promoted to director, set time forward to director
  for (i in 1:length(role_dates[!is.na(role_dates$to_director),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$to_director),]$Full.Name[i]
    promotion_date <- role_dates[!is.na(role_dates$to_director),]$to_director[i]
    if(length(openair[openair$User %in% psm & openair$Date >= promotion_date, ]$role) > 0){
      openair[openair$User %in% psm & openair$Date >= promotion_date, ]$role <- "Director"
    }
  }
  
  #for psms who left PS, set time forward to NA
  for (i in 1:length(role_dates[!is.na(role_dates$End.Date),]$Full.Name)){
    psm <- role_dates[!is.na(role_dates$End.Date),]$Full.Name[i]
    term_date <- role_dates[!is.na(role_dates$End.Date),]$End.Date[i]
    if(length(openair[openair$User %in% psm & openair$Date >= term_date, ]$role) > 0){
      openair[openair$User %in% psm & openair$Date >= term_date, ]$role <- NA
    }
  }
  
  #****************************** /import role dates
  
  openair$Billable <- 0
  openair[openair$Project %in% unique(openair$Project)[grep("Hour", unique(openair$Project))],]$Billable <- 1
  openair[openair$Project %in% unique(openair$Project)[grep("Fixed", unique(openair$Project))],]$Billable <- 1
  openair
  
}

import_openair_workload <- function(name = "Team_Workload_report_pivot.csv", wd = "C:/R/workspace/source"){
  setwd(wd)
  workload <- read.csv(name, header = T , stringsAsFactors=F)
  print(paste(name, "last updated", round(difftime(Sys.time(), file.info(name)$mtime, units = "days"), digits = 1), "days ago", sep = " "))
  
  # cleanup names
  original_names <- c("ï..Date", "Project.Project.owner", "Project.Account","Project.Product","Project.Project.Type",
                      "Project.Form.Type","Project.Quarter.End.Date..QED.", "Project.Filing.Date", "Project.Filing.Deadline.Date",
                      "Project.Project.stage", "Project...Complete", "Resources...All.booked.hours", "Projects...All.assigned.hours",
                      "Timesheets...All.hours")
  new_names <- c("Date", "Owner", "Account.Name","Product.Name","Project.Type","Form.Type","Quarter.End.Date", "Filing.Date", 
                 "Filing.Deadline", "Project.Stage", "Project.Complete", "Resources.all.booked.hours", "Projects.all.assigned.hours",
                 "Timesheets.all.hours")
  for(i in 1:length(original_names)){
    names(workload)[names(workload) %in% original_names[i]] <- new_names[i]
  }
  workload$Quarter.End.Date <- as.Date(workload$Quarter.End.Date, format = "%m/%d/%Y")
  workload$Filing.Date <- as.Date(workload$Filing.Date, format = "%m/%d/%Y")
  workload$Filing.Deadline <- as.Date(workload$Filing.Deadline, format = "%m/%d/%Y")
  
  workload
}