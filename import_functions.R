#I wanted to separate import and cleanup functions to minimize the noise in the aggregation


import_timelog <- function(name = "timelog_for_ps_history.csv", wd = 'C:/R/workspace/source', output = 'simple'){
    #import and cleanup timelog
    setwd(wd)
    timelog <- read.csv(name, header = T , stringsAsFactors=F)
    start_dates <- read.csv("ps_start_dates.csv", header = T , stringsAsFactors=F)
    print(paste(name, "last updated", difftime(Sys.time(), file.info(name)$ctime, units = "days"), "days ago", sep = " "))
    print(paste("ps_start_dates.csv", "last updated", difftime(Sys.time(), file.info("ps_start_dates.csv")$ctime, units = "days"), "days ago", sep = " "))
    
    #trim footer information by removing rows without a valid value for services ID
    timelog <- timelog[substr(timelog$Services.ID,0,3) %in% c("a01"), ]
    
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
    timelog$is_psm <- NA
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
    if(length(timelog[timelog$User.Title %in% ps_titles & is.na(timelog$is_psm), ]$is_psm) > 0){
      timelog[timelog$User.Title %in% ps_titles & is.na(timelog$is_psm), ]$is_psm <- 1
    }
    
    #now all relevant time is marked, remove 0 and na time from timelog
    timelog <- timelog[timelog$is_psm == 1 & !is.na(timelog$is_psm), ]
    
    #Construct the Period Identifiers for service grouping
    timelog$filingPeriod <- paste(as.numeric(format(timelog$Date, "%Y")), ceiling(as.numeric(format(timelog$Date, "%m"))/3), sep = "")
    timelog$reportingPeriod <- ifelse(substr(timelog$filingPeriod, nchar(timelog$filingPeriod), nchar(timelog$filingPeriod)) == 1,
          paste(as.numeric(format(timelog$Date, "%Y")) -1, 4, sep = ""),
          paste(as.numeric(format(timelog$Date, "%Y")), ceiling(as.numeric(format(timelog$Date, "%m"))/3) - 1, sep = ""))

    #****************************** import role dates
    #get role dates
    setwd("C:/R/workspace/source")
    role_dates <- read.csv("ps_start_dates.csv", header = T, stringsAsFactors = F)
    role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))] <- 
      lapply(role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))],FUN = as.Date, format = "%m/%d/%Y")
    
    #set all time to PSM when the title says psm or sr psm
    timelog$role <- NA
    timelog[timelog$User.Title %in% unique(timelog$User.Title)[grep("Professional", unique(timelog$User.Title))],]$role <- "PSM"
    
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
    
    #****************************** /import role dates
    
    #aggregate time by billable and non-billable
    time_billable <- aggregate(Hours ~ Account.Name + reportingPeriod + Billable, FUN = sum, data = timelog)
    names(time_billable) <- c("Account.Name", "reportingPeriod", "Billable", "Hours") #change names to something meaningful
    time_billable[time_billable$Billable == 0 & !is.na(time_billable$Billable), ]$Billable <- rep("Full Service", dim(time_billable[time_billable$Billable == 0 & !is.na(time_billable$Billable), ])[1])  
    time_billable[time_billable$Billable == 1  & !is.na(time_billable$Billable), ]$Billable <- rep("Billable", dim(time_billable[time_billable$Billable == 1 & !is.na(time_billable$Billable), ])[1])
    #aggregate total time
    time_total <- aggregate(Hours ~ Account.Name + reportingPeriod, FUN = sum, data = timelog)
    names(time_total) <- c("Account.Name", "reportingPeriod", "Hours") #change names to something meaningful
    time_total$Billable <- rep("Total", dim(time_total)[1]) #add billable status
    time_total <- time_total[,names(time_billable)] #rearrange to match ordering in time_by_qtr
    
    time_all <- rbind(time_billable, time_total)
    
    time_all <- dcast(time_all, Account.Name ~ reportingPeriod + Billable)

	if(output %in% c("psh")){
		time_all
	}else if(output %in% c("simple")){
		timelog
	}
	
    
}

import_services <- function(name = "services_for_ps_history_R.csv", wd = 'C:/R/workspace/source', output = 'simple'){
    ##import services report
    setwd(wd)
    services <- read.csv(name, header = T , stringsAsFactors=F)
    print(paste(name, "last updated", difftime(Sys.time(), file.info(name)$ctime, units = "days"), "days ago", sep = " "))
    
    #trim footer information by removing rows without a valid value for services ID
    #services <- services[substr(services$Solution.Name,0,3) == "SEC"  & !is.na(services$Solution.Name), ] #Remove non-SEC solutions
    
    #cleanup names and data values
    names(services)[names(services) %in% c("Account..Account.Name")] <- "Account.Name"
    names(services)[names(services) %in% c("PSM..Full.Name")] <- "PSM"
    names(services)[names(services) %in% c("Sr.PSM..Full.Name")] <- "Sr.PSM"
    names(services)[names(services) %in% c("CSM..Full.Name")] <- "CSM"
    names(services)[names(services) %in% c("Sr.CSM..Full.Name")] <- "Sr.CSM"
    names(services)[names(services) %in% c("Churned.Effective.Date")] <- "Churn.Date"
    services$Form.Type[services$Form.Type == 'N/A' & !is.na(services$Form.Type)] <- NA
    services$Goodwill.Hours.Available[services$Goodwill.Hours.Available %in% c("0")] <- NA
    services$Quarter.End <- as.Date(services$Quarter.End, format = "%m/%d/%Y")
    services$Filing.Date <- as.Date(services$Filing.Date, format = "%m/%d/%Y")
    services$Filing.Deadline <- as.Date(services$Filing.Deadline, format = "%m/%d/%Y")
    services$Year.End <- format(services$Year.End, format = "%Y-%U")
    
    #build a lits of unique customers and customer data
    base_info <- c("Account.Name", "CIK", "CSM", "Sr.CSM", "PSM", "Sr.PSM", "Churn.Date", "Year.End", "XBRL.Status", "Goodwill.Hours.Available")
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
    services <- services[!is.na(services$Quarter.End), ] #remove services without quarter ends (can't place them)
    #remove CS migrations for PSH, but not general case
    if(output %in% c("psh")){
      services <- services[!(services$CS.PS %in% c('CS')),] #remove all CS services.
    }else if(output %in% c("simple")){
      services <- services[!(services$CS.PS %in% c('CS') & !(services$Service.Type %in% "Migration")),] #remove all CS services.
    }
    
    services <- services[!(services$Service.Type %in% c('Reserve Hours', 'Other', 'Training', '')), ] #Remove Reserve Projects
    
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
    
    svc_by_qtr <- aggregate(services$Service.Name, by=list(services$Account.Name, services$reportingPeriod), paste, collapse = "\n")
    names(svc_by_qtr) <- c("Account.Name", "reportingPeriod", "Services")
    svc_by_qtr <- dcast(svc_by_qtr, Account.Name ~ reportingPeriod)
    #remove?
    svc_by_qtr <- sapply(svc_by_qtr, function(x) ifelse(x %in% c("NULL"), NA, x)) #change NULLs produced by dcast into NAs
    
    #join unique customers with dcast services    
    merged <- merge(unique_customers, svc_by_qtr, by = "Account.Name", all.x = T)
    merged <- merged[order(merged$Account.Name), ]
    
    
	if(output %in% c("psh")){
		merged[!(merged$XBRL.Status %in% c("None")),]
	}else if(output %in% c("simple")){
		services
	}
    
}

import_sec <- function(name = "filing_data.csv" ){
  setwd('C:/R/workspace/source')
  facts <- read.csv(name, header = T , stringsAsFactors=F)
  print(paste(name, "last updated", difftime(Sys.time(), file.info(name)$ctime, units = "days"), "days ago", sep = " "))
  
  #remove extra fields to remove potential for incomplete cases due to something extraneous
  valid_list <- c("accession_number", "name", "cik", "sic", "form", "form_group", 
             "filing_date", "filing_qtr", "filing_month", "facts", "xbrl_software")
  facts <- facts[, names(facts) %in% valid_list]
  #facts <- facts[facts$xbrl_software %in% c("WebFilings"), ]
  #remove incomplete cases
  facts <- facts[complete.cases(facts),]
  #cast columns properly
  facts$filing_date <- as.Date(facts$filing_date, format = "%m/%d/%Y")
  facts$facts <- as.numeric(facts$facts)
  facts <- facts[facts$form_group %in% c("tens", "amend"),]
  facts <- facts[rev(order(facts$filing_date)), ]
  print(paste(name, "last updated", difftime(Sys.time(), file.info(name)$ctime, units = "days"), "days ago", sep = " "))
  facts
}

import_sales_recommendations <- function(name = "sales_recommendations_for_ps_history.csv" ){
  setwd('C:/R/workspace/source')
  sales_rec <- read.csv(name, header = T , stringsAsFactors=F)
  sales_rec$Service.QED <- as.Date(sales_rec$Service.QED, format = "%m/%d/%Y")
  sales_rec <- sales_rec[!is.na(sales_rec$Service.QED),]
  sales_rec$period <- paste(as.numeric(format(sales_rec$Service.QED, "%Y")), ceiling(as.numeric(format(sales_rec$Service.QED, "%m"))/3) , sep = "")
  
  svc_by_qtr <- aggregate(sales_rec$Product, by=list(sales_rec$Account.Name, sales_rec$period), paste, collapse = "\n")
  names(svc_by_qtr) <- c("Account.Name", "period", "Services")
  result <- dcast(svc_by_qtr, Account.Name ~ period)
  for (i in 1:length(names(result)[grep('[1-9]+', names(result), perl = T)])) {
       names(result)[grep('[1-9]+', names(result), perl = T)][i] <- 
         paste(names(result)[grep('[1-9]+', names(result), perl = T)][i] , "Sales Rec", collapse = "\n")
  }
  print(paste(name, "last updated", difftime(Sys.time(), file.info(name)$ctime, units = "days"), "days ago", sep = " "))
  result
}

import_opportunities <- function(name = "opportunities_for_R.csv"){
  setwd('C:/R/workspace/source')
  opps <- read.csv(name, header = T, stringsAsFactors = F)
  legacy_pricing <- read.csv("legacy_list_pricing.csv", header = T, stringsAsFactors = F)
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
  print(paste(name, "last updated", difftime(Sys.time(), file.info(name)$ctime, units = "days"), "days ago", sep = " "))
  opps
}

import_contracts <- function(name = "contracts_for_pshistory.csv"){
  setwd('C:/R/workspace/source')
  contracts <- read.csv(name, header = T, stringsAsFactors = F)
  contracts$Contract.Start.Date <- as.Date(contracts$Contract.Start.Date, format = "%m/%d/%Y")
  contracts$CIK <- as.numeric(contracts$CIK)
  print(paste(name, "last updated", difftime(Sys.time(), file.info(name)$ctime, units = "days"), "days ago", sep = " "))
  contracts
}

import_hierarchy <- function(name = "hierarchy.csv"){
  setwd('C:/R/workspace/source')
  hierarchy <- read.csv(name, header = T, stringsAsFactors = F)
  print(paste(name, "last updated", difftime(Sys.time(), file.info(name)$ctime, units = "days"), "days ago", sep = " "))
}