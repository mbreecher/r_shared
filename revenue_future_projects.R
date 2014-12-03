# import function to read revenue data from database
get_revenue_data <- function(){
  library(RMySQL)
  setwd("C:/R/workspace/shared")
  source("get_query.r")
  
  
  #grab project and collapsed time data from mysql database
  con <- dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "revenue_analysis")
  
  sql <- paste("select subcloud.service_id, subcloud.list_price, subcloud.sales_price, subcloud.opportunity_id 
               from subcloud
               where subcloud.service_id like 'a0%'", sep = "")                
  
  query <- dbGetQuery(con, sql)
  dbDisconnect(con)
  
  #import 2013Q2+ services per scheduled services
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  services <- import_services()
  services <- services[services$reportingPeriod >= '20132' & !is.na(services$reportingPeriod), ]
  
  setwd("C:/R/workspace/Ali")
  
  result <- merge(services, query, by.x = "Services.ID", by.y = "service_id", all.x = T)
  
  #temp abigail changes
  setwd("C:/R/workspace/Ali")
  price_update <- read.csv("abigail_price_updates.csv", header = T, stringsAsFactors = F)
  check <- c()
  for (id in unique(price_update$Services.ID)){
    if(length(result[result$Services.ID %in% id,]$Services.ID) > 0){
      result[result$Services.ID %in% id,]$list_price <- price_update[price_update$Services.ID %in% id,]$list_price_updated
      result[result$Services.ID %in% id,]$sales_price <- price_update[price_update$Services.ID %in% id,]$sales_price_updated 
    }
  }
  
  result
}

setwd("C:/R/workspace/output")
write.csv(result, file = "q22013_forward_projects_with_sales_price.csv", row.names = F, na = "")