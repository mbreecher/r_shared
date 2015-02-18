# import function to read revenue data from database
get_revenue_data <- function(remove_blank_oppid = T){
  opportunities <- import_opportunities()
  opportunities <- opportunities[,names(opportunities) %in% c("Line.Item.18.Digit.Id", "List.Price", "Sales.Price")]
  
  #import 2013Q2+ services per scheduled services
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  services <- import_services()
  if(remove_blank_oppid == T){
    services <- services[!services$OpportunityLineItem.Id %in% "",]  
  }else{
    blank_oppid <- services[services$OpportunityLineItem.Id %in% "",]  
    blank_oppid$List.Price = -1; blank_oppid$Sales.Price = -1
    services <- services[!services$OpportunityLineItem.Id %in% "",]  
  }
  services <- services[services$reportingPeriod >= '20132' & !is.na(services$reportingPeriod), ]
  
  setwd("C:/R/workspace/Ali")
  
  opportunities <- opportunities[opportunities$Line.Item.18.Digit.Id %in% services$OpportunityLineItem.Id,]
  result <- merge(services, opportunities, by.x = "OpportunityLineItem.Id", by.y = "Line.Item.18.Digit.Id", all.x = T)
  
  if(remove_blank_oppid == F){
    result <- rbind(result, blank_oppid)
  }
  
  #temp abigail changes
  setwd("C:/R/workspace/Ali")
  price_update <- read.csv("abigail_price_updates.csv", header = T, stringsAsFactors = F)
  check <- c()
  for (id in unique(price_update$Services.ID)){
    if(length(result[result$Services.ID %in% id,]$Services.ID) > 0){
      result[result$Services.ID %in% id,]$List.Price <- price_update[price_update$Services.ID %in% id,]$list_price_updated
      result[result$Services.ID %in% id,]$Sales.Price <- price_update[price_update$Services.ID %in% id,]$sales_price_updated 
    }
  }
  
  result
}

# setwd("C:/R/workspace/output")
# write.csv(result, file = "q22013_forward_projects_with_sales_price.csv", row.names = F, na = "")