import_wdesk_app_filing_data <-function(){
  setwd("C:/R/workspace/shared")
  source("wh_query.R")
  app_filing <- wh_query("select *, 
                        if (live_filing_flag = 1,1,0) AS live_filing_flag_corrected ,
                        if (official_sec_filing_flag = 1,1,0) AS official_sec_filing_flag_corrected
                       from f_filing
                       join (select company_id, central_index_key, intacct_id from d_company) as d_company
                       using (company_id)")
                         
   app_filing$last_update <- as.Date(app_filing$last_update, format = "%Y-%m-%d %H:%M:%S")
   app_filing$created <- as.Date(app_filing$created, format = "%Y-%m-%d %H:%M:%S")
   app_filing$filing_agent_cik <- substr(app_filing$accession_number,1,10)
   app_filing$year <- substr(app_filing$accession_number,12,13)
   app_filing$count <- as.numeric(substr(app_filing$accession_number,15,20))
   
   app_filing
}