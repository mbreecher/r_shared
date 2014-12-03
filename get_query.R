load_query <- function(service_type = '%', form_type = '%', period = '%', hour_lim = 0){
  
  con <- dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "revenue_analysis")
  
  sql <- paste("select subcloud.service_id, subcloud.opportunity_id, timelog.is_psm, subcloud.account_name, subcloud.service_name, 
                subcloud.service_type,subcloud.form, subcloud.service_period, timelog.period, subcloud.list_price, subcloud.cs_ps,
                subcloud.sales_price, subcloud.period_int, timelog.relative_week_num, subcloud.last_k, subcloud.last_q,
                subcloud.avg_q, subcloud.service_status, subcloud.filing_deadline_recalc, subcloud.quarter_end, timelog.PSM, sum(timelog.hours) 
                from subcloud
                left join timelog on subcloud.service_id collate latin1_bin = timelog.service_id collate latin1_bin
                where subcloud.service_id like 'a0%' and timelog.is_psm = 1 and subcloud.service_status = 'Completed'
                  and subcloud.service_type like '",service_type,"' 
                  and subcloud.form like '", form_type,"'
                  and subcloud.service_period like '", period,"'
                  and timelog.relative_week_num >= 0
                group by subcloud.service_id, subcloud.account_name, timelog.is_psm, timelog.PSM;", sep = "")                
                
  query <- dbGetQuery(con, sql)
  if(!is.null(query)){names(query)[names(query) == "sum(timelog.hours)"] <- "time"}
  
  dbDisconnect(con)
  query
}