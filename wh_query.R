wh_query <- function(sql = ""){
  if (sql == ""){break}
  load(file.path("C:/", "creds.Rdata"))
  
  con <- dbConnect(drv = dbDriver("MySQL"),
                  host = "etldatawarehouse-prod-56.cviolxtuowdr.us-east-1.rds.amazonaws.com",
                  dbname = "wf_warehouse",
                  user = dw_username,
                  password = dw_password
  )
                
  query <- dbGetQuery(con, sql)
  if(!is.null(query)){names(query)[names(query) == "sum(timelog.hours)"] <- "time"}
  
  dbDisconnect(con)
  query
}