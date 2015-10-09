wh_query <- function(sql = ""){
  require('RMySQL')
  if (sql == ""){break}
  load(file.path("C:/", "creds.Rdata"))
  
  con <- dbConnect(drv = dbDriver("MySQL"),
                  host = "datawarehouse-prod.workiva.net",
                  dbname = "wf_warehouse",
                  user = dw_username,
                  password = dw_password
  )
                
  query <- dbGetQuery(con, sql)
  
  dbDisconnect(con)
  query
}