pg_query <- function(sql = ""){
  require(RPostgreSQL)
  if (sql == ""){break}
  load(file.path("C:/", "creds.Rdata"))
  
  con <- dbConnect(drv = dbDriver("PostgreSQL"),
                   host = pg_host,
                   dbname = "debug3_db",
                   port = 8084,
                   user = pg_username,
                   password = pg_password
  )
  
  query <- dbGetQuery(con, sql)
  
  dbDisconnect(con)
  query
}