
library(RPostgreSQL)

readRenviron("~/R/.Renviron")
REhost =     Sys.getenv('pgserver')
REport =     Sys.getenv('pgport')
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

# context("dbGetQuery")

  # con <- dbConnect(RPostgres::Postgres())

  con <- dbConnect(PostgreSQL(), user=REuser, password=REpassword, dbname=REdbname) 
  q <- "select * from \"tblInterface\" where \"ObjectName\" = 'Case Study' "
  r <- dbSendQuery(con, q)
  s <- fetch(r)

  dbDisconnect(con)
  

  library(DBI)
  ### DBI native testing
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user=REuser, password=REpassword, dbname=REdbname)
  res <- dbSendQuery(con, "select * from \"tblInterface\" where \"ObjectName\" = 'Case Study'")
  is(drv, "DBIObject")   ## True
  is(con, "DBIObject")   ## True
  is(res, "DBIObject")
  s <- fetch(res)  
  summary(res)
  
  dbDisconnect(con)
  