# Functions to replace the SQL database with a local static database for testing
# Also overrides the RStudio Connect pin data
if("odbc" %in% (.packages())){
  stop("Package `odbc` is attached. Only run `dummy_sql.R` for testing without `odbc` as it replaces the COVID-19 data model with dummy data.")
}

library(sqldf)
library(tibble)
library(dplyr)
library(lubridate)


#' Override odbc::dbGetQuery with sqldf. Note: `TOP N ...` in SQL Server is incompatible
#' with SQLite so queries in the app have been manually recoded to `... LIMIT N`.
dbGetQuery = function(conn, query) {
  sqldf(query)
}
