library(RMySQL)
library(plyr)
library(ggplot2)


my_data <- read.csv("credentials.csv", header = TRUE, sep = ",", colClasses = c("character", "character", "character", "character"))
source("modules/csv_upload_module.R", local = T)
options(shiny.maxRequestSize=100*1024^2)
lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)


mydb <- dbConnect(MySQL(),
  user = my_data[1, "username"],
  # rstudioapi::askForPassword("Database user"),
  password = my_data[1, "password"],
  # rstudioapi::askForPassword("Database password"),
  dbname = my_data[1, "dbname"],
  host = my_data[1, "host"]
)

# RetreiveUniqueColmnVals() Used to get unique values available for a column
# USAGE:
# dtest = RetreiveUniqueColmnVals("Email")
RetreiveUniqueColVals <- function(tablename, column) {
  queryString <- paste("SELECT DISTINCT", column, "FROM", tablename, sep = " ")
  res <- dbSendQuery(mydb, queryString)
  vals <- fetch(res, n = -1)
  dbClearResult(dbListResults(mydb)[[1]])
  return(unname(unlist(vals))) # if there are several values, they arrive as a list, so unlist them on arrival.
}

all_accounts <- RetreiveUniqueColVals("tunnel_fit_test", "Email")


# RetreiveDataSet() Used to query for a specific dataset.
# Setting colvalue to NULL retreives all data.
# USAGE:
# dtest = RetreiveDataSet("tunnel_fit_test","Email","mhel@create.aau.dk")
RetreiveDataSet <- function(tablename, column, colvalue) {
  queryString <- "SELECT *"
  queryString <- paste(queryString, "FROM", tablename, sep = " ")
  if (colvalue != "NA") {
    queryString <- paste(queryString, "WHERE", column, "= ", sep = " ")
    queryString <- paste(queryString, "\'", colvalue, "\'", sep = "")
  }
  print(queryString)
  res <- dbSendQuery(mydb, queryString)
  df <- fetch(res, n = -1)
  dbClearResult(dbListResults(mydb)[[1]])
  return(df)
}

# RefreshDataSet is a helper function called in the app for refreshing TunnelGoalFitts dataset.
# Setting colfilter to NULL retreives all data.
# USAGE:
# RefreshDataSets("mhel@create.aau.dk")
RefreshDataSets <- function(colfilter) {
  if (colfilter == "-1") {
    # -1 is the default value R Shiny uses on startup.
    return()
  }
  if (local_data) {
    # -1 is the default value R Shiny uses on startup.
    return()
  }
  # REFRESH REACTION TIME DATASET
  df_all <<- RetreiveDataSet("tunnel_fit_test", "Email", colfilter)
  df_all$GameType <<- as.factor(df_all$GameType)
  df_all$GameType <<- factor(df_all$GameType, levels = c("Goal", "Fitts", "Tunnel"))
  df_all$PID <<- as.factor(df_all$PID)
  df_all$TrialNo <<- as.factor(df_all$TrialNo)
}

RefreshDataLocal <- function() {
  # REFRESH REACTION TIME DATASET
  df_all$GameType <<- as.factor(df_all$GameType)
  df_all$GameType <<- factor(df_all$GameType, levels = c("Goal", "Fitts", "Tunnel"))
  df_all$PID <<- as.factor(df_all$PID)
  df_all$TrialNo <<- as.factor(df_all$TrialNo)
  local_data <<- T
}

local_data = F
df_all <- data.frame()
