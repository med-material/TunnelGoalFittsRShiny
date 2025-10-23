# --- Libraries ---------------------------------------------------------------
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggpubr)

# CSV upload module
source("modules/csv_upload_module.R", local = TRUE)

options(shiny.maxRequestSize = 100 * 1024^2)

# --- OFFLINE FLAGS + NO-DB STUBS --------------------------------------------
DB_AVAILABLE <- FALSE

# Kept for compatibility with existing server.R code:
RetreiveUniqueColVals <- function(tablename, column) character(0)
RetreiveDataSet      <- function(tablename, column, colvalue) data.frame()
RefreshDataSets      <- function(colfilter, local_data) invisible(NULL)

# Local “refresh” (unchanged logic, just returns a cleaned df)
RefreshDataLocal <- function(df) {
  df$GameType <- factor(df$GameType, levels = c("Goal", "Fitts", "Tunnel"))
  df$PID      <- as.factor(df$PID)
  df$TrialNo  <- as.factor(df$TrialNo)
  df
}