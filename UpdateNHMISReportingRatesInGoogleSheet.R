cat(as.character(Sys.time()), "==","Script Update NHMIS data started successfully\n")


setwd("C:/Users/totus tuus/Documents/R_projects/reportingRates/data/")
library(rlang, lib.loc = "C:/Program Files/R/R-3.5.1patched/library")
library(googlesheets, lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library(googledrive)
gs_auth(token = "token.RDS", verbose = FALSE)
new_file_name <- "new.csv"
newData <- read.csv(file = new_file_name, as.is = TRUE, check.names = FALSE)
newData[is.na(newData)] <- ""
sheetIDcsv <- read.csv("C:/Users/totus tuus/Documents/R_projects/reportingRates/data/sheetsID.csv")
key <- as.character(sheetIDcsv$SheetID)

inputsheet <- gs_key(key, verbose = FALSE)
newDataUpdate <- inputsheet %>% gs_edit_cells(ws = "newData", input = newData, verbose = FALSE)
cat(as.character(Sys.time()), "==","Script Update NHMIS data completed successfully\n\n----------\n")

