setwd("C:/Users/totus tuus/Documents/R_projects/reportingRates/data/")
library(rlang, lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library(googlesheets, lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library(googledrive)
gs_auth(token = "token.RDS")
combined_file_name <- "combined.csv"
# new_file_name <- "new.csv"
# newData <- read.csv(file = new_file_name, as.is = TRUE, check.names = FALSE)
combinedData <- read.csv(file = combined_file_name, as.is = TRUE, check.names = FALSE)

inputsheet <- gs_key("1cK9sez34vFIclB3DCF7EVder0eNi5tV-KDBpLEW7qiM")
# newDataUpdate <- inputsheet %>% gs_edit_cells(ws = "newData", input = newData)

combinedDataUpdate <- inputsheet %>% gs_edit_cells(ws = "combinedData", input = combinedData)
print(paste(as.character(Sys.time()), "__", "Script Update Combined NHMIS data completed successfully\n\n\n---------"))

