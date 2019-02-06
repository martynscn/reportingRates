cat(as.character(Sys.time()), "==","Script Update NHMIS data started successfully\n")



library(rlang)
library(googlesheets)
library(googledrive)
gs_auth(token = "token.RDS", verbose = FALSE)
setwd("data")
new_file_name <- "new.csv"
newData <- read.csv(file = new_file_name, as.is = TRUE, check.names = FALSE)
newData[is.na(newData)] <- ""
sheetIDcsv <- read.csv("sheetsID.csv")
key <- as.character(sheetIDcsv$SheetID)

inputsheet <- gs_key(key, verbose = FALSE)
newDataUpdate <- inputsheet %>% gs_edit_cells(ws = "newData", input = newData, verbose = FALSE)
# print(paste(as.character(Sys.time()), "__", "Script Update NHMIS data completed successfully"))
cat(as.character(Sys.time()), "==","Script Update NHMIS data completed successfully\n\n----------\n")