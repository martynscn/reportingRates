cat(as.character(Sys.time()), "==","Script Download data started successfully\n")


library("rlang", lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library("httr", lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library("rjson", lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library("plyr", lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library("googlesheets", lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library("dplyr", lib.loc = "C:/Program Files/R/R-3.5.1patched/library",warn.conflicts = FALSE)
library("XML", lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library("RCurl", lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )
library("tidyr", lib.loc = "C:/Program Files/R/R-3.5.1patched/library", warn.conflicts = FALSE)
library("data.table", lib.loc = "C:/Program Files/R/R-3.5.1patched/library",warn.conflicts = FALSE)
library("tibble", lib.loc = "C:/Program Files/R/R-3.5.1patched/library" )

config <- config::get()
username <- config$NHMISInstanceUsername
password <- config$NHMISInstancePassword
server_version <- config$ServerVersion

timeout <-1000
if(server_version == "yes") {
  setwd("data")
  # source("/srv/shiny-server/e4e-apps/functions/collect_return_period_fxn2.R")
} else if (server_version == "no") {
  setwd("~/R_projects/reportingRates/data/")
  # source("~/R_projects/R programming/DHIS2 data extract/Functions/collect_return_period_fxn2.R")
}



files <- list.files()
no_of_files <- length(files) 

combined_file_name <- "combined.csv"
new_file_name <- "new.csv"
ind_file_name <- paste0("reportingRates","_",(no_of_files + 1),".csv")


  analytics_url <- "https://dhis2nigeria.org.ng/dhis/api/29/analytics.json?&dimension=dx:lyVV9bPLlVy.REPORTING_RATE&dimension=ou:s5DPBsdoE8b;LEVEL-1;LEVEL-2&dimension=pe:LAST_MONTH&skipMeta=true&skipData=false&skipRounding=true&hierarchyMeta=false&ignoreLimit=true&hideEmptyRows=false&showHierarchy=false&includeNumDen=false&displayProperty=NAME&outputIdScheme=NAME&tableLayout=true&columns=dx&rows=pe;ou"
  url <- URLencode(analytics_url)
  r <- httr::GET(url = url, httr::authenticate(username,password),httr::timeout(seconds = timeout))
  
  r <- httr::content(r, "text")
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  r0 <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  names(r0) <- d$headers$column
  r1 <- mutate(.data = r0, States = gsub("^( )| state","",organisationunitname,ignore.case = TRUE))
  
  r2 <- separate(data = r1, col = States, into = c("Abbr","States"),sep = " ",remove = TRUE, convert = FALSE,
           extra = "merge", fill = "warn")
  r3 <- mutate(r2, States = gsub("Federal Capital Territory","FCT",gsub("Federal Government","National",States)))
  Rdata <- select(.data = r3,periodname,States,`NHMIS Monthly Summary (version 2013) Reporting rate`)

  RdataTransposed <- transpose(Rdata[,2:3])
  names(RdataTransposed) <- RdataTransposed[1,]
  RRData0 <- add_column(.data = RdataTransposed, Date = as.character(Sys.time()), .before = 1)
  RRData <- RRData0[2,]
  write.csv(RRData,new_file_name, row.names = FALSE)
  newData <- read.csv(file = new_file_name, as.is = TRUE, check.names = FALSE)
initialCombinedData <- read.csv(file = combined_file_name, as.is = TRUE, check.names = FALSE)
combinedData <- bind_rows(initialCombinedData,newData)

# Run first time only
# write.csv(as.data.frame(RRData),combined_file_name, row.names = FALSE)
write.csv(combinedData,combined_file_name, row.names = FALSE)
write.csv(as.data.frame(RRData),file = ind_file_name,row.names = FALSE)
cat(as.character(Sys.time()), "==","Script Download data completed successfully\n\n----------\n")
