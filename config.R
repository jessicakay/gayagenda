
# gayagenda config files
# jessica kant, 2024 

# only to update R base

if(version$major==4 && version$minor==3.3){
  cat("\n",version$version.string," loaded.\n")}else{
    if("installr" %in% installed.packages()==FALSE){
      install.packages("airtable")} else {installr::install.R()}}

cat(paste(
  "package(s): ",libs[which(as.vector(libs %in% as.data.frame(installed.packages())$Package) == TRUE)]," already installed"))

cat(paste(
  "package(s): ",libs[which(as.vector(libs %in% as.data.frame(installed.packages())$Package) == FALSE)]," not found"))

cat(paste(
  "package(s): ",libs[which(as.vector(libs %in% as.data.frame(installed.packages())$Package) == TRUE)]," already installed"))

cat(paste(
  "package(s): ",libs[which(as.vector(libs %in% as.data.frame(installed.packages())$Package) == FALSE)]," not found"))

c("ggplot2",
  "dplyr",
  "stringr",
  "rairtable",
  "lubridate",
  "gridExtra",
  "googlesheets4") -> libs

install.packages(
  libs[which(as.vector(c("ggplot2",
                         "dplyr",
                         "stringr",
                         "rairtable",
                         "lubridate",
                         "gridExtra",
                         "googlesheets4") %in% 
                         as.data.frame(installed.packages())$Package) == FALSE)])

for(l in 1:length(libs)){library(l)}


libs[which(as.vector(libs %in% as.data.frame(installed.packages())$Package) == FALSE)]

detach(package:googlesheets4) ; googlesheets4::gs4_auth()
headers <- c("EntryPublished","EntryTitle","EntryURL","EntryContent","FeedTitle","FeedURL","keyword","region")
headers   <- as.data.frame(cbind(headers))
googlesheets4::sheet_append(tsheetall,as.vector(headers),sheet =1) # inserts headers into blank sheet, only run first time

 
