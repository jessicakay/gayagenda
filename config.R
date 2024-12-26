
# config files
# jessica kant, 2024 

# package list

c("ggplot2",
  "dplyr",
  "stringr",
  "rairtable",
  "lubridate",
  "gridExtra",
  "googlesheets4") -> libs

# run me if on a new/different machine <3

   if(version$major==4 && version$minor==3.3){
    cat("\n",version$version.string," already installed and loaded.\n\n")}else{
      if("installr" %in% installed.packages()==FALSE){
        install.packages("installr")} else {installr::install.R()}}
  
   cat("checking package(s):\n\n",
      paste(libs[which(as.vector(libs %in% as.data.frame(installed.packages())$Package) == TRUE)]," already installed\n"),
      "\n",libs[which(as.vector(libs %in% as.data.frame(installed.packages())$Package) == FALSE)]," not found")
  
    install.packages(
      libs[which(as.vector(c(libs) %in% as.data.frame(installed.packages())$Package) == FALSE)])

# load all packages
    
sapply(libs, require,character.only = TRUE)
