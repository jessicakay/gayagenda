# load using this, jan 2026
# jessdkant.bsky.social

{ library(ggplot2)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(paletteer)
  library(rvest)
  library(gridExtra)}

#  read.csv("~/gayagenda/datasets/ex_kws.csv") -> exds
#  read.csv("~/gayagenda/datasets/Jan2026.csv") -> ds

read.csv("~/nightly_2026_03_12.csv")-> mega_ds

  # newest merge and clean code
  exds[which(exds$keyword!="woke ideology"),] -> exds
  ds <- subset(ds, select=c(-theday, -X, -topic))
  ds <- subset(ds, select=c(-pullURL))
  exds <- subset(exds,select = c(-X))
  exds[names(exds)[1:11]] -> exds
  exds <- subset(exds, select=c(-pullURL))
  

  # if using refresh(arg="ex"): select(exds,c(names(ds))) -> exds
  union(ds,exds) -> mega_ds
  names(mega_ds)
  
  mega_ds %>%
    mutate(the_day=as.Date(mdy(str_extract(
      EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
    mutate(month=month(lubridate::as_date(the_day))) %>%
    mutate(dayweek=weekdays(the_day)) %>%
    mutate(year = year(lubridate::as_date(the_day))) %>%
    mutate(quarter = quarter(lubridate::as_date(the_day))) %>%
    mutate(RSS=str_extract(FeedURL,"(?<=https://www.google.com/alerts/feeds/[0-9]{20}/)[0-9]+")) %>% 
    mutate(pullURL=
             str_remove(
               str_remove(
                 str_extract(
                   str_remove(EntryURL,"www."),
                   pattern="https?:\\/\\/[a-z0-9A-Z]+[a-z0-9A-Z.-]+/" ),
                 "https?:\\/\\/"),"/")) -> mega_ds
  
  # remove unneeded columns
  subset(mega_ds, select=c(-FeedTitle, -FeedURL)) -> mega_ds
  

  # mega_ds[which(mega_ds$year!="2026"),] -> mega_ds
    
    mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"yahoo"))] <- "yahoo.com"
    mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"twitter.com"))] <- "x.com"
    mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"thetimes.com"))] <- "thetimes.co.uk"
    mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"bbc.com"))] <- "bbc.co.uk"

  # mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"substack.com"))] <- "substack.com"
  
   c( "msn.com","yahoo.com","aol.com", "pressreader.com") -> portals
   c( "youtube.com",
      "x.com",
      "instagram.com",
      "substack",
      "facebook.com") ->socials 
   c("transgender", "biological sex", "gender identity")-> main_kws
   c("nature.com","sciencedirect.com") -> exc_sites
 
   top_outlets<-function(dataset_name, number){
     dataset_name %>% filter(is.na(pullURL)==FALSE) %>%
       filter(keyword %in% main_kws) %>%
       filter(!(pullURL %in% portals)) %>% filter(!(pullURL %in% socials)) %>% filter(!pullURL %in% exc_sites) %>%
       distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
       arrange(by_group=desc(n)) %>% head(n=number) %>% select(pullURL) -> tophits
     as.vector(tophits$pullURL) ->> topouts} 
   
 # mega_ds$region[which(is.na(mega_ds$region))] <- "not set"
 #  mega_ds$region[which(is.na(mega_ds$region))] <- "all regions"
 #  table(mega_ds$region, useNA = "always")
   
   {
         getwd()-> curr_dir 
         setwd("~/gayagenda/datasets/")
          
         paste("nightly",gsub(
            pattern = "-", replacement = "_", 
            Sys.Date()), sep="_")    -> file_handle                   # generate filenames 
         paste0(file_handle,".csv")  -> file_csv
         paste0(file_handle,".7z")   -> file_7z
          
         write.csv(mega_ds, file_csv)                                 # create nightly export
         system2(command = "7z",args = c("a",file_7z,file_csv))       # zip it for repo
         system2(command = "rm",args = c(file_csv))                   # remove CSV, leaving archive

         system2(command = "git",args = c("add",file_7z))             # push to github
         system2(command = "git",args = c("commit","-m",file_handle))
         system2(command = "git",args = c("push","origin","main"))
      
         setwd(curr_dir)
   }