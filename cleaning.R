# load using this, jan 2026
# jessdkant.bsky.social

{ library(ggplot2)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(paletteer)
  library(gridExtra)}

  read.csv("~/gayagenda/datasets/ex_kws.csv") -> exds
  read.csv("~/gayagenda/datasets/Jan2026.csv") -> ds

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
    mutate(pullURL=
             str_remove(
               str_remove(
                 str_extract(
                   str_remove(EntryURL,"www."),
                   pattern="http?s:\\/\\/[a-z0-9A-Z]+[a-z0-9A-Z.-]+/" ),
                 "http?s:\\/\\/"),"/")) -> mega_ds
  
  # remove unneeded columns
  subset(mega_ds, select=c(-FeedTitle, -FeedURL, -EntryPublished)) -> mega_ds
  
  # mega_ds[which(mega_ds$year!="2026"),] -> mega_ds
  
  mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"yahoo"))] <- "yahoo.com"
  mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"twitter.com"))] <- "x.com"
  mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"thetimes.com"))] <- "thetimes.co.uk"
  mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"bbc.com"))] <- "bbc.co.uk"

   c( "msn.com","yahoo.com","aol.com") -> portals
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
    
 