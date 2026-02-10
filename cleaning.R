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
    # mutate(myear=my(lubridate::as_date(the_day)))
    mutate(year = year(lubridate::as_date(the_day))) %>%
    mutate(quarter = quarter(lubridate::as_date(the_day))) %>%
    mutate(pullURL=
             str_remove(
               str_remove(
                 str_extract(
                   str_remove(EntryURL,"www."),
                   pattern="http?s:\\/\\/[a-z0-9A-Z]+[a-z0-9A-Z.-]+/" ),
                 "http?s:\\/\\/"),"/")) -> mega_ds
  
  # data cleaning
  mega_ds[which(mega_ds$keyword != "transgenderism"),] -> mega_ds
  mega_ds[which(mega_ds$keyword!="gender confusion"),] -> mega_ds
  mega_ds[which(mega_ds$year!="2026"),] -> mega_ds
  
  mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"yahoo"))] <- "yahoo.com"
  mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"twitter.com"))] <- "x.com"
  mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"thetimes.com"))] <- "thetimes.co.uk"
  mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"bbc.com"))] <- "bbc.co.uk"


  { c("dailysignal.com",
    "christanpost.com",
    "washingtonexaminer.com",
    "dailywire.com",
    "foxnews.com",
    "breitbart.com") -> six_sources

   c( "msn.com","yahoo.com") -> portals

   c( "youtube.com",
      "x.com",
      "instagram.com",
      "substack",
      "facebook.com") ->socials }
