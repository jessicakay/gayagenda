# fixed TLD grabber
# jessdkant.bsky.social

# read.csv("~/gayagenda/datasets/Jan2026.csv") -> ds

{ library(ggplot2)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(paletteer)
  library(gridExtra)}

# refresh(arg = "ex")

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

  c("dailysignal.com",
    "christanpost.com",
    "washingtonexaminer.com",
    "dailywire.com",
    "foxnews.com",
    "breitbart.com") -> six_sources

   c( "msn.com",
      "yahoo.com") -> portals

   c( "youtube.com",
      "x.com",
      "instagram.com",
      "substack",
      "facebook.com") ->socials

   # top 10 / top 20

    # data cleaning
     mega_ds[which(mega_ds$keyword != "transgenderism"),] -> mega_ds
     mega_ds[which(mega_ds$keyword!="gender confusion"),] -> mega_ds
     mega_ds[which(mega_ds$year!="2026"),] -> mega_ds

     mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"yahoo"))] <- "yahoo.com"
     mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"twitter.com"))] <- "x.com"
     mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"thetimes.com"))] <- "thetimes.co.uk"
     mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"bbc.com"))] <- "bbc.co.uk"


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

  #  ds[which(ds$pullURL=="news.yahoo.com"),]      -> y_a ; dim(a)
  #  ds[which(ds$pullURL=="yahoo.com"),]           -> y_b ; dim(b)
  #  ds[which(ds$pullURL=="uk.yahoo.com"),]        -> y_c ; dim(c)
  #  ds[which(ds$pullURL=="uk.news.yahoo.com"),]   -> y_d ; dim(d)
  #  ds[which(ds$pullURL=="sports.yahoo.com"),]    -> y_e ; dim(e)
  #  dim(y_a)[1]+ dim(y_b)[1]+ dim(y_c)[1]+dim(y_d)[1]+dim(y_e)[1]



# exds_export -> exds
# write.csv(exds_export,"~/gayagenda/datasets/ex_kws.csv")
# exds %>% group_by(keyword,quarter,year) %>% arrange(desc(year)) %>% summarise(n=n()) -> summed
