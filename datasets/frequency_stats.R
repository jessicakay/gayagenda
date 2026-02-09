# frequency stats
# jessdkant.bsky.social

{ library(ggplot2)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(gridExtra)} 


# refresh(arg = "ex")

read.csv("~/gayagenda/datasets/ex_kws.csv") -> exds
read.csv("~/gayagenda/datasets/Jan2026.csv") -> ds


ds <- subset(ds, select=c(-theday, -X, -topic))
exds <- subset(exds,select = c(-X))
union(ds,exds) -> mega_ds
  

# exds_export -> exds 
# write.csv(exds_export,"~/gayagenda/datasets/ex_kws.csv") 

# exds %>%
#  group_by(keyword,quarter,year) %>% arrange(desc(year)) %>% summarise(n=n()) -> summed

mega_ds %>%
  group_by(keyword,quarter,year) %>% arrange(desc(year)) %>% summarise(n=n()) -> summed

# install.packages("devtools")
# install.packages("paletteer")

  c("dailysignal.com",
    "christanpost.com",
    "washingtonexaminer.com",
    "dailywire.com",
    "foxnews.com",
    "breitbart.com"
  ) -> six_sources

exds[which(exds$keyword!="woke ideology"),] -> exds
    
exds %>% 
  mutate(the_day=as.Date(mdy(str_extract(
    EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>%
  mutate(quarter = quarter(lubridate::as_date(the_day))) %>%
  mutate(pullURL=
           str_remove(
             str_remove(
               str_extract(
                 str_remove(EntryURL,"www."),
                 pattern="http?s:\\/\\/[a-z0-9A-Z]+[a-z0-9A-Z.-]+/" ),
               "http?s:\\/\\/"),"/")) -> exds

exds %>% 
  filter(pullURL %in% six_sources) %>%
  distinct(EntryURL, .keep_all = TRUE) %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>% 
  group_by(keyword,year) %>%
  arrange(desc(the_day)) %>%
  summarise(n=n()) -> x


ds %>% filter(is.na(pullURL)==FALSE) %>%
  distinct(EntryURL, .keep_all = TRUE) %>%
  group_by(pullURL) %>%
   summarize(n=n()) %>% 
   arrange(by_group=desc(n)) %>%
   head(n=20) %>% select(pullURL) -> top_20



