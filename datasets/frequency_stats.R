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

union(ds,exds) -> mega_ds
  
mega_ds %>% 
  filter(keyword!="woke ideology") %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>%
  mutate(quarter = quarter(lubridate::as_date(the_day))) -> mega_ds

exds %>% 
  filter(keyword!="woke ideology") %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>% 
  group_by(keyword,year) %>%
  arrange(desc(the_day)) %>%
  summarise(n=n())

# exds_export -> exds 
# write.csv(exds_export,"~/gayagenda/datasets/ex_kws.csv") 

# exds %>%
#  group_by(keyword,quarter,year) %>% arrange(desc(year)) %>% summarise(n=n()) -> summed

mega_ds %>%
  group_by(keyword,quarter,year) %>% arrange(desc(year)) %>% summarise(n=n()) -> summed

# install.packages("devtools")
# install.packages("paletteer")

c("www.dailysignal.com/",
  "www.christanpost.com/",
  "www.washingtonexaminer.com/",
  "www.dailywire.com/",
  "www.foxnews.com/",
  "www.breitbart.com/"
) -> six_sources

exds %>% 
  filter(pullURL %in% six_sources) %>%
  filter(keyword!="woke ideology") %>%
  distinct(EntryURL, .keep_all = TRUE) %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>% 
  group_by(keyword,year) %>%
  arrange(desc(the_day)) %>%
  summarise(n=n())
