# frequency stats
# jessdkant.bsky.social

ds %>% 
  filter(keyword=="transgender") %>% 
  filter(region=="all regions") %>%
  group_by(EntryURL) -> cleaned_ds

View(cleaned_ds)

refresh(arg = "ex")

exds %>% 
  mutate(the_day=as.Date(mdy(str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) ->> exds
  substring(str_extract(exds$EntryURL, pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z]+/"), first=9) -> exds$pullURL

exds %>% 
  filter(keyword!="woke ideology") %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>%
  mutate(quarter = quarter(lubridate::as_date(the_day))) -> exds_export
  
exds_export -> exds 

exds %>%
  group_by(keyword,quarter,year) %>%
  arrange(desc(year)) %>%
  summarise(n=n()) 

View(exds)

