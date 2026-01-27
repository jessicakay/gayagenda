# frequency stats
# jessdkant.bsky.social

ds %>% 
  filter(keyword=="transgender") %>% 
  filter(region=="all regions") %>%
  group_by(EntryURL) -> cleaned_ds
View(cleaned_ds)

refresh(arg = "ex")
exds %>% 
  filter(keyword!="woke ideology") %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>% 
  group_by(keyword,year) %>%
  arrange(desc(the_day)) %>%
  summarise(n=n())
