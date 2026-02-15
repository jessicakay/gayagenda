# subsets

mega_ds %>% filter(pullURL=="foxnews.com") %>% 
   distinct(EntryURL, .keep_all = T) %>% 
  filter(year != "2026") %>%
  select(the_day,dayweek,month, quarter,year,EntryTitle,EntryURL,keyword,pullURL) %>%
  mutate(type=case_when
                   (
                     str_detect(EntryURL,"/video/") == TRUE ~ "video",
                     str_detect(EntryURL,"/video/") == FALSE ~ "article"
                   )) %>% 
  mutate(cat=gsub("/","",str_extract(EntryURL,pattern="/[a-z-]+/"))) %>% 
  write.csv("~/gayagenda/datasets/subsets/just_fox.csv",row.names = FALSE)

top_outlets(mega_ds,100)

mega_ds %>%
  filter(pullURL %in% topouts) %>%
  distinct(EntryURL, .keep_all = T) %>% 
  select(the_day,dayweek,month, quarter,year,EntryTitle,EntryURL,keyword,pullURL) -> top_20

  top_20 %>% write.csv("~/gayagenda/datasets/subsets/top_20.csv",row.names = FALSE)

  mega_ds %>% filter(year=="2026") %>% 
    filter(pullURL=="foxnews.com") %>% 
    distinct(EntryURL, .keep_all = TRUE) %>% 
    mutate(type=case_when(
                     str_detect(EntryURL,"/video/") == TRUE ~ "video",
                     str_detect(EntryURL,"/video/") == FALSE ~ "article"
                   )) %>% 
    select(month, year, type, EntryTitle,EntryURL,pullURL,cat)  
    