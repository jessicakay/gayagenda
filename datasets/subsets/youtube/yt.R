
  #   filter(region=="all regions"|is.na(region)) %>%

 youtube2023$region[is.na(youtube2023$region)]<-"all regions"

  youtube2023 %>% 
    filter(region=="all regions") %>%
    select(keyword,year,region,`channel name`) %>% 
    group_by(keyword,region,`channel name`) %>% 
    summarise(count=n()) %>% 
    arrange(desc(count))


  unique(youtube2023$EntryURL)


  # write.csv(youtube_scrape_2023,"~/gayagenda/datasets/subsets/youtube_scrape_2025.csv")

  youtube_scrape -> youtube_scrape_2025
  as.data.frame(youtube_scrape_2025)->youtube_scrape_2025

  names(youtube_scrape_2025)[1]<-"video_ID"
  names(youtube_scrape_2025)[2]<-"posted_on_YT"
  names(youtube_scrape_2025)[3]<-"channel name"
  names(youtube_scrape_2025)[4]<-"yt_long_ID"

merge(youtube_scrape_2025,youtube,by="video_ID") ->youtube2025
select(youtube2024,c(-X,-pullURL,-EntryContent))-> youtube2023

youtube2025 %>% 
  select(EntryPublished,the_day,posted_on_YT,keyword,video_ID,yt_long_ID,`channel name`,EntryURL,region,RSS) -> 
  youtube2025

table(yt2024$`channel name`,yt2024$keyword) |> View()



# calculations for article  4 

mega_ds %>% filter(pullURL=="youtube.com") %>% 
  distinct(EntryURL) |> dim() -> a

mega_ds %>% distinct(EntryURL) |> dim() -> b

mega_ds %>% distinct(EntryURL,.keep_all = T) %>% select(pullURL) |> table() %>% View()

youtube2023 %>% distinct(EntryURL,.keep_all = T) %>% 
  filter(keyword=="transgender")%>%
  filter(region=="all regions") %>%
  select(`channel name`) %>%table() |> View()



c("EntryPublished","the_day","year","month","video_ID",
  "posted_on_YT","channel name","yt_long_ID","EntryTitle",
  "EntryURL","keyword","region","RSS") -> pull_columns

rbind(
youtube2023 %>% select(any_of(pull_columns)),
youtube2024 %>% select(any_of(pull_columns)),
youtube2025 %>% select(any_of(pull_columns)))->yt_2023to2025

write.csv(yt_2023to2025,"~/gayagenda/datasets/subsets/youtube/youtube_2023to2025_merged.csv")