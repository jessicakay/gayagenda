

# note: unspecified and all regions is merged, as these are functionally the same.

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

mega_ds %>% distinct(EntryURL,.keep_all= T) %>% select(pullURL) |> table() %>% View()

youtube2023 %>% distinct(EntryURL,.keep_all = T) %>% 
  filter(keyword=="transgender")%>%
  filter(region=="all regions") %>%
  select(`channel name`) %>%table() |> View()


c("EntryPublished","video_ID",
  "posted_on_YT","channel name","yt_long_ID","EntryTitle",
  "EntryURL","keyword","region","RSS") -> pull_columns

rbind(
youtube2023 %>% select(any_of(pull_columns)),
youtube2024 %>% select(any_of(pull_columns)),
youtube2025 %>% select(any_of(pull_columns)))->yt_2023to2025

write.csv(yt_2023to2025,"~/gayagenda/datasets/subsets/youtube/youtube_2023to2025_merged.csv")

yt_2023to2025 %>%
  mutate(the_day=as.Date(mdy(str_extract(
    EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
  mutate(month=month(lubridate::as_date(the_day))) %>%
  mutate(dayweek=weekdays(the_day)) %>%
  mutate(year = year(lubridate::as_date(the_day))) %>%
  mutate(quarter = quarter(lubridate::as_date(the_day))) %>%
  mutate(ym=paste0(month,"-",year)) %>%
  arrange(the_day)-> yt_df

# checking my work

yt_2023to2025 %>% select(region,keyword) |> table() # why is UK missing? 

mega_ds %>% filter(pullURL=="youtube.com") %>%   # UK never served YT as news??
  select(region) |> table()                      # returns 5207 for all regions, 4505 for US
                                                                        
mega_ds %>% filter(pullURL=="youtube.com") %>% select(EntryURL) |> unique() |> dim() 

yt_2023to2025 %>%
  filter(region=="all regions") |> dim() # 5623 entries


# plots

#----------#

{
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    theme_dark()+
    xlab("")+ylab("")+
    labs(title = "YouTube served on Google News by keyword",
         subtitle = "2023-2025, all regions\n\n")+
    geom_bar(aes(x=keyword,fill=year))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none")+
    scale_fill_paletteer_d("yarrr::cars")-> plot_1
  
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    theme_dark()+
    xlab("")+ylab("jessk.org/blog")+
    geom_bar(aes(x=keyword,fill=year),position = position_dodge(preserve = "single"))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          axis.text.x = element_text(color="white"),
          legend.position = "bottom")+
    scale_fill_paletteer_d("yarrr::cars") -> plot_2
  
  grid.arrange(plot_1, plot_2,ncol=1)
  
}

{
yt_df %>%
  filter(region=="all regions")%>%
  filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
  mutate(year=as.factor(year))%>%
  ggplot()+
  theme_dark()+
  xlab("")+ylab("")+
  labs(title = "YouTube served on Google News by keyword and quarter",
       subtitle = "2023-2025, all regions\n\n")+
    geom_bar(aes(x=keyword,fill=as.factor(quarter)))+
  theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
        panel.background = element_rect("black"),legend.background = element_rect("black"),
        legend.box.background = element_rect("black"),legend.key = element_rect("black"),
        text = element_text(colour = "white"),
        legend.position = "none")+
  scale_fill_paletteer_d("yarrr::info")+
  facet_grid(.~year)-> plot_b

yt_df %>%
  filter(region=="all regions")%>%
  filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
  mutate(year=as.factor(year))%>%
  ggplot()+
  xlab("")+ylab("jessk.org/blog")+
  labs(fill="quarter")+
  theme_dark()+
  geom_bar(aes(x=keyword,fill=as.factor(quarter)),position = position_dodge(preserve = "single"))+
  theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
        panel.background = element_rect("black"),legend.background = element_rect("black"),
        legend.box.background = element_rect("black"),legend.key = element_rect("black"),
        text = element_text(colour = "white"),
        legend.position = "bottom",
        axis.text.x = element_text(color="white"),
        strip.text = element_blank())+
  scale_fill_paletteer_d("yarrr::info")+
  facet_grid(.~year)-> plot_c

grid.arrange(plot_b, plot_c,ncol=1)

}


#----------#

  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    theme_dark()+
    geom_bar(aes(x=year,fill=year),position = position_dodge(preserve = "single"))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "bottom")+
    scale_fill_paletteer_d("yarrr::info")+
      facet_grid(.~keyword)-> simple_year
  
  
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    theme_dark()+
    geom_bar(aes(x=year,fill=as.factor(month)),position = position_dodge(preserve = "single"))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "bottom")+
    scale_fill_paletteer_d("beyonce::X98")+
    facet_grid(.~keyword)
  
  
  yt_2023to2025 %>% distinct(EntryURL,.keep_all = T) %>% 
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    filter(region=="all regions") %>%
    mutate(the_day=as.Date(mdy(str_extract(
      EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
    mutate(month=month(lubridate::as_date(the_day))) %>%
    mutate(year = year(lubridate::as_date(the_day))) %>%
    group_by(`channel name`)%>%
    mutate(num_per_channel=n()) %>%
    group_by(`channel name`, keyword)%>%
    mutate(num_channel_by_kw=n()) %>% 
    group_by(`channel name`,keyword,year)%>%
    mutate(num_chan_kw_year=n())%>%
    select(year,EntryTitle,contains("num")) %>% View()
  
  as.data.frame(x$items) |> select(contains("snippet.tag")) |> names()
  
  yt_2023to2025 %>% rename(channel_unique_ID=yt_long_ID) -> yt_2023to2025
  merge(yt_2023to2025,subscriber_scrape,by=c("channel_unique_ID"))
  left_join(yt_2023to2025,subscriber_scrape,by=c("channel_unique_ID")) -> yt_master_dsac
  
  yt_master_ds[
    which(yt_master_ds$channel.name==yt_master_ds$channel_unique_ID),
    ] -> missing # video unavailable



  youtube_2023to2025 %>%
    mutate(the_day=as.Date(mdy(str_extract(
    EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
    mutate(year = year(lubridate::as_date(the_day))) %>%
    filter(year==2025)%>%
    select(channel.name,channel_unique_ID,video_ID) %>% distinct(video_ID,.keep_all = T)  -> vids_2025
  
  
  # see: https://www.youtube.com/watch?v=uDZiru1rPng
  # veruss: https://www.youtube.com/watch?v=UqB5wipYqLM = "set to private"
  
  
  
  
  
  yt_master_ds %>% 
    filter(region=="all regions"|is.na(region))%>%
    mutate(the_day=as.Date(mdy(str_extract(
      EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
    mutate(month=month(lubridate::as_date(the_day))) %>% 
    mutate(year = year(lubridate::as_date(the_day))) %>%
    filter(keyword=="transgender") %>%
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(channel.name, year) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>%
    ungroup() %>% select(year,channel.name,n) %>%
    tidyr::pivot_wider(names_from=year,values_from = n) -> kw_transgender
  
  
  
  
  
  ############
  
  
  
  
  yt_master_ds %>% 
    filter(region=="all regions"|is.na(region))%>%
    mutate(the_day=as.Date(mdy(str_extract(
      EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
    mutate(month=month(lubridate::as_date(the_day))) %>% 
    mutate(year = year(lubridate::as_date(the_day))) %>%
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(channel.name, year, keyword) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>%
    ungroup() %>% select(year,keyword,channel.name,n) %>%
    tidyr::pivot_wider(names_from=year, values_from = n) %>% head(5)
  
  
  yt_master_ds$year 
  
  yt_master_ds %>% 
    filter(region=="all regions"|is.na(region))%>%
    mutate(the_day=as.Date(mdy(str_extract(
      EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
    mutate(month=month(lubridate::as_date(the_day))) %>% 
    mutate(year = year(lubridate::as_date(the_day))) %>%
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(channel.name, year, keyword) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>%
    ungroup() %>% select(year,keyword,channel.name,n) %>%
    tidyr::pivot_wider(names_from=year, values_from = n) %>% head(10)
  
  
  
  yt_master_ds %>% 
    filter(region=="all regions"|is.na(region))%>%
    mutate(the_day=as.Date(mdy(str_extract(
      EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
    mutate(month=month(lubridate::as_date(the_day))) %>% 
    mutate(year = year(lubridate::as_date(the_day))) %>%
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(channel.name, year, keyword) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>%
    ungroup() %>% select(year,keyword,channel.name,n) %>%
    tidyr::pivot_wider(names_from=c(year,keyword), values_from = n) %>% head(5) |> View()
  
  
  yt_master_ds %>% 
    filter(region=="all regions"|is.na(region))%>%
    mutate(the_day=as.Date(mdy(str_extract(
      EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
    mutate(month=month(lubridate::as_date(the_day))) %>% 
    mutate(year = year(lubridate::as_date(the_day))) %>%
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(channel.name,customURL, subscriber_count, year, keyword) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>%
    ungroup() %>% 
    tidyr::pivot_wider(names_from=year, values_from = n) %>% head(10)
  
  
  names(yt_master_ds)
  
  yt_master_ds %>% select(channel.name,channel_unique_ID,video_ID) %>% distinct(video_ID,.keep_all = T)  -> vids_2025
  
  vids_2023[-which(vids_2023$channel.name==vids_2025$video_ID),]->vids_2025_cleaned
  
  youtube_2023to2025 %>% 
    filter(region=="all regions"|is.na(region))%>%
    mutate(the_day=as.Date(mdy(str_extract(
      EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
    mutate(year = year(lubridate::as_date(the_day))) %>%
    filter(year=="2024")%>%
    select(channel.name,channel_unique_ID,video_ID) %>% distinct(video_ID,.keep_all = T)  -> vids_2024
  
  vids_2023[-which(vids_2024$channel.name==vids_2025$video_ID),]->vids_2023_cleaned
  vids_2024[-which(vids_2024$channel.name==vids_2024$video_ID),]->vids_2024_cleaned
  vids_2025[-which(vids_2024$channel.name==vids_2025$video_ID),]->vids_2025_cleaned
  
  vids_2023_cleaned %>% distinct(video_ID) -> vids_2023
  vids_2024_cleaned %>% distinct(video_ID) -> vids_2024
  vids_2025_cleaned %>% distinct(video_ID) -> vids_2025
  
  
  like_scrape -> like_scrape2025
  like_scrape -> like_scrape2023  
  
  like_scrape <-NULL
  i<-1 ; for (i in 1:length(vids_2024_cleaned$video_ID)){
    tuber::get_stats(vids_2024_cleaned$video_ID[i])->> x
    plyr::rbind.fill(like_scrape,as.data.frame(x))->> like_scrape
    sample(5)[1]->sleepy
    print(paste0("channel: ", as.character(vids_2024_cleaned$video_ID[as.numeric(1)]), 
                 " - video: ", vids_2024_cleaned$video_ID[as.numeric(i)]," ~ wait ",sleepy," seconds.."))
    Sys.sleep(sleepy)
    print(as.data.frame(x))
  } 
  
  like_scrape$id->like_scrape$video_ID
  
  left_join(like_scrape,yt_master_ds,by = "video_ID") %>% filter(video_ID %in% vids_2025_cleaned$video_ID) -> yt2025_likes
  
  yt2025_likes  %>% group_by(keyword)%>%
    mutate(m=mean(as.numeric(likeCount),na.rm=TRUE))%>% select(keyword,m) %>% distinct(keyword,.keep_all = T) %>% as_tibble() %>% arrange(desc(m))
  
  left_join(like_scrape,yt_master_ds,by = "video_ID") %>% filter(video_ID %in% vids_2023_cleaned$video_ID) -> yt_2023_likes
  
  cbind(
    
    (yt_2023_likes  %>% group_by(keyword)%>%
       mutate(m=mean(as.numeric(likeCount),na.rm=TRUE))%>% select(keyword,m) %>% distinct(keyword,.keep_all = T) %>% as_tibble() %>% arrange(desc(m))),
    (yt2025_likes  %>% group_by(keyword)%>%
       mutate(m=mean(as.numeric(likeCount),na.rm=TRUE))%>% select(keyword,m) %>% distinct(keyword,.keep_all = T) %>% as_tibble() %>% arrange(desc(m)))) 
  