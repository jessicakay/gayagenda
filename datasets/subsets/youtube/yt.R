

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

yt_df

yt_df %>%
  filter(region=="all regions")%>%
  filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
  mutate(year=as.factor(year))%>%
  ggplot()+
  theme_dark()+
  geom_bar(aes(x=ym,fill=year),position = position_dodge(preserve = "single"))+
  theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
        panel.background = element_rect("black"),legend.background = element_rect("black"),
        legend.box.background = element_rect("black"),legend.key = element_rect("black"),
        text = element_text(colour = "white"),
        legend.position = "bottom")+
  scale_fill_paletteer_d("yarrr::info")+
  facet_grid(.~keyword) 

  core+geom_bar(aes(x=yq,fill=year),position = position_dodge(preserve = "single"))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "bottom")+
    scale_fill_paletteer_d("yarrr::info")+
    facet_grid(.~keyword)

  grid.arrange(plot_a,plot_b,ncol=2)  
  
  
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
  