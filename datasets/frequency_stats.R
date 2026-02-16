# frequency stats
# jessdkant.bsky.social


# ds %>% filter(is.na(pullURL)==FALSE) %>%
#  distinct(EntryURL, .keep_all = TRUE) %>%
#  group_by(pullURL) %>%
#   summarize(n=n()) %>% 
#   arrange(by_group=desc(n)) %>%
#   head(n=20) %>% select(pullURL) -> top_20

# top 10 / top 20


  mega_ds %>% filter(is.na(pullURL)==FALSE) %>%
    filter(!(pullURL %in% portals)) %>% filter(!(pullURL %in% socials)) %>% filter(!str_detect(pullURL,".co.uk|uk")) %>% 
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>% head(n=10) %>% select(pullURL) -> top_10_us
  as.vector(top_10_us$pullURL) -> top10us
  # as.list(top_10_us$pullURL) -> top_ten_us
  
  mega_ds %>% filter(is.na(pullURL)==FALSE) %>%
    filter(!(pullURL %in% portals)) %>%filter(!(pullURL %in% socials)) %>%filter(str_detect(pullURL,".co.uk")) %>%
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>% head(n=10) %>% select(pullURL) -> top_10_uk
  as.vector(top_10_uk$pullURL) -> top10uk
  # as.list(top_10_uk$pullURL) -> top_ten_uk

  mega_ds %>% filter(keyword %in% main_kws) %>% select(region, keyword, year) %>% table()
  
  mega_ds %>% 
    filter(keyword %in% main_kws) %>%
    filter(region %in% c("all regions", "not set")) %>%
    filter(pullURL %in% top10us) %>% select(pullURL, year, region) %>% table() 
  
  mega_ds %>% filter(is.na(pullURL)==FALSE) %>%
    filter(keyword %in% main_kws) %>%
    filter(!(pullURL %in% portals)) %>% filter(!(pullURL %in% socials)) %>%  
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>% head(n=10) %>% select(pullURL) -> top_10
   as.vector(top_10$pullURL) -> topten

   
   top_outlets<-function(dataset_name, number){
     dataset_name %>% filter(is.na(pullURL)==FALSE) %>%
         filter(keyword %in% main_kws) %>%
         filter(!(pullURL %in% portals)) %>% filter(!(pullURL %in% socials)) %>%  
         distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
         arrange(by_group=desc(n)) %>% head(n=number) %>% select(pullURL) -> tophits
       as.vector(tophits$pullURL) ->> topouts
       topouts
   }
   
   top_outlets(mega_ds,100)
      
   
   mega_ds %>% 
     filter(region %in% c("all regions", "not set")) %>%
     filter(pullURL %in% topouts) %>% group_by(pullURL, year, region, keyword) %>% 
     mutate(n=n()) %>% arrange(desc(n)) %>% View()
   
   mega_ds[mega_ds$pullURL %in% portals,] -> portal_data
   portal_data %>% select(month, quarter, year, keyword, region, pullURL) %>%
     mutate(binsize=n()) %>% select(year, pullURL, binsize) %>% table()

   mega_ds %>% select(dayweek, year) %>% 
     ggplot()+
     geom_bar(aes(x=dayweek,fill=as.factor(year)),position=position_dodge())
      
   
   top_outlets(mega_ds, 5)

#  mega_ds %>% reorder(dayweek, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday"))
  
  