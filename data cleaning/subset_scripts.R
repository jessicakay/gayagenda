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
    
  mega_ds %>% filter(pullURL=="foxnews.com") %>% 
    distinct(EntryURL, .keep_all = T) %>% 
    filter(year == "2026") %>%
    select(the_day,dayweek,month, quarter,year,EntryTitle,EntryURL,keyword,pullURL) %>%
    mutate(type=case_when
           (
             str_detect(EntryURL,"/video/") == TRUE ~ "video",
             str_detect(EntryURL,"/video/") == FALSE ~ "article"
           )) %>% 
    mutate(cat=gsub("/","",str_extract(EntryURL,pattern="/[a-z-]+/"))) %>%
    select(month,cat) %>% table()
           
  
 
search_query_1 = "Dr. Upton|Beth Upton|Dr. Beth Upton|transgender medic|trans* medic"
search_query_2 = "trans|medic|"
mega_ds %>%
mutate(mention= 
  case_when(
    str_detect(EntryContent, pattern=search_query )==TRUE ~ str_extract(EntryContent,search_query)
  )) -> ds_query



mega_ds %>% filter(region=="all regions") %>% distinct(EntryURL, .keep_all = T) %>% select(keyword) %>% table()
mega_ds %>% filter(region=="all regions") %>% distinct(EntryURL, .keep_all = T) %>% select(keyword) -> ds_only ; table(ds_only) ; round(prop.table(table(ds_only)),2)
mega_ds %>% filter(region=="all regions" | is.na(region)) %>% select(keyword) -> ds_only ; table(ds_only) ; round(prop.table(table(ds_only)),2)


# pressreader subset

mega_ds %>% filter(pullURL=="pressreader.com") %>%
    mutate(url_stub=str_remove(EntryURL,'https?:\\/\\/www.pressreader.com/')) %>%
    mutate(country=str_extract(url_stub, '[a-z]+')) %>%
    mutate(publication=str_extract(url_stub,'[a-zA-Z0-9-]+(?=/[0-9]{8})')) %>%
    mutate(date=str_extract(EntryURL,'(?=[0-9]{8})[0-9]+')) %>%
    mutate(articleID=str_extract(EntryURL,'[0-9]+(?<=[0-9]{15})')) %>%
    select(the_day,date,country,articleID,publication) -> pressr
  

# mutate(new=as.vector(unlist(str_split(url_stub,"/")))[[1]]) %>%
  
xh<-pressr$EntryURL[1]

  select(new_string) %>% table()

# pressr$EntryURL[(grepl("uk", pressr$EntryURL)==TRUE)]

     