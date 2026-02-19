# subsets

# jessdkant.bsky.social
# tech.lgbt/@jessdkant

# fox news

# pull subset

  mega_ds %>% filter(pullURL=="foxnews.com") %>% 
     distinct(EntryURL, .keep_all = T) %>% 
    filter(year != "2026") %>%
    select(the_day,dayweek,month, quarter,year,EntryTitle,EntryURL,keyword,pullURL) %>%
    mutate(type=case_when
                     (
                       str_detect(EntryURL,"/video/") == TRUE ~ "video",
                       str_detect(EntryURL,"/video/") == FALSE ~ "article"
                     )) %>% 
    mutate(cat=gsub("/","",str_extract(EntryURL,pattern="/[a-z-]+/"))) -> just_fox

# create subset CSV

    write.csv(just_fox, "~/gayagenda/datasets/subsets/just_fox.csv",row.names = FALSE)

# top outlets only - top 100

  top_outlets(mega_ds,100)

  mega_ds %>%
    filter(pullURL %in% topouts) %>%
    distinct(EntryURL, .keep_all = T) %>% 
    select(the_day,dayweek,month, quarter,year,EntryTitle,EntryURL,keyword,pullURL) -> top_100
  
    top_100 %>% write.csv("~/gayagenda/datasets/subsets/top_20.csv",row.names = FALSE)
  
# pull all data with various keywords

    search_query_1 = "keywords|otherkeywords"
    mega_ds %>%
    mutate(mention= 
      case_when(
        str_detect(EntryContent, pattern=search_query )==TRUE ~ str_extract(EntryContent,search_query)
      )) -> ds_query
  
# tabulate above data
    
  mega_ds %>% filter(region=="all regions") %>% distinct(EntryURL, .keep_all = T) %>% select(keyword) -> ds_only 
  table(ds_only) ; round(prop.table(table(ds_only)),2)
  mega_ds %>% filter(region=="all regions" | is.na(region)) %>% select(keyword) -> ds_only 
  table(ds_only) ; round(prop.table(table(ds_only)),2)

# pressreader subset

pressr %>% select(country,keyword) %>% table()

mega_ds %>% filter(pullURL=="pressreader.com") %>%
    mutate(url_stub=str_remove(EntryURL,'https?:\\/\\/www.pressreader.com/')) %>%
    mutate(country=str_extract(url_stub, '[a-z]+')) %>%
    mutate(publication=gsub(pattern = "-", replacement = " ", 
                            str_extract(url_stub,'[a-zA-Z0-9-]+(?=/[0-9]{8})'))) %>%
    mutate(pubdate=str_extract(EntryURL,'(?=[0-9]{8})[0-9]+')) %>%
    mutate(articleID=str_extract(EntryURL,'[0-9]+(?<=[0-9]{15})')) -> pressr
  
pressr |> select(the_day, country, EntryURL, pubdate, articleID, publication, region) %>% View()

pressr %>% select(year, publication, country, region) %>% 
  group_by(year, publication, country, region) %>% summarize(num=n()) %>% 
  arrange(desc(num), desc(region)) 

pressr %>% filter(region=="all regions") %>%
  select(year, publication, country, keyword) %>% 
  group_by(year, publication, country, keyword) %>% summarize(num=n()) %>% 
  arrange(desc(num), desc(keyword)) -> pressr_allregions

# mutate(new=as.vector(unlist(str_split(url_stub,"/")))[[1]]) %>%
# pressr$EntryURL[(grepl("uk", pressr$EntryURL)==TRUE)]

pressr_allregions %>% 
  ggplot()+
  geom_boxplot(aes(x=num,fill=keyword))

# get all unique IDs independent of region

 c("region", 
   "keyword", 
   "dayweek", 
   "the_day",
   "month", 
   "quarter") -> cutvars

pressr %>% 
  distinct(EntryURL,.keep_all = TRUE) %>%
  select(-contains("Entry")) %>% select(-cutvars) -> pressr_unique

pressr_unique %>% select(publication,country) %>% table()
pressr_unique %>% group_by(country) %>% summarize(n=n()) %>% arrange(desc(n))

pressr %>% 
  select(-contains("Entry")) %>% select(-cutvars) %>% 
  arrange(country, publication) %>% 
  write.csv(file = "~/gayagenda/datasets/subsets/pressreader.csv")

# substack

mega_ds %>%
  filter(str_detect(EntryURL,"substack.com")) %>% 
  mutate(substack=pullURL)%>%
  mutate(subname= case_when(
           str_detect(pullURL,"[a-z]+.substack.com")==FALSE  ~ "substack.com",
           str_detect(pullURL,"[a-z]+.substack.com")==TRUE ~ str_extract(pullURL,"[a-z_]+(?=.substack.com)"))
         ) %>%  
  mutate(post_ID = case_when(
    str_detect(pullURL,"[a-z]+.substack.com") == FALSE  ~ str_remove(str_extract(EntryURL,"post\\/[a-z0-9A-Z-]+"),"post/p-"),
    str_detect(pullURL,"[a-z]+.substack.com") == TRUE ~ gsub("-"," ",str_remove(str_extract(EntryURL,"\\/p\\/[a-z0-9A-Z-]+"),"/p/")))
        ) %>% select(the_day,month,year,keyword,EntryTitle,pullURL,subname, post_ID, EntryURL) -> substack

write.csv(substack,
          paste0(paste("substack_",month(Sys.Date()),year(Sys.Date()),sep="_"),".csv"),
          row.names = F)
