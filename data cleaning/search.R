# pull all data with various keywords

search_query = "sex|gender"
mega_ds %>%
  mutate(mention= 
           case_when(
             str_detect(EntryContent, pattern=search_query )==TRUE ~ str_extract(EntryContent,search_query)
           )) %>% filter(!is.na(mention)) %>% 
  select(year, month, mention, keyword, EntryTitle, pullURL, EntryURL) %>%
  View()
  
# tabulate above data

mega_ds %>% filter(region=="all regions") %>% distinct(EntryURL, .keep_all = T) %>% select(keyword) -> ds_only 
table(ds_only) ; round(prop.table(table(ds_only)),2)
mega_ds %>% filter(region=="all regions" | is.na(region)) %>% select(keyword) -> ds_only 
table(ds_only) ; round(prop.table(table(ds_only)),2)


top_outlets<-function(dataset_name, number){
  dataset_name %>% filter(is.na(pullURL)==FALSE) %>%
    filter(keyword %in% main_kws) %>%
    filter(!(pullURL %in% portals)) %>% filter(!(pullURL %in% socials)) %>%  
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>% head(n=number) %>% select(pullURL) -> tophits
  as.vector(tophits$pullURL) ->> topouts
  topouts
}

top_outlets(mega_ds,20)

# pull all data with various keywords

search_dataset <-function(dataset,search_terms,filter_terms){
  search_query_1 = search_terms 
  search_query_2 = filter_terms
  mega_ds %>%
    mutate(mention_1= 
             case_when(
               str_detect(EntryContent, pattern=search_query_1 )==TRUE ~ str_extract(EntryContent,search_query_1)
             )) %>%
    mutate(mention_2= 
             case_when(
               str_detect(EntryContent, pattern=search_query_2 )==TRUE ~ str_extract(EntryContent,search_query_2)
             ))->> ds_query
          }


  search_dataset(substack, 
                 search_terms = "sex|gender", 
                 filter_terms = "rejecting|denying")
  
  ds_query  %>%
    filter(keyword %in% main_kws) %>%
    filter(!is.na(mention_1)) %>%
    filter(!is.na(mention_2)) %>%
    group_by(keyword, pullURL, EntryTitle, year, mention_1,mention_2)%>% summarize(n=n())%>% 
    ungroup()%>% 
    arrange(desc(year), desc(n)) |> View()

    #ggplot()+
    #geom_bar(aes(x=year))+
    #facet_grid(mention_1~mention_2)
  
             