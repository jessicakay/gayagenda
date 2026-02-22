# subsets

# jessdkant.bsky.social
# tech.lgbt/@jessdkant

read.csv("~/gayagenda/datasets/nightly_2026_02_16.csv") -> mega_ds


# fox news  ----------------------------------------------#
#

  mega_ds %>% filter(pullURL=="foxnews.com") %>% 
    distinct(EntryURL, .keep_all = T) %>% 
    filter(year != "2026") %>%
    select(the_day,dayweek,month, quarter,year,EntryTitle,EntryURL,keyword,pullURL) %>%
    mutate(type=case_when(
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

# pressreader subset -------------------------------------#
# 
  
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

# substack -----------------------------------------------#
#

# find potential stacks that are not on substack sub/domains


mega_ds %>% filter(str_detect(EntryURL,"/p/[a-zA-Z0-9]+") & 
                     !str_detect(EntryURL,"substack.com")) %>% select(pullURL,EntryURL) -> possible_subs

  unique(possible_subs$pullURL) -> site_list
  site_list <- paste0("https://www.",site_list)
  sub_list  <- as.vector(NULL)
  meta_list <- as.vector(NULL)
  i<-0
  for (i in 1:length(site_list)){
    tryCatch({
      append(sub_list, str_detect(rawToChar(curl::curl_fetch_memory(site_list[i])$headers), "substackcdn"))->> sub_list
      if(str_detect(rawToChar(curl::curl_fetch_memory(site_list[i])$headers), "substackcdn")==TRUE){
        read_html(site_list[i]) -> meta_data
        append(meta_list ,str_remove(
          str_extract(
            toString(html_elements(meta_data, ".publication-meta")),pattern = '(?<=meta\\">)[a-zA-Z0-9 -_]+'), 
          pattern="</div>"))->>meta_list}
    },  error= function(w){
      append(sub_list,"error")->>sub_list
      append(meta_list, NULL)->>meta_list
    }) 
  } 

#else{append(meta_list, "not substack")->>meta_list}
  # backup code

  {
  unique(possible_subs$pullURL) -> site_list
  site_list <- paste0("https://www.",site_list)
  sub_list  <- as.vector(NULL)
  i<-0
  for (i in 1:length(site_list)){
    tryCatch({
      append(sub_list, str_detect(rawToChar(curl::curl_fetch_memory(site_list[i])$headers), "substackcdn"))->> sub_list
    },  error= function(w){
      append(sub_list,"error")->>sub_list
    }) 
  } 
  }

cbind(site_list,sub_list,meta_list)

as.data.frame(cbind(as.vector(site_list),as.vector(sub_list),as.vector(meta_list))) -> substackdetector_results
colnames(substackdetector_results)[1]<-"test_url"
colnames(substackdetector_results)[2]<-"result"
colnames(substackdetector_results)[3]<-"about"

as.vector(str_remove(substackdetector_results$test_url[
  which(substackdetector_results$result==TRUE)],"https://www.")
  ) -> found_stacks

# version 2

mega_ds %>%
  filter(str_detect(EntryURL,"substack.com") | pullURL %in% found_stacks) %>% 
  mutate(substack=pullURL)%>%
  mutate(subname= case_when(
    str_detect(pullURL,"[a-z]+.substack.com")==FALSE & !(pullURL %in% found_stacks)  ~ "substack.com",
    str_detect(pullURL,"[a-z]+.substack.com")==TRUE ~ str_extract(pullURL,"[a-z_]+(?=.substack.com)"),
    pullURL %in% found_stacks ~ pullURL)) %>%  
  mutate(post_ID = case_when(
    str_detect(EntryURL,"\\/p\\/[a-z0-9A-Z-]+")==TRUE ~ gsub(
      "-"," ",str_remove(str_extract(EntryURL,"\\/p\\/[a-z0-9A-Z-]+"),"/p/")),
    str_detect(EntryURL,"post\\/[a-z0-9A-Z-]+")==TRUE ~ str_remove(
      str_extract(EntryURL,"post\\/[a-z0-9A-Z-]+"),"post/p-"),
    str_detect(EntryURL,"\\/p\\/[a-z0-9A-Z-]+")==FALSE & 
      str_detect(EntryURL,"post\\/[a-z0-9A-Z-]+")==FALSE ~ str_extract(EntryURL,".com/[a-z-]+")
  ))%>%
  group_by(pullURL,keyword,year)%>%
  mutate(post_count=n())%>%
  arrange(desc(post_count))%>%
  ungroup()%>%
  select(the_day,month,year,keyword,EntryTitle,pullURL,subname, post_ID, EntryURL,post_count) -> substack

   substack$subname<-gsub(".com","",substack$subname)
         
    # write month/year stamped CSV
  
  { getwd() -> curr_dir
    setwd(dir = "~/gayagenda/datasets/subsets/")
    write.csv(substack,
                paste0(paste("substack",month(Sys.Date()),year(Sys.Date()),sep="_"),".csv"), 
              row.names = F); setwd(curr_dir)}
  
    substackdetector_results[which(substackdetector_results$result=="error"),] |> table()
    
    # youtube ------------------------------------------------#
    # 
    
    vids <- mega_ds %>% filter(pullURL=="youtube.com") %>% 
                        mutate(video_ID=str_extract(
                          EntryURL, '(?<=watch\\?v=)[a-zA-Z0-9-_]+')
                          ) %>% 
                        select(EntryURL, pullURL, video_ID) %>% 
                        distinct(video_ID) 
    as.vector(vids$video_ID)->vids
    vid_list  <- as.vector(NULL)
    i<-0 ; for (i in 1:length(vids)){
     get_video_details(video_id = vids[as.numeric(i)])$items ->> x
      append(vid_list, x[[1]]$snippet$channelTitle) ->> vid_list
      sample(3)[1]<-sleepy ; Sys.sleep(sleepy) 
      print(vids(as.numeric(i)),"wait ",sleepy)} 
    read_html(yt$EntryURL[1]) %>% html_elements("#text-container.ytd-channel-name")  -> x
    str_extract(x,"(?<=title=\")")
    
    # BBC ----------------------------------------------------#
    # 
    
    mega_ds %>% filter(keyword %in% main_kws) %>% 
      filter(pullURL=="bbc.co.uk") %>% 
      select(keyword , year) %>% count(keyword, year)%>% 
    arrange(desc(year))

        mega_ds %>% filter(keyword=="transgender") %>% 
      filter(pullURL=="bbc.co.uk")  %>% select(EntryTitle , year) %>% 
      arrange(desc(year)) -> beeb
    
    search_query = "transgender child|trans child|trans kid|gender diverse child|children who identify"
    mega_ds %>%
      #filter(keyword=="transgender") %>%
      filter(pullURL=="bbc.co.uk") %>%
      mutate(mention= 
               case_when(
                 str_detect(tolower(EntryContent), pattern=search_query )==TRUE ~ str_extract(tolower(EntryContent),search_query),
                 str_detect(tolower(EntryTitle), pattern=search_query )==TRUE ~ str_extract(tolower(EntryTitle),search_query)
               )) %>%  
      arrange(desc(year), desc(month)) %>%
      distinct(EntryURL, .keep_all = T)%>%
      rename(google_news_keyword=keyword) %>%
      select(month, year, mention, pullURL, keyword, EntryTitle,google_news_keyword) %>% View()
    
    