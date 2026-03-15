# subsets

# jessdkant.bsky.social
# tech.lgbt/@jessdkant

read.csv("~/nightly_2026_03_05.csv") -> mega_ds

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
  
pressr |> select(the_day, country, EntryURL, EntryContent, pubdate, articleID, publication, region) %>% View()

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

  # subscriber pull v2

  unique(possible_subs$pullURL) -> site_list
  site_list <- paste0("https://www.",site_list)
  sub_list  <- as.vector(NULL)
  meta_list <- as.vector(NULL)
  i<-0
  for (i in 1:length(site_list)){
    tryCatch({
      append(sub_list, str_detect(rawToChar(curl::curl_fetch_memory(site_list[i])$headers), "substackcdn"))->> sub_list
      if(str_detect(rawToChar(curl::curl_fetch_memory(site_list[i])$headers), "substackcdn")){
        read_html(site_list[i]) -> meta_data
        append(meta_list ,str_remove(
          str_extract(
            toString(html_elements(meta_data, ".publication-meta")),pattern = '(?<=meta\\">)[a-zA-Z0-9 -_]+'), 
          pattern="</div>"))->>meta_list}
      else{append(meta_list, "not scraped")->>meta_list}
    },  error= function(w){
      append(sub_list,"error")->>sub_list
      append(meta_list, "error")->>meta_list
    }) 
  } 

  as.data.frame(cbind(
    site_list,
    sub_list,
    meta_list))-> substackdetector_results

    colnames(substackdetector_results)[1]<-"test_url"
    colnames(substackdetector_results)[2]<-"result"
    colnames(substackdetector_results)[3]<-"about"
  
  substackdetector_results$test_url<-str_remove(
    substackdetector_results$test_url,"https://www.")
  
  paste0(
    "https://substack.com/@",
    str_remove(substackdetector_results$test_url,
               "\\.ca|\\.co\\.uk|\\.com|\\.media|\\.news|\\.tv|\\.org|\\.net|\\.info|\\.blog")
    )->substackdetector_results$about_page
  
  as.vector(str_remove(substackdetector_results$test_url[
    which(substackdetector_results$result==TRUE)],"https://www.")) -> found_stacks

  # second pass at substack metadata
  
      unique(substackdetector_results$about_page) -> profile_list
      profile_data<- as.vector(NULL)
      i<-0
      for (i in 1:length(profile_list)){
        tryCatch({
          append(profile_data, str_extract(
              toString(html_elements(read_html(substackdetector_results$about_page[i]), "a.pencraft.pc-reset.link-LIBpto")
                       ),pattern = '[a-zA-Z0-9.,]+K\\+ subscribers'))->> profile_data
        },  error= function(w){
          append(profile_data,"error")->>profile_data
        })} 
        
      cbind(substackdetector_results,profile_data)->substackdetector_results

      substackdetector_results %>% filter(result==TRUE|
                                            str_detect(about,"subscriber|months")|
                                            str_detect(profile_data,"subscriber")) -> substack_results_clean
      
      substack_results_clean %>% as.data.frame() -> substack_results_clean
      substack_results_clean$about[is.na(substack_results_clean$about)]<-""
      substack_results_clean$profile_data[is.na(substack_results_clean$profile_data)]<-""
      
      str_extract(substack_results_clean$about[
        str_detect(substack_results_clean$about,"[0-9]+")],"[0-9]+")
      
      substack_results_clean %>%
        mutate(subscribers=
                 as.numeric(case_when(
                   str_detect(about,"[0-9]+") ~ paste0(str_extract(about,"[0-9]+"),"000"),
                   str_detect(profile_data,"[0-9]+") ~ paste0(str_extract(profile_data,"[0-9]+"),"000"),
                   !str_detect(about,"[0-9]+") & !str_detect(profile_data,"[0-9]+") ~ "" ))) -> 
        substack_results_clean
        
      write.csv(substack_results_clean,"~/gayagenda/datasets/subsets/other_subs.csv")
      
      read.csv("~/gayagenda/datasets/subsets/sub_roster.csv") -> sub_list
      
      mega_ds %>%
        filter(pullURL %in% c(sub_list$test_url)) -> substack
      
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
      select(the_day,month,year,keyword,EntryTitle,pullURL,subname, post_ID, EntryURL,post_count, region) -> substack

   substack$subname<-gsub(".com","",substack$subname)
   
    # write month/year stamped CSV
  
  { getwd() -> curr_dir
    setwd(dir = "~/gayagenda/datasets/subsets/")
    write.csv(substack,
                paste0(paste("substack",month(Sys.Date()),year(Sys.Date()),sep="_"),".csv"), 
              row.names = F); setwd(curr_dir)}
  
    substackdetector_results[which(substackdetector_results$result=="error"),] |> table()
    
    unique(sub$test_url)-> substacks
    
    # youtube ------------------------------------------------#
    # 

    # starting with smaller subset for API quota
    
    mega_ds %>% filter(pullURL=="youtube.com") %>% 
      mutate(video_ID=str_extract(EntryURL, '(?<=watch\\?v=)[a-zA-Z0-9-_]+')) %>%
      filter(year==2025)-> youtube 
  
    vids <- mega_ds %>% filter(pullURL=="youtube.com") %>% 
                        filter(year==2025) %>%
                        mutate(video_ID=str_extract(
                          EntryURL, '(?<=watch\\?v=)[a-zA-Z0-9-_]+')
                          ) %>% 
                        select(EntryURL, pullURL, video_ID) %>% 
                        distinct(video_ID) 
    as.vector(vids$video_ID)->vids
    
    # api: old version
    
      i<-0 ; for (i in 1:length(vids)){
       get_video_details(video_id = vids[as.numeric(i)])$items ->> x
        append(vid_list, x[[1]]$snippet$channelTitle) ->> vid_list
        sample(3)[1]<-sleepy ; Sys.sleep(sleepy) 
        print(vids(as.numeric(i)),"wait ",sleepy)
        } 

    # api: new version #
    
    vid_list <- NULL
    channel_list <-NULL
    youtube_scrape <-NULL
        i<-0 ; for (i in 1:length(vids)){
             get_video_details(video_id = vids[as.numeric(i)])$items ->> x
             append(vid_list, x$channelTitle) ->> vid_list
             rbind(youtube_scrape,c(vids[i],x[[1]]$snippet$publishedAt,x[[1]]$snippet$channelTitle,x[[1]]$snippet$channelId))->> youtube_scrape
             sample(1)[1]->sleepy ; Sys.sleep(sleepy) 
             print(paste0(x[[1]]$snippet$channelTitle,"   ...   # ",as.numeric(i)," ~ waiting ",toString(sleepy)," seconds..."))
        } 
        
        # rvest version 1 #

    vid_list  <- as.vector(NULL)
    
    #i<-0 ; for (i in 1:length(vids)){
    i<-0 ; for (i in 20:25){
      targ_url <- paste0("https://youtube.com/watch?v=",vids[i])
      read_html_live(targ_url) %>% html_elements("#text-container.ytd-channel-name") ->> x
      if(toString(x)==""){
        read_html_live(targ_url) %>% html_elements("#yt-simple-endpoint.style-scope.yt-formatted-string") ->> x
        if(toString(x)==""){
          append(vid_list, "error")->> vid_list
        }}append(vid_list, html_text(x))->> vid_list} 

    # rvest version 2 #
   
    match(vids,channel_list)
    
    i<-0 ; for (i in 1:100){
      targ_url <- paste0("https://youtube.com/watch?v=",vids[i])
      read_html_live(targ_url) %>% html_elements("#text-container.ytd-channel-name") %>% html_text() ->> x
      print(paste0(targ_url, x))
      append(vid_list, x)->> vid_list
      rbind(channel_list,c(targ_url,x)) ->> channel_list
      sample(30)[1]->sleepy ; Sys.sleep(sleepy) 
      print(paste0("wait ",sleepy," seconds...\n"))
    }
    
    # BBC ----------------------------------------------------#
    # 
    
    mega_ds %>% filter(keyword %in% main_kws) %>% 
      filter(pullURL=="bbc.co.uk") %>% 
      select(keyword , year) %>% count(keyword, year)%>% 
    arrange(desc(year))

        mega_ds %>% filter(keyword=="transgender") %>% 
      filter(pullURL=="bbc.co.uk")  %>% select(EntryTitle , year) %>% 
      arrange(desc(year)) -> beeb
    
    search_query = "key|word"
    mega_ds %>%
      #filter(keyword=="transgender") %>%
      filter(pullURL=="bbc.co.uk") %>%
      mutate(mention= 
               case_when(
                 str_detect(tolower(EntryContent), pattern=search_query )==TRUE ~ 
                   str_extract(tolower(EntryContent),search_query),
                 str_detect(tolower(EntryTitle), pattern=search_query )==TRUE ~ 
                   str_extract(tolower(EntryTitle),search_query)
               )) %>%  
      arrange(desc(year), desc(month)) %>%
      distinct(EntryURL, .keep_all = T)%>%
      rename(google_news_keyword=keyword) %>%
      select(month, year, mention, pullURL, keyword, EntryTitle,google_news_keyword) %>% View()
    
    
    # Atlantic -----------------------------------------------#
    # 
    
    atlantic %>% arrange(desc(the_day)) %>% 
      select(-the_day,-EntryContent) %>% 
      select(EntryPublished,EntryTitle,region,keyword,dayweek,month,year,RSS) %>% 
      group_by(EntryTitle) %>% summarize(count=n()) %>%
      arrange(desc(count)) %>%
      write.csv("subsets/atlantic.csv")
    
    atlantic %>% 
      select(EntryPublished,EntryTitle,region,keyword,dayweek,month,year,RSS,the_day,EntryURL) %>% 
      group_by(EntryTitle) %>% mutate(count=n()) %>%
      ungroup()%>%
      arrange(desc(count)) %>%
      write.csv("subsets/atlantic_morevars.csv")
    
    # finding artifacts

    mega_ds %>% 
      filter(str_detect(EntryURL,"\\?")==TRUE & str_detect(EntryURL,"\\=")==TRUE) %>% 
      mutate(subscriber_ID=str_extract(EntryURL, "(?<=\\?r\\=)[a-z0-9A-Z]+|(?<=\\&r\\=)[a-z0-9A-Z]+")) %>% 
      mutate(argstring=str_extract(tolower(EntryURL), "(?<=connector).*|(?<=fckeditor).*|z0x.top.*")) %>% 
      filter(!is.na(argstring)) %>% 
      select(the_day,dayweek,year,keyword,argstring,EntryURL) %>% View()
    

    # skynews  -----------------------------------------------#
      
      mega_ds %>% 
        #filter(year!=2026) %>%
        filter(str_detect(tolower(EntryURL), "skynews")) %>% 
        select(pullURL,year) %>% table() -> sky_table 
        
        sky_table %>% prop.table() %>% round(2)
        sky_table %>% prop.table() %>% round(2)
    
    
    # social -------------------------------------------------#
        
        
        social_results %>% filter(pullURL=="x.com") %>% 
          filter(str_detect(EntryURL,"grok")) %>% 
          select(EntryTitle,EntryURL) %>% 
          mutate(preview=head(EntryTitle))%>%
          distinct(EntryURL,.keep_all = T) |> as_tibble()
        
        
        c( "msn.com","yahoo.com","aol.com", "pressreader.com") -> portals
        c( "youtube.com",
           "x.com",
           "instagram.com",
           "twitter.com",
           "reddit.com",
           "substack",
           "facebook.com") ->socials 
        c("transgender", "biological sex", "gender identity")-> main_kws
        c("nature.com","sciencedirect.com") -> exc_sites
        
        top_outlets<-function(dataset_name, number){
          dataset_name %>% filter(is.na(pullURL)==FALSE) %>%
            filter(keyword %in% main_kws) %>%
            filter(!(pullURL %in% portals)) %>% filter(!(pullURL %in% socials)) %>% filter(!pullURL %in% exc_sites) %>%
            distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
            arrange(by_group=desc(n)) %>% head(n=number) %>% select(pullURL) -> tophits
          as.vector(tophits$pullURL) ->> topouts} 

    mega_ds %>% 
      filter(pullURL %in% socials) -> social_results
    
      social_results %>%   
        group_by(pullURL,keyword)%>%
        summarise(n=n()) 

      social_results %>%
        filter(region=="all regions"|is.na(region)) %>%
        filter(year==2023|year==2024|year==2025)%>%
        ggplot()+
        ylab("")+xlab("")+
        labs(title = "Social media in Google News results\n2023-2025")+
        geom_bar(aes(x=keyword,fill=pullURL))+
        facet_grid(.~year)+
        labs(fill="platform",subtitle = "all regions")+
        theme(plot.background=element_rect("white", colour = "white"),
              panel.grid = element_line("white"),  
              panel.background = element_rect("white"),
              legend.background = element_rect("white"),
              legend.box.background = element_rect("white",colour = "white"),
              legend.frame = element_rect("white"),
              legend.key = element_blank(),
              text = element_text(colour = "black"),
              legend.position = "right",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              strip.background =element_rect("white"))+
        scale_fill_paletteer_d("yarrr::info") -> top
      
      social_results %>%
        filter(year==2023|year==2024|year==2025)%>%
        filter(region=="all regions"|is.na(region)) %>%
        filter(pullURL!="youtube.com")%>%
        ggplot()+
        ylab("")+xlab("")+
        labs(title = "Excluding YouTube")+
        geom_bar(aes(x=keyword,fill=pullURL))+
        facet_grid(.~year)+
        labs(fill="platform",
             caption = "jessk.org/blog")+
        theme(plot.background=element_rect("white", colour = "white"),
              panel.grid = element_line("white"),  
              panel.background = element_rect("white"),
              legend.background = element_rect("white"),
              legend.box.background = element_rect("white",colour = "white"),
              legend.frame = element_rect("white"),
              legend.key = element_blank(),
              text = element_text(colour = "black"),
              legend.position = "right",
              axis.text.x = element_text(color="black", angle=90),
              strip.background =element_rect("white"))+
        scale_fill_paletteer_d("yarrr::info") -> bottom
      
      grid.arrange(top,bottom)
      