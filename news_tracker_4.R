
# contact jkant@bu.edu for use 
# github.com/jessicakay/gayagenda

 install.packages("googlesheets4")
  
  { library(ggplot2)
    library(dplyr)
    library(stringr)
    library(lubridate)
    library(gridExtra)
    library(googlesheets4) } 
 
# clean OAuth tokens and authenticate

   detach(package:googlesheets4) ; googlesheets4::gs4_auth()
   headers <- c("EntryPublished","EntryTitle","EntryURL","EntryContent","FeedTitle","FeedURL","keyword","region")
   headers   <- as.data.frame(cbind(headers))
   # googlesheets4::sheet_append(tsheetall,as.vector(headers),sheet =1) # inserts headers into blank sheet, only run first time


refresh<-function(arg="all"){
  if(arg=="pull" | arg=="all"){
   googledrive::drive_find(pattern = "trans news tracker",verbose = TRUE ) -> data_sheets 
   dat <<- NULL
   for(i in 1:dim(data_sheets)[1]){
   if(i==1){googlesheets4::read_sheet(data_sheets$id[i]) ->> dat
    }else{rbind(dat, googlesheets4::read_sheet(data_sheets$id[i])) -> dat }
   dat -> ds ; dat ->> ds ; assign("ds",ds,envir = .GlobalEnv)}
  ds %>% group_by(EntryTitle) %>% as_tibble(as.data.frame()) ->> ds
  return(pullStats())
  run_nlp()}
  if(arg=="statsOnly"){pullStats()}
  if(arg=="ex"){
    googledrive::drive_find(pattern = "rhetorictracker",verbose = TRUE ) ->> ext_sheet 
    exdat <<- NULL
    for(i in 1:dim(ext_sheet)[1]){
      if(i==1){googlesheets4::read_sheet(ext_sheet$id[i]) ->> exdat
      }else{rbind(exdat, googlesheets4::read_sheet(ext_sheet$id[i])) -> exdat }
      exdat -> exds ; exdat ->> exds ; assign("exds",exds,envir = .GlobalEnv)}
    exds %>% group_by(EntryTitle) %>% as_tibble(as.data.frame()) ->> exds
    assign("exds",exds,envir = .GlobalEnv)
  }
  if(exists("ds")==TRUE){assign("ds",ds,envir = .GlobalEnv)}
  if(exists("exds")==TRUE){assign("exds",exds,envir = .GlobalEnv)}
}

pullStats <- function(){
   min_arts <- as.numeric(summarize(group_by(ds %>% filter(!is.na(pullURL)), pullURL),ct=n())$ct %>% quantile(c(.98)))
  max_arts <- as.numeric(summarize(group_by(ds %>% filter(!is.na(pullURL)), pullURL),ct=n())$ct %>% max)
  month(head(sort(ds$the_day))[1]) -> start_month
  day(head(sort(ds$the_day))[1]) -> start_day
  gsub(pattern="/",replace="",ds$pullURL)->ds$pullURL
  select(ds, region, the_day, pullURL) %>% 
    filter(!is.na(pullURL) & pullURL != "www.youtube.com/") %>%
    group_by(pullURL,.drop=FALSE) %>%
    summarize(count=n()) %>% filter(count>min_arts) -> top_outlets
  paste(paste(top_outlets$pullURL,sep = " | ",collapse="|"))->k
    cat("\nlast 5 entries: \n\n");print(tail(ds %>% arrange(EntryPublished),n=5))
  cat(paste("\n ->",dim(ds)[1]," rows, ",dim(ds)[2]," variables | avg. = ",
            round(mean(as.data.frame(as.data.frame(table(ds$the_day))[2])$Freq),2), " per day\n",
            "-> date range: ", min(ds$the_day), "-", max(ds$the_day)),"\n\n")
  assign("top_outlets",top_outlets,envir = .GlobalEnv)
  select(ds[which(grepl(k,ds$pullURL)),],region) %>% table() -> total
  return(rbind(ds[which(grepl(k,ds$pullURL)),] %>% select(pullURL,region) %>% table(),total))
  assign("ds",ds,envir = .GlobalEnv) 
  }

refresh()
pullStats()

  
# stratify by keyword

 
ds %>% 
  mutate(textcontent = paste(EntryContent,EntryURL, EntryTitle)) %>%
  mutate(theday=str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
  mutate(the_day=as.Date(mdy(theday))) ->> ds
  substring(str_extract(ds$EntryURL, pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z]+/"), first=9) -> ds$pullURL

ds %>%
    mutate(topic=case_when(
    str_detect(textcontent,"(?i)sport|(?i)athlet|(?i)competiti|(?i)swim|(?i)hockey|(?i)rugby|(?i)soccer|(?i)football") == TRUE ~ "sports",
    str_detect(textcontent,"(?i)restroom|(?i)bathroom|(?i)locker|(?i)naked|(?i)ymca")== TRUE ~ "bathrooms",
    str_detect(textcontent,"(?i)school|(?i)educat|(?i)universit|(?i)college|(?i)dormit|(?i)student") == TRUE ~ "education",
    str_detect(textcontent,"(?i)detrans|(?i)desist|(?i)de-trans|(?i)regret") == TRUE ~ "detransition",
    str_detect(textcontent,"(?i)mental\\s(?i)ilness|(?i)disorder|(?i)therapy|(?i)psychiatr|(?i)psychology") == TRUE ~ "therapy",
    str_detect(textcontent,"(?i)supreme|(?i)court|(?i)discriminat|(?i)lawsuit|(?i)legal|(?i)lawyer") == TRUE ~ "judicidial",
    str_detect(textcontent,"(?i)actor|(?i)film|(?i)movie|(?i)television|(?i)author|(?i)actress|(?i)singer") == TRUE ~ "entertainment",
    str_detect(textcontent,"(?i)legislat|(?i)bill|(?i)lawmaker|(?i)reform|(?i)senat|(?i)ban|(?i)house\\s(?i)repre|(?i)filibuster") == TRUE ~ "legislation",
    str_detect(textcontent,"(?i)medical|(?i)healthcare|(?i)hormone|(?i)medication|(?i)surgery|(?i)physician|(?i)blocker") == TRUE ~ "healthcare",
    str_detect(textcontent,"(?i)murder|(?i)rape|(?i)rapist|(?i)kidnap|(?i)killed|(?i)offender|(?i)predator|(?i)assault|(?i)shooter|(?i)shooting") == TRUE ~ "crime")) ->> ds


ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=region, colour="daily"))+
  geom_point(aes(x=the_day,y=ct,color=region, colour="daily"))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  theme(legend.position = "none")+
  facet_grid(keyword~region) -> kw


ds %>%
  ggplot()+
  geom_bar(aes(x=the_day, fill=topic), position="fill")+
  facet_grid(keyword~region)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_discrete(name="keyword")+
  labs(y="proportion of articles",x=element_blank()) -> bottom



# URL + top 2% outlets plots

  top_outlets %>% ggplot()+
  theme_minimal()+
  scale_fill_discrete(element_blank())+
  geom_bar(aes(x=count,fill=pullURL),position = "dodge")+
  theme(legend.position = "bottom",
        axis.text.y = element_blank())+
  labs(y=element_blank(),x="# articles per outlet",
       title = paste("\ntop news sources, 98th percentile (",round(min_arts),"+ max = ", max_arts, ") since: ", 
                     start_month,"/",start_day,": \n", sep="")) -> urlPlot
  
  ds[which(grepl(k,ds$pullURL)),] %>% select(pullURL,region,keyword) %>% 
    mutate(urlSmall=gsub(pattern="www.","",pullURL)) %>%
    ggplot()+
    theme_bw()+
    scale_fill_discrete(element_blank())+
    geom_bar(aes(x=pullURL,fill=urlSmall),position = "dodge")+
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())+
    labs(y=element_blank(),x=element_blank(),title = "Top drivers of conversation by region + keyword")+
    facet_grid(keyword~region) -> top_plot
  
  
gridExtra::grid.arrange(kw,bottom,urlPlot,heights=c(2,2,1.25))

grid.arrange(bottom,urlPlot,heights=c(2.5,0.75)) -> a_plot
grid.arrange(kw,top_plot,heights=c(2,1.5)) -> b_plot
gridExtra::grid.arrange(a_plot,b_plot,ncol=2)


ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=topic, colour="daily"),alpha = 0.1,position="dodge")+
  geom_point(aes(x=the_day,y=ct,color=topic, colour="daily",size=ct,alpha = 0.05))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  scale_alpha_continuous(guide="none")+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  facet_grid(keyword~region)


