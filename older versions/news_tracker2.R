
# jkant@bu.edu

 install.packages("googlesheets4")
  
  library(ggplot2)
  library(dplyr)
  library(png)
  library(stringr)
  library(lubridate)
  library(googlesheets4)
 
# clean OAuth tokens and authenticate
#
#   detach(package:googlesheets4)
#   googlesheets4::gs4_auth()
#   headers <- c("EntryPublished","EntryTitle","EntryURL","EntryContent","FeedTitle","FeedURL","keyword","region")
#   headers   <- as.data.frame(cbind(headers))
#   googlesheets4::sheet_append(tsheetall,as.vector(headers),sheet =1) # inserts headers into blank sheet, only run first time
#
# set global variables

targsheet <- "https://docs.google.com/spreadsheets/d/1dSMwRLOJ1HbYixm7RzS_4Q8Uu1aq3326auxBkJ5g-JY/edit?usp=sharing"
tsheetall <- "https://docs.google.com/spreadsheets/d/1HW8m7xKLmCebdSa0RbmBdJkKaD3SZPc8XMQW-Q680FQ/edit?usp=sharing"

refresh<-function(){
  read_sheet(targsheet) -> dat  
  read_sheet(tsheetall) -> dat2 
  ds <<- as.data.frame(rbind(dat,dat2)) %>% as_tibble()
  ds %>%
    mutate(theday=str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
    mutate(the_day=as.Date(mdy(theday))) ->> ds
  cat("\nlast 5 entries: \n\n") ; tail(ds %>% arrange(EntryPublished),n=10)
  }

# stratify by keyword

ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=region, colour="daily"))+
  geom_point(aes(x=the_day,y=ct,color=region, colour="daily"))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  theme(legend.position = "bottom")+
  facet_grid(keyword~region) -> kw

ds %>% 
  mutate(textcontent = paste(EntryContent,EntryTitle)) %>%
  mutate(topic=case_when(
    str_detect(textcontent,"(?i)sport|(?i)athlet|(?i)competiti|(?i)swim|(?i)hockey|(?i)rugby") == TRUE ~ "sports",
    str_detect(textcontent,"(?i)restroom|(?i)bathroom|(?i)locker|(?i)naked|(?i)ymca")== TRUE ~ "bathrooms",
    str_detect(textcontent,"(?i)school|(?i)educat|(?i)universit|(?i)college|(?i)dormit|(?i)student") == TRUE ~ "education",
    str_detect(textcontent,"(?i)detrans|(?i)desist|(?i)de-trans|(?i)regret") == TRUE ~ "detransition",
    str_detect(textcontent,"(?i)mental\\s(?i)ilness|(?i)disorder|(?i)therapy|(?i)psychiatr|(?i)psychology") == TRUE ~ "therapy",
    str_detect(textcontent,"(?i)supreme|(?i)court|(?i)discriminat|(?i)lawsuit|(?i)legal|(?i)lawyer") == TRUE ~ "judicidial",
    str_detect(textcontent,"(?i)actor|(?i)film|(?i)movie|(?i)television|(?i)author|(?i)actress|(?i)singer") == TRUE ~ "entertainment",
    str_detect(textcontent,"(?i)legislat|(?i)bill|(?i)lawmaker|(?i)reform|(?i)senat|(?i)ban|(?i)house\\s(?i)repre") == TRUE ~ "legislation",
    str_detect(textcontent,"(?i)medical|(?i)healthcare|(?i)hormone|(?i)medication|(?i)surgery|(?i)physician") == TRUE ~ "healthcare",
    str_detect(textcontent,"(?i)murder|(?i)rape|(?i)rapist|(?i)kidnap|(?i)killed|(?i)offender|(?i)predator|(?i)assault") == TRUE ~ "crime")) %>% 
  ggplot()+
  geom_bar(aes(x=the_day, fill=topic), position="fill")+
  facet_grid(keyword~region)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_discrete(name="keyword")+
  labs(y="proportion of articles",x=element_blank()) -> bottom

# URL plot

substring(str_extract(ds$EntryURL, pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z]+/"), first=9) -> ds$pullURL
min_arts <- as.numeric(summarize(group_by(ds %>% filter(!is.na(pullURL)), pullURL),ct=n())$ct %>% quantile(c(.98)))
max_arts <- as.numeric(summarize(group_by(ds %>% filter(!is.na(pullURL)), pullURL),ct=n())$ct %>% max)
month(head(sort(ds$the_day))[1]) -> start_month
  day(head(sort(ds$the_day))[1]) -> start_day

  ds %>%
  select(region, the_day, pullURL) %>%
  filter(!is.na(pullURL) & pullURL != "www.youtube.com/") %>%
  group_by(pullURL,.drop=FALSE) %>%
  summarize(count=n()) %>% filter(count>min_arts) %>%
  ggplot()+
  theme_minimal()+
  scale_fill_discrete(element_blank())+
  geom_bar(aes(x=count,fill=pullURL),position = "dodge")+
  theme(legend.position = "bottom",
        axis.text.y = element_blank())+
  labs(y=element_blank(),x="# articles per outlet",
       title = paste("\ntop news sources, 98th percentile (",round(min_arts),"+ max = ", max_arts, ") since: ", 
                     start_month,"/",start_day,": \n", sep="")) -> urlPlot

gridExtra::grid.arrange(kw,bottom,urlPlot,heights=c(2,2,1.25))


# 

install.packages("googledrive") ; library(googledrive)

googledrive::drive_find(pattern = "trans news tracker",verbose = TRUE ) -> data_sheets 


googlesheets4::read_sheet(data_sheets)

dat <- NULL
for(i in 1:dim(data_sheets)[1]){
  if(i==1){
    googlesheets4::read_sheet(data_sheets$id[i]) -> dat
  }else{
  rbind(dat, googlesheets4::read_sheet(data_sheets$id[i])) ->> dat
  }
  }

as.data.frame(rbind(dat,dat2)) %>% as_tibble()

# below this line is more of a scrap notebook to be incorporated later ; will deprecate
# 
# attempt using base R

substring(str_extract(ds$EntryURL[-which(str_detect(ds$EntryURL,pattern="www"))], 
                      pattern="https:\\/\\/?.[a-zA-Z0-9]+?.?[a-z]+/"), first=9) -> urlList
append(substring(str_extract(ds$EntryURL[which(str_detect(ds$EntryURL,pattern="www"))], 
                             pattern="https:\\/\\/?www.[a-zA-Z0-9]+?.?[a-z]+/"), first=9),urlList) -> x
as.data.frame(x)->x

# experimental NLP section, keywords used to further tag items

ds$tag_sports <- ifelse(grepl("(?i)sport|(?i)athletic|(?i)athlete|(?i)competition", ds$EntryContent),1,0)
ds$tag_leg    <- ifelse(grepl("(?i)bill|(?i)legislat",                              ds$EntryContent),1,0)
ds$tag_school <- ifelse(grepl("(?i)school|(?i)educat|(?i)universit",                ds$EntryContent),1,0)
ds$tag_sglsex <- ifelse(grepl("(?i)women\\sonly|(?i)single-sex|(?i)sex-based",      ds$EntryContent),1,0)


colIDs <- names(select(ds, contains("tag_")))

ds[which(ds$tag_leg==1),] %>% select(tag_leg, region,the_day) %>% ggplot()+
  geom_bar(aes(x=the_day,fill=region))

ds %>% tidyr::unite("tags",colIDs, sep = ",", remove = FALSE) %>% select(tags) %>% View()

# NLP using TM package

library(tm)

gsub("<b>|</b>|&nbsp;|;|&#39;|(|)|\\...|&quo","",paste(ds$EntryContent,ds$EntryTitle,collapse = " ")) -> x

as.data.frame(strsplit(x," ")) -> z
VCorpus(VectorSource(z)) -> zx
tm_map(zx,removeWords,stopwords(kind = "en")) -> z
findFreqTerms(term_matrix,lowfreq = 150)
TermDocumentMatrix(Corpus(VectorSource(x))) -> term_matrix
findAssocs(term_matrix, term="transgender",corlimit = 0.1)

# RSS directly

install.packages("tidyRSS")
library(tidyRSS)

tidyRSS::tidyfeed(feed = "https://www.google.com/alerts/feeds/02717371275706320887/1916116072231156194")
