
# experimental NLP section, keywords used to further tag items

ds$tag_sports <- ifelse(grepl("(?i)sport|(?i)athletic|(?i)athlete|(?i)competition", ds$EntryContent),1,0)
ds$tag_leg    <- ifelse(grepl("(?i)bill|(?i)legislat|(?i)senat|(?i)representative",                              ds$EntryContent),1,0)
ds$tag_school <- ifelse(grepl("(?i)school|(?i)educat|(?i)universit",                ds$EntryContent),1,0)
ds$tag_sglsex <- ifelse(grepl("(?i)women\\sonly|(?i)single-sex|(?i)sex-based",      ds$EntryContent),1,0)


colIDs <- names(select(ds, contains("tag_")))

ds[which(ds$tag_leg==1),] %>% select(tag_leg, region,the_day) %>% ggplot()+
  geom_bar(aes(x=the_day,fill=region),position = "stack")+theme_void()


ds %>% mutate(timetime=as.POSIXct(thetime)) %>%
  ggplot()+
  geom_point(aes(x=the_day,y=timetime,color=topic))+
  facet_grid(keyword~region)+
  theme_minimal()


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


ds$EntryPublished

paste("(?i)",paste(states,sep = " | ",collapse="|(?i)"),sep="")->state_list
ds[which(grepl(state_list,ds$textcontent)),]




kw+theme_dark()+
  theme(plot.background = element_rect("black"),
        panel.background = element_rect("black"),
        legend.background = element_rect("black"),
        legend.key = element_rect("black"),
        text = element_text(colour = "white"),
        panel.grid = element_blank())+
  scale_color_brewer(palette = "Reds")





# 



states_data %>%
  filter(the_day<the_day-14) %>%
  mutate(week=case_when(
    the_day >= today()-7 ~ "past 7 days",
    the_day >= today()-14 & the_day < today()-7 ~ "prior week")
    ) %>%
  group_by(the_day,in_state) %>%
  filter(!is.na(in_state) & region!="all regions") %>%
  mutate(ct=n()) %>%
  # filter("2023-02-01" < the_day & the_day >"2023-01-01") %>%
  ggplot()+
  geom_line(aes(x=the_day,y=ct,color="orange", colour="daily"))+
  geom_point(aes(x=the_day,y=ct,color="orange", colour="daily"))+
  labs(title = paste("Articles mentioning a US state, ",format(min(states_data$the_day),"%m/%d")," - ",
                     format(max(states_data$the_day),"%m/%d"),sep=""),
       subtitle = paste(round(dim(states_data[which(!is.na(states_data$in_state)),])[1]/as.numeric(table(ds$region)[3][1])*100,2),"% out of ",
                        as.numeric(table(ds$region)[3][1])," articles, region: USA ( total dataset N =",dim(ds)[1],")"),
       caption=paste("updated",Sys.time()," \ngithub.com/jessicakay/gayagenda\njkant@bu.edu\n"),sep="",
       colour="Google News search term: ")+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_dark()+
  theme(legend.position = "none", 
        panel.grid = element_blank(), panel.border = element_blank(), 
        axis.text.x = element_text(colour="white",angle = 90),
        axis.text.y = element_text(colour="white"),
        legend.background = element_rect("black"),
        legend.box.background = element_rect("black"),
        legend.key = element_rect("black"),
        plot.background = element_rect("black"), panel.background =element_rect("black"),
        text=element_text(colour="white"))+
  scale_color_brewer(palette = "Spectral")+
  facet_grid(in_state~.)


ds %>% group_by(pullURL,keyword) %>% 
  summarize(freq=n()) %>% 
  filter(freq>9) %>%
  arrange(by=rev(freq))

ds[which(str_detect(ds$EntryURL,pattern = "dailywire") & ds$region == "USA"),] %>% select(EntryPublished,EntryTitle,keyword) %>% arrange(keyword)

