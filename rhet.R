refresh("ex")

substring(str_extract(exds$EntryURL, pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z]+/"), first=9) -> exds$pullURL

exds[(exds$keyword!="woke ideology"),][c(-8)] -> exds_cleaned

# write.csv(exds_cleaned,file = "negativevalencekws.csv")

read.csv("/Users/jessa/Downloads/merged_news_feb22024.csv") -> nds

nds %>% 
  filter(region=="all regions") %>%
  select(names(exds_cleaned)) -> set1

typeof(nds$theday)

typeof(set1$the_day)
typeof(exds_cleaned$the_day)

exds_cleaned %>%
  mutate(textcontent = paste(EntryContent,EntryURL, EntryTitle)) %>%
  mutate(theday=str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
  mutate(the_day=as.Date(mdy(theday))) %>%
  filter(the_day > "2023-04-01") %>%
  group_by(keyword,pullURL) %>% mutate(urlCount=n()) %>% select(urlCount)

union(set1,exds_cleaned) %>%
  mutate(textcontent = paste(EntryContent,EntryURL, EntryTitle)) %>%
  mutate(theday=str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
  mutate(the_day=as.Date(mdy(theday))) %>%
  filter(the_day > "2023-04-01") %>%
  group_by(pullURL) %>% mutate(urlCount=n()) %>% select(urlCount)
  
  
  group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% 
  ggplot()+
  geom_point(aes(x=the_day,y=ct,color=keyword),alpha=0.01)+
  geom_smooth(aes(x=the_day,y=ct,color=keyword),se=FALSE,method="lm")+
  labs(title = "frequency of news articles in Google News index by keyword",
       subtitle = "github.com/jessicakay/gayagenda",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  ggdark::dark_theme_minimal()+
  scale_size_continuous(guide = "none")+
  theme(legend.position = "none")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  scale_color_brewer(palette = "Spectral") +
  facet_wrap(.~keyword)
  
as.character(exds_cleaned$the_day)->exds_cleaned$the_day
as.character(nds$the_day)->nds$the_day

union(set1,exds_cleaned) %>%
  mutate(textcontent = paste(EntryContent,EntryURL, EntryTitle)) %>%
  mutate(theday=str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
  mutate(the_day=as.Date(mdy(theday))) %>%
  filter(the_day> "2023-04-01") %>%
  mutate(mnth=month(the_day)) %>%
  mutate(my=quarter(the_day,with_year = T)) %>% 
  group_by(the_day,keyword) %>% mutate(ct=n()) -> datapool 

datapool %>%  
  ggplot()+
  geom_point(aes(x=the_day,y=ct,color=keyword),alpha=0.01)+
  geom_line(aes(x=the_day,y=ct,color=keyword),alpha=0.1)+
  geom_smooth(aes(x=the_day,y=ct,color=keyword),se=FALSE,method="lm")+
  xlab(element_blank())+
  ylab("number of articles")+
  ggdark::dark_theme_minimal()+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill=NA,colour="black"),
        panel.border = element_rect(fill=NA,colour="black"),
        panel.grid.major = element_blank(), 
        plot.background = element_rect("black",colour = "black"))+
  scale_color_brewer(palette = "PuRd") -> full_spread

datapool %>%  
  ggplot()+
  geom_point(aes(x=the_day,y=ct,color=keyword),alpha=0.01)+
  geom_line(aes(x=the_day,y=ct,color=keyword),alpha=0.1)+
  geom_smooth(aes(x=the_day,y=ct,color=keyword),se=FALSE,method="lm")+
  labs(title = "frequency of news articles in Google News index by keyword",
       subtitle = "All regions, by quarter",
       caption=paste("github.com/jessicakay/gayagenda | updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  ggdark::dark_theme_minimal()+
  theme(legend.position = "none")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect("black",colour = "black"))+
  scale_color_brewer(palette = "PuRd") +
  facet_grid(.~my,scales = "free_x") -> by_quarter


grid.arrange(by_quarter,full_spread,ncol=c(1)) 

