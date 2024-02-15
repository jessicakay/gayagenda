refresh("ex")


# write.csv(exds_cleaned,file = "negativevalencekws.csv")

read.csv("/Users/jessa/Downloads/merged_news_feb22024.csv") -> nds
read.csv("/Users/jessa/OneDrive/Documents/GitHub/misc/gayagenda/datasets/negativevalencekws.csv") -> exds_cleaned

# exds[(exds$keyword!="woke ideology"),][c(-8)] -> exds_cleaned


nds %>% 
  filter(region=="all regions") %>%
  select(names(exds_cleaned)) -> set1
  
as.character(exds_cleaned$the_day)->exds_cleaned$the_day
as.character(nds$the_day)->nds$the_day

union(set1,exds_cleaned) %>%
  mutate(textcontent = paste(EntryContent,EntryURL, EntryTitle)) %>%
  mutate(theday=str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
  mutate(the_day=as.Date(mdy(theday))) %>%
  filter(the_day> "2023-04-01") %>%
  mutate(pullURL=substring(str_extract(EntryURL, pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z0-9]+?.?[a-z]+?.[a-z]/"), first=9)) %>%
  mutate(mnth=month(the_day)) %>%
  group_by(EntryURL) %>%
  mutate(numStory=n())%>%
  ungroup()%>%
  mutate(my=quarter(the_day,with_year = T)) %>% 
  group_by(the_day,keyword) %>% mutate(ct=n()) -> datapool
  
datapool %>%  
  ggplot()+
  #geom_point(aes(x=the_day,y=ct,color=keyword),alpha=0.01)+
  geom_line(aes(x=the_day,y=ct,color=keyword),alpha=0.1)+
  geom_smooth(aes(x=the_day,y=ct,color=keyword),se=FALSE,)+
  xlab(element_blank())+
  ylab("number of articles")+
  labs(caption=paste("jessk.org/blog"))+
  ggdark::dark_theme_minimal()+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill=NA,colour="black"),
        panel.border = element_rect(fill=NA,colour="black"),
        panel.grid.major = element_blank(), 
        plot.background = element_rect("black",colour = "black"))+
  scale_color_brewer(palette = "Spectral")  

datapool %>%  
  ggplot()+
  geom_point(aes(x=the_day,y=ct,color=keyword),alpha=0.01)+
  geom_line(aes(x=the_day,y=ct,color=keyword),alpha=0.1)+
  geom_smooth(aes(x=the_day,y=ct,color=keyword),se=FALSE,method="lm")+
  labs(title = "frequency of news articles in Google News",
       subtitle = "All regions, by quarter")+
  xlab(element_blank())+
  ylab("number of articles")+
  ggdark::dark_theme_minimal()+
  theme(legend.position = "none")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect("black",colour = "black"))+
  scale_color_brewer(palette = "PuRd") +
  facet_grid(.~my,scales = "free_x") -> by_quarter


# png(filename = "long_smooth.png",width = 3000, height = 2000, res = 300)
# full_spread 
# dev.off()


datapool %>%
  select(EntryURL, pullURL) %>%
  distinct(EntryURL) %>% as_tibble()

# datapool %>%
#  filter(pullURL=="www.dailysignal.com/") %>% 
#  group_by(keyword,my) %>%
#  mutate(npermonth=n()) -> dp

datapool[which(grepl("(?)dailysignal|(?)christianpost|(?)washingtonexaminer|(?)dailywire|(?)www.foxnews|(?)breitbart.com",datapool$pullURL)),] -> dp
datapool[which(grepl("(?)nbcnews|(?)msnbc|(?)www.cnn.com|(?)nytimes.com|(?)huffpost|(?)cbsnews",datapool$pullURL)),] -> dp

dp %>%
  ungroup() %>%
  distinct(EntryURL,.keep_all = T)->dp

write.csv(table(dp$mnth,dp$pullURL),"clipboard")


#    select(mnth,keyword) %>%
#    table() %>% write.csv("clipboard")




write.csv(table(dp$mnth,dp$keyword),"clipboard")


table(dp$mnth,dp$pullURL)


c("www.dailysignal.com/",
  "www.christanpost.com/",
  "www.washingtonexaminer.com/",
  "www.dailywire.com/",
  "www.foxnews.com/",
  "www.breitbart.com/"
  ) -> six_sources

dp$pullURL

