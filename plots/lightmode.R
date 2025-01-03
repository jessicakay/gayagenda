as.numeric(dim(ds[which(ds$region!="all regions"),])[1]) -> num_selected
round((num_selected/dim(ds)[1])*100,2) -> perc_selected
paste(num_selected," headlines, ",
      format(min(ds$the_day),"%m/%d")," - ",
      format(max(ds$the_day),"%m/%d"),
      " |  (",perc_selected,"% of total dataset (N=",dim(ds)[1],")",sep="") -> cappy



# main

ds %>% 
  filter(region!="UK") %>%
  group_by(the_day,region,keyword) %>% 
  mutate(ct=n()) %>% 
  ggplot(aes(the_day,ct,color=keyword))+
  geom_point()+
  theme_void()+
  theme(
        legend.position = "bottom")+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "jessk.org/blog",
       caption=paste("updated",Sys.time()," | @jessdkant.bsky.soc"))+
  facet_grid(.~keyword)

ds %>% 
  group_by(the_day,region,keyword) %>% 
  mutate(ct=n()) %>% 
  ggplot(aes(the_day,ct,color=region))+
  geom_point()+
  theme_void()+
  theme(
    legend.position = "bottom")+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "jessk.org/blog",
       caption=paste("updated",Sys.time()," | @jessdkant.bsky.soc"))+
  facet_grid(.~keyword)


ds %>% 
  filter(keyword=="transgender") %>%
  group_by(the_day,region,keyword) %>% 
  mutate(ct=n()) %>% 
  ggplot(aes(the_day,ct,color=region))+
  geom_point()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90))+  
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "jessk.org/blog",
       caption=paste("updated",Sys.time()," | @jessdkant.bsky.soc"))+
  facet_grid(.~keyword)


ds %>% 
  filter(keyword!="gender identity") %>%
  filter(region!="all regions")%>%
  group_by(the_day,region,keyword) %>% 
  mutate(ct=n()) %>% 
  ggplot(aes(the_day,ct,color=region))+
  geom_point()+
  theme_void()+
  theme(
    legend.position = "bottom")+
  labs(title = "Distribution",
       subtitle = "jessk.org/blog",
       caption=paste("updated",Sys.time()," | @jessdkant.bsky.soc"))+
  facet_grid(.~keyword)

ds %>% 
  filter(keyword!="gender identity") %>%
  filter(region!="all regions")%>%
  group_by(the_day,region,keyword) %>% 
  mutate(ct=n()) %>%
  ggplot()+
  geom_bar(aes(y=ct,fill=region),position="dodge")+
  theme(
    legend.position = "bottom")+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "jessk.org/blog",
       caption=paste("updated",Sys.time()," | @jessdkant.bsky.soc"))+
  facet_grid(.~keyword)
ds %>% filter(region=="all regions")%>%  
         filter(keyword=="transgender")%>%
        mutate(mnth=lubridate::month(the_day)) %>% 
        mutate(yr=lubridate::year(the_day)) %>%
    mutate(year=year(the_day)) %>% 
    group_by(the_day,region,keyword) %>% mutate(ct=n()) %>%

View(ds[(ds$region=="UK"),])




datapool %>%  
  ggplot()+
  geom_point(aes(x=the_day,y=ct,color=keyword),alpha=0.1)+
  geom_line(aes(x=the_day,y=ct,color=keyword),alpha=0.1)+
  labs(title = "frequency of news articles in Google News",
       subtitle = "All regions, by quarter")+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  scale_color_brewer(palette = "Paired") +
  facet_grid(keyword~my,scales = "free") 
