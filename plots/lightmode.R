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

View(ds[(ds$region=="UK"),])