
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
  facet_grid(keyword~region) -> p1

ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=topic, colour="daily"))+
  labs(title = "",
       subtitle = "github.com/jessicakay/gayagenda",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  facet_grid(keyword~region) -> p2


ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=topic, colour="daily"),alpha = 0.05)+
  geom_point(aes(x=the_day,y=ct,color=topic, colour="daily",size=ct))+
  labs(title = "Topics by search term + region",
       subtitle = "",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  scale_y_continuous()+
  theme(legend.position = "none")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  facet_grid(keyword~region) -> p3

grid.arrange(grid.arrange(p1,p2,ncol=2),p3,ncol=1)
