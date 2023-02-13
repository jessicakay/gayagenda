

ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=topic, colour="daily"))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  facet_grid(keyword~region) -> a


ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=topic, colour="daily"),alpha = 0.05)+
  geom_point(aes(x=the_day,y=ct,color=topic, colour="daily",size=ct))+
  labs(title = " ",
       subtitle = "",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  scale_y_continuous()+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  facet_grid(keyword~region)-> b

ds %>% group_by(the_day,region) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=region, colour="daily"))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "github.com/jessicakay/gayagenda",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  scale_y_continuous()+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed")) 

ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct, colour="daily"))+
  geom_point(aes(x=the_day,y=ct,size=ct))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  scale_y_continuous()+
  theme(legend.position = "bottom")+
  facet_grid(keyword~region) -> c

ds %>% group_by(the_day,region,keyword) %>% 
  mutate(ct=n())%>%
  mutate(over1=as.numeric(ifelse(ct>30,1,0))) %>%
  ggplot()+
  geom_line(aes(x=the_day,y=ct, alpha=over1))+
  geom_point(aes(x=the_day,y=ct, colour="daily",size=ct,alpha = 0.05))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  facet_grid(keyword~region) -> d

ds %>% group_by(the_day,region,topic) %>% mutate(ct=n()) %>% ggplot()+
  geom_point(aes(x=the_day,y=ct,color=topic, colour="daily",size=ct))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  scale_y_continuous()+
  theme(legend.position = "bottom")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  facet_grid(.~region) -> e


grid.arrange(a,b,c,d,ncol=2)


png(filename = "topic_plot.png", res=800, width = 24, height = 12, units = "in")
grid.arrange(a,b,ncol=2)
dev.off()

e+theme_dark()+
  theme(plot.background=element_rect("black"),panel.grid = element_line("black"),  
        panel.background = element_rect("black"),
        legend.background = element_rect("black"),
        legend.box.background = element_rect("black"),legend.key = element_rect("black"),
        text = element_text(colour = "white"))
