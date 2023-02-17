
# dark mode

ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>%
  filter(region!="all regions") %>%
  ggplot()+
  geom_line(aes(x=the_day,y=ct,color=keyword, colour="daily"))+
  geom_point(aes(x=the_day,y=ct,color=keyword, colour="daily"))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  facet_grid(region~.)+
  theme_dark()+
  theme(plot.background=element_rect("black"),panel.grid = element_line("black"),  
        panel.background = element_rect("black"),
        legend.background = element_rect("black"),
        legend.box.background = element_rect("black"),legend.key = element_rect("black"),
        text = element_text(colour = "white"))+
  scale_color_brewer(palette = "Spectral")

ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% 
  filter(region!="all regions") %>%
  ggplot()+
  geom_line(aes(x=the_day,y=ct,color=topic, colour="daily"),alpha = 0.1,position="dodge")+
  geom_point(aes(x=the_day,y=ct,color=topic, colour="daily",size=ct,alpha = 0.05))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_dark()+
  scale_size_continuous(guide = "none")+
  scale_alpha_continuous(guide="none")+
  scale_color_brewer(palette = "Spectral")+
  theme(text=element_text(colour="white"),
        legend.position = "bottom",legend.background = element_rect("black"),
        panel.grid.minor = element_line(linetype = "dotted"),
        panel.grid.major = element_line(linetype = "dotted"), legend.key = element_rect("black"),
        panel.background = element_rect("black"), legend.box.background = element_rect("black"), plot.background = element_rect("black"))+
  facet_grid(region~keyword)

