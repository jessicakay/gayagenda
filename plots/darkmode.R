# 2023, jessica kant

# quick frequency stats

as.numeric(dim(ds[which(ds$region!="all regions"),])[1]) -> num_selected
round((num_selected/dim(ds)[1])*100,2) -> perc_selected
paste(num_selected," headlines, ",
      format(min(states_data$the_day),"%m/%d")," - ",
      format(max(states_data$the_day),"%m/%d"),
      " |  (",perc_selected,"% of total dataset (N=",dim(ds)[1],")",sep="") -> cappy


# dark mode

grid.arrange(
  ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>%
    filter(region!="all regions") %>%
    ggplot()+
    geom_line(aes(x=the_day,y=ct,color=keyword, colour="daily"))+
    geom_point(aes(x=the_day,y=ct,color=keyword, colour="daily"))+
    labs(title = "Articles about trans people in US + UK news media",
         subtitle = cappy,
         caption="github.com/jessicakay/gayagenda | @jessdkant")+
    xlab(element_blank())+
    ylab("number of articles")+
    facet_grid(region~.)+
    theme_dark()+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),
          legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "bottom")+
    scale_color_brewer(palette = "Spectral")
,

  ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% 
    filter(region!="all regions") %>%
    ggplot()+
    geom_line(aes(x=the_day,y=ct,color=topic, colour="daily"),alpha = 0.1,position="dodge")+
    geom_point(aes(x=the_day,y=ct,color=topic, colour="daily",size=ct,alpha = 0.05))+
    labs(title = "Key topics in article excerpt, stratified by search term & region",
         subtitle = "color indicates most frequently discussed topic",
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
          panel.background = element_rect("black"),
          legend.box.background = element_rect("black"), plot.background = element_rect("black",colour = "black"))+
    facet_grid(region~keyword)
, ncol=1, heights=c(1,2))


# generate dark mode table for states 

setwd("/GitHub/misc/gayagenda/plots/")


# png(filename = paste("state_map_",gsub("-","_",Sys.Date()),".png",sep=""), 
#    res=800, width = 18, height = 16, units = "in")

states_data %>%
  group_by(the_day,in_state) %>%
  filter(!is.na(in_state)) %>%
  mutate(ct=n()) %>%
  # filter("2023-02-01" < the_day & the_day >"2023-01-01") %>%
  ggplot()+
  geom_line(aes(x=the_day,y=ct,color=keyword, colour="daily"))+
  geom_point(aes(x=the_day,y=ct,color=keyword, colour="daily"))+
  labs(title = paste("Articles mentioning a US state, ",format(min(states_data$the_day),"%m/%d")," - ",
                     format(max(states_data$the_day),"%m/%d"),sep=""),
       subtitle = paste(round(dim(states_data[which(!is.na(states_data$in_state)),])[1]/as.numeric(table(ds$region)[3][1])*100,2),"% out of ",
                        as.numeric(table(ds$region)[3][1])," articles, region: USA ( total dataset N =",dim(ds)[1],")"),
       caption=paste("updated",Sys.time()," \ngithub.com/jessicakay/gayagenda\njkant@bu.edu\n"),sep="",
       colour="Google News search term: ")+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_dark()+
  theme(legend.position = "bottom", 
        panel.grid = element_blank(), panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),
        axis.text.y = element_text(colour="white"),
        legend.background = element_rect("black"),
        legend.box.background = element_rect("black"),
        legend.key = element_rect("black"),
        plot.background = element_rect("black"), panel.background =element_rect("black"),
        text=element_text(colour="white"))+
  scale_color_brewer(palette = "Spectral")+
  facet_wrap(in_state~.)

# dev.off()

#png(filename = paste("state_map_",gsub("-","_",Sys.Date()),".png",sep=""), 
#    res=800, width = 18, height = 16, units = "in")

states_data %>%
  group_by(the_day,in_state) %>%
  filter(!is.na(in_state)) %>%
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
  facet_wrap(in_state~.)

# dev.off()



states_data %>%
  mutate(week=case_when(
      the_day >= the_day-7 ~ "past 7 days",
      the_day <= the_day-14 & the_day > the_day-7 ~ "prior week"
  )
  )
    filter("2023-02-01" < the_day & the_day >"2023-01-01") 


states_data %>%
  group_by(the_day,in_state) %>%
  filter(!is.na(in_state)) %>%
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
  facet_wrap(in_state~.)