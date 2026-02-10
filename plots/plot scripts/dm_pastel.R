as.numeric(dim(ds[which(ds$region!="all regions"),])[1]) -> num_selected
round((num_selected/dim(ds)[1])*100,2) -> perc_selected
paste(num_selected," headlines, ",
      format(min(states_data$the_day),"%m/%d")," - ",
      format(max(states_data$the_day),"%m/%d"),
      " |  (",perc_selected,"% of total dataset (N=",dim(ds)[1],")",sep="") -> cappy


states_data %>%
  group_by(the_day,in_state) %>%
  filter(!is.na(in_state)) %>%
  mutate(ct=n()) %>%
  ggplot()+
  geom_point(aes(x=the_day,y=ct,color=keyword, colour="daily"))+
  labs(title = paste("Articles mentioning a US state, ",format(min(states_data$the_day),"%m/%d")," - ",
                     format(max(states_data$the_day),"%m/%d"),sep=""),
       subtitle = cappy,
       caption=paste("updated",Sys.time()," \ngithub.com/jessicakay/gayagenda"),sep="")+
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
