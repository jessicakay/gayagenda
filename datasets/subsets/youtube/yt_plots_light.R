#----------#

{
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    xlab("")+ylab("\n\n\n")+
    labs(title = "YouTube served on Google News",
         subtitle = "by keyword\n")+
    geom_bar(aes(x=keyword,fill=year))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "none",
          strip.background = element_rect("white"),
          strip.text = element_text(color="black"))+
    scale_fill_paletteer_d("yarrr::cars")-> plot_1
  
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    xlab("")+ylab("\njessk.org/blog\n\n")+
    geom_bar(aes(x=keyword,fill=year),position = position_dodge(preserve = "single"))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white",colour = "white"),
          legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          axis.text.x = element_text(color="black"),
          legend.position = "bottom",
          strip.text = element_text(color="black"))+
    scale_fill_paletteer_d("yarrr::cars") -> plot_2
  
  grid.arrange(plot_1, plot_2,ncol=1)->> image_a
  
}

{
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    xlab("")+ylab("\n\n\n")+
    labs(title = "by keyword and quarter",
         subtitle = "2023-2025, all regions\n\n")+
    geom_bar(aes(x=keyword,fill=as.factor(quarter)))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "none")+
    scale_fill_paletteer_d("yarrr::nemo")+
    facet_grid(.~year)-> plot_b
  
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    xlab("")+ylab("\n\n\n")+
    labs(fill="quarter")+
    geom_bar(aes(x=keyword,fill=as.factor(quarter)),position = position_dodge(preserve = "single"))+
    theme(plot.background=element_rect("white", colour = "white"),
          panel.grid = element_line("white"),  
          panel.background = element_rect("white"),
          legend.background = element_rect("white"),
          legend.box.background = element_rect("white",colour = "white"),
          legend.frame = element_rect("white"),
            legend.key = element_blank(),
          text = element_text(colour = "black"),
          legend.position = "bottom",
          axis.text.x = element_text(color="black"),
          strip.text = element_blank())+
    scale_fill_paletteer_d("yarrr::nemo")+
    facet_grid(.~year)-> plot_c
  
  grid.arrange(plot_b, plot_c,ncol=1)->>image_b
  
}

grid.arrange(image_a,image_b,ncol=2,widths=c(2,4))

#----------#
