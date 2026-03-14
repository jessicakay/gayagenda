#----------#

{
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    theme_dark()+
    xlab("")+ylab("")+
    labs(title = "YouTube served on Google News by keyword",
         subtitle = "2023-2025, all regions\n\n")+
    geom_bar(aes(x=keyword,fill=year))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "none")+
    scale_fill_paletteer_d("yarrr::cars")-> plot_1
  
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    theme_dark()+
    xlab("")+ylab("jessk.org/blog")+
    geom_bar(aes(x=keyword,fill=year),position = position_dodge(preserve = "single"))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          axis.text.x = element_text(color="black"),
          legend.position = "bottom")+
    scale_fill_paletteer_d("yarrr::cars") -> plot_2
  
  grid.arrange(plot_1, plot_2,ncol=1)
  
}

{
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    theme_dark()+
    xlab("")+ylab("")+
    labs(title = "YouTube served on Google News by keyword and quarter",
         subtitle = "2023-2025, all regions\n\n")+
    geom_bar(aes(x=keyword,fill=as.factor(quarter)))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "none")+
    scale_fill_paletteer_d("yarrr::info")+
    facet_grid(.~year)-> plot_b
  
  yt_df %>%
    filter(region=="all regions")%>%
    filter(keyword %in% c("biological sex","gender identity","transgender")) %>%
    mutate(year=as.factor(year))%>%
    ggplot()+
    xlab("")+ylab("jessk.org/blog")+
    labs(fill="quarter")+
    theme_dark()+
    geom_bar(aes(x=keyword,fill=as.factor(quarter)),position = position_dodge(preserve = "single"))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "bottom",
          axis.text.x = element_text(color="black"),
          strip.text = element_blank())+
    scale_fill_paletteer_d("yarrr::info")+
    facet_grid(.~year)-> plot_c
  
  grid.arrange(plot_b, plot_c,ncol=1)
  
}


#----------#
