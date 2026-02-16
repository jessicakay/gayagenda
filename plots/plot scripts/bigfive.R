
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="cnn.com") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    facet_grid(.~year)+
    ylab(label = "jessk.org/blog")+
    xlab(label = "\n\n")+
    labs(title = "cnn.com")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none") -> a
  
  
  mega_ds %>%
    #  filter(pullURL %in% six_sources) %>%
    filter(pullURL=="foxnews.com") %>%
    filter(keyword!="gender confusion") %>%
    filter(year!="2026") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    facet_grid(.~year)+
    labs(title = "foxnews.com")+
    xlab(label = "\n\n")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"), legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "right",
          axis.text.y = element_text(color = "black")) -> c
  
  mega_ds %>%
    #  filter(pullURL %in% six_sources) %>%
    filter(pullURL=="nytimes.com") %>%
    filter(keyword!="gender confusion") %>%
    filter(year!="2026") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    facet_grid(.~year)+
    labs(title = "nytimes.com")+
    xlab(label = "\n\n")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"), legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none",
          axis.text.y = element_text(color = "black")) -> b
  
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="advocate.com") %>%
    ggplot()+
    theme_dark()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    ylab(label = "")+
    xlab(label = "\n\n")+
    facet_grid(.~year)+
    labs(title = "advocate.com")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none",
          strip.text= element_text(colour="white"),
          axis.text.y = element_text(color = "black")) -> d
  
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="dailymail.co.uk") %>%
    ggplot()+
    theme_dark()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    xlab(label = "\n\n")+
    ylab(label = " ")+
    labs(title = "dailymail.co.uk")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black") ,panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none",
          axis.text.y = element_text(color = "black"),
          axis.ticks = element_blank())-> dm

  gridExtra::grid.arrange(a,b,dm,d,c,widths=c(4,4,4,4,5.5)) 
  