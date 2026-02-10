# frequency stats
# jessdkant.bsky.social

{ library(ggplot2)
  library(dplyr)
  library(lubridate)
  library(paletteer)
  library(gridExtra)} 

# View(cleaned_ds)
# refresh(arg = "ex")

read.csv("~/gayagenda/datasets/ex_kws.csv") -> exds
read.csv("~/gayagenda/datasets/Jan2026.csv") -> ds

exds[names(exds)[1:11]] -> exds

ds <- subset(ds, select=c(-pullURL))

ds <- subset(ds, select=c(-theday, -X, -topic))
exds <- subset(exds,select = c(-X))



union(ds,exds) -> mega_ds

mega_ds %>% 
  mutate(the_day=as.Date(mdy(str_extract(
    EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>%
  mutate(quarter = quarter(lubridate::as_date(the_day))) %>%
  mutate(pullURL=
           str_remove(
             str_remove(
               str_extract(
                 str_remove(EntryURL,"www."),
                 pattern="http?s:\\/\\/[a-z0-9A-Z]+[a-z0-9A-Z.-]+/" ),
               "http?s:\\/\\/"),"/")) -> mega_ds

# start scratchpad

# dark mode 
{
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="cnn.com") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    ylab(label = "jessica kant")+
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
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
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
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    labs(title = "nytimes.com")+
    xlab(label = "\n\n")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"), legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none",
          axis.text.y = element_text(color = "white")) -> b
  
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="advocate.com") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    xlab(label = "\n\n")+
    ylab(label = "")+
    facet_grid(.~year)+
    xlab(label = "\n\njessk.org/blog")+
    labs(title = "advocate.com")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none",
          strip.text= element_text(colour="white"),
#          strip.background = element_rect(fill="lightpink"),
          axis.text.y = element_text(color = "black")) -> d
  
}

# light mode

{
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="cnn.com") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    ylab(label = "jessica kant")+
    xlab(label = "\n\n")+
    labs(title = "cnn.com")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "none",
  axis.text.y = element_text(color = "black")) -> a
  
  
  mega_ds %>%
    #  filter(pullURL %in% six_sources) %>%
    filter(pullURL=="foxnews.com") %>%
    filter(keyword!="gender confusion") %>%
    filter(year!="2026") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    labs(title = "foxnews.com")+
    xlab(label = "\n\njessk.org/blog")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect(colour="white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "right",
          axis.text.y = element_text(color = "lightgray"),
          axis.ticks = element_blank()) -> c
  
  mega_ds %>%
    #  filter(pullURL %in% six_sources) %>%
    filter(pullURL=="nytimes.com") %>%
    filter(keyword!="gender confusion") %>%
    filter(year!="2026") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    labs(title = "nytimes.com")+
    xlab(label = "\n\n")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "none",
          axis.text.y = element_text(color = "lightgray"),
          axis.ticks = element_blank()) -> b
  
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="advocate.com") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    ylab(label = "")+
    xlab(label = "\n\n")+
    labs(title = "advocate.com")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "none",
          strip.text= element_text(colour="black"),
#          strip.background = element_rect(fill="lightpink"),
          axis.text.y = element_text(color = "lightgray"),
axis.ticks = element_blank()) -> d
 
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
   
}

gridExtra::grid.arrange(a,c,widths=c(4,5.5))         -> cnn_to_foxnews
gridExtra::grid.arrange(a,b,c,widths=c(4,4,5.5))       -> cnn_to_nyt_to_Fox
gridExtra::grid.arrange(a,b,d,c,widths=c(4,4,4,6)) -> cnn_nyt_advocate_fox

gridExtra::grid.arrange(a,b,d,dm,c,widths=c(4,4,4,4,7)) 




# dark mode 
{
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="cnn.com") %>%
    ggplot()+
    geom_bar(aes(x=year,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    ylab(label = "jessk.org/blog")+
    xlab(label = " ")+
    labs(title = "cnn.com")+
    coord_cartesian(ylim = c(0,1000))+
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
    geom_bar(aes(x=year,fill=keyword), alpha=topkw,position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    labs(title = "foxnews.com")+
    xlab(label = "")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,1000))+
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
    coord_cartesian(ylim = c(0,1000))+
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
    geom_bar(aes(x=year,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    ylab(label = "")+
    xlab(label = "\n\n")+
    labs(title = "advocate.com")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none",
          strip.text= element_text(colour="black"),
          strip.background = element_rect(fill="white"),
          axis.text.y = element_text(color = "black")) -> d
  
}

# light mode 
{
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="cnn.com") %>%
    ggplot()+
    geom_bar(aes(x=year,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    ylab(label = "jessk.org/blog\n")+
    xlab(label = " ")+
    labs(title = "cnn.com")+
    coord_cartesian(ylim = c(0,1000))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "none") -> a
  
  
  mega_ds %>%
    #  filter(pullURL %in% six_sources) %>%
    filter(pullURL=="foxnews.com") %>%
    filter(keyword!="gender confusion") %>%
    filter(year!="2026") %>%
    ggplot()+
    geom_bar(aes(x=year,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    labs(title = "foxnews.com")+
    xlab(label = "")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,1000))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"), legend.background = element_rect(colour = "white", fill="white"),
          legend.box.background = element_rect(colour="white"),legend.key = element_rect(colour="white", fill = "white"),
          text = element_text(colour = "black"),
          legend.position = "right",
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(color = "white")) -> c
  
  mega_ds %>%
    #  filter(pullURL %in% six_sources) %>%
    filter(pullURL=="nytimes.com") %>%
    filter(keyword!="gender confusion") %>%
    filter(year!="2026") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    labs(title = "nytimes.com")+
    xlab(label = "\n\n")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,1000))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"), legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect(colour = "white"),
          text = element_text(colour = "black"),
          legend.position = "none",
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(color = "white")) -> b
  
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="advocate.com") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    scale_fill_paletteer_d("yarrr::cars")+
    ylab(label = "")+
    xlab(label = "\n\n")+
    labs(title = "advocate.com")+
    coord_cartesian(ylim = c(0,1000))+
    theme(plot.background=element_rect("white", colour = "white"),panel.grid = element_line("white"),  
          panel.background = element_rect("white"),legend.background = element_rect("white"),
          legend.box.background = element_rect("white"),legend.key = element_rect("white"),
          text = element_text(colour = "black"),
          legend.position = "none",
          strip.text= element_text(colour="white"),
          strip.background = element_rect(fill="black"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(color = "white")) -> d
  
}
