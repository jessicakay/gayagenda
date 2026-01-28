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


 ds %>% 
  mutate(the_day=as.Date(mdy(str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>%
  mutate(quarter = quarter(lubridate::as_date(the_day)))-> ds
  substring(str_extract(ds$EntryURL, pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z]+/"), first=9) -> ds$pullURL

ds <- subset(ds, select=c(-theday, -X, -topic))
exds <- subset(exds,select = c(-X))

exds %>% 
  mutate(the_day=as.Date(mdy(str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) -> exds
  substring(str_extract(exds$EntryURL, pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z]+/"), first=9) -> exds$pullURL


union(ds,exds) -> mega_ds
  
mega_ds %>% 
  filter(keyword!="woke ideology") %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>%
  mutate(quarter = quarter(lubridate::as_date(the_day))) -> mega_ds

# start scratchpad

{
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="www.cnn.com/") %>%
    ggplot()+
  geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    ylab(label = "jessica kant")+
    xlab(label = " ")+
    labs(title = "cnn.com")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none") -> a
  
  
  mega_ds %>%
  #  filter(pullURL %in% six_sources) %>%
    filter(pullURL=="www.foxnews.com/") %>%
    filter(keyword!="gender confusion") %>%
    filter(year!="2026") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    labs(title = "foxnews.com")+
    xlab(label = "")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"), legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "right") -> c
  
  mega_ds %>%
    #  filter(pullURL %in% six_sources) %>%
    filter(pullURL=="www.nytimes.com/") %>%
    filter(keyword!="gender confusion") %>%
    filter(year!="2026") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    labs(title = "nytimes.com")+
    xlab(label = "")+
    ylab(label = "")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"), legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "white"),
          legend.position = "none") -> b
  
  mega_ds %>%
    filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(pullURL=="www.advocate.com/") %>%
    ggplot()+
    geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
    facet_grid(.~year)+
    scale_fill_paletteer_d("yarrr::cars")+
    theme_dark()+
    ylab(label = "")+
    xlab(label = "jessk.org/blog")+
    labs(title = "advocate.com")+
    coord_cartesian(ylim = c(0,300))+
    theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
          panel.background = element_rect("black"),legend.background = element_rect("black"),
          legend.box.background = element_rect("black"),legend.key = element_rect("black"),
          text = element_text(colour = "hotpink"),
          legend.position = "none",
          strip.text= element_text(colour="black"),
          strip.background = element_rect(fill="purple")) -> d

}

  gridExtra::grid.arrange(a,c,widths=c(4,5.5))         -> cnn_to_foxnews
  gridExtra::grid.arrange(a,b,c,widths=c(4,4,5))       -> cnn_to_nyt_to_Fox
  gridExtra::grid.arrange(a,b,d,c,widths=c(4,4,4,5.5)) -> cnn_nyt_advocate_fox
