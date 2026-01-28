# frequency stats
# jessdkant.bsky.social

{ library(ggplot2)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(gridExtra)} 


ds %>% 
  filter(keyword=="transgender") %>% 
  filter(region=="all regions") %>%
  group_by(EntryURL) -> cleaned_ds

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
merge_cols <- names(ds)

exds %>% 
  mutate(the_day=as.Date(mdy(str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) -> exds
  substring(str_extract(exds$EntryURL, pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z]+/"), first=9) -> exds$pullURL


union(ds,exds) -> mega_ds
  
mega_ds %>% 
  filter(keyword!="woke ideology") %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>%
  mutate(quarter = quarter(lubridate::as_date(the_day))) -> mega_ds

# exds %>% 
#  filter(keyword!="woke ideology") %>%
#  mutate(month=month(lubridate::as_date(the_day))) %>% 
#  mutate(year = year(lubridate::as_date(the_day))) %>%
#  mutate(quarter = quarter(lubridate::as_date(the_day))) -> exds_export
  
# exds_export -> exds 
# write.csv(exds_export,"~/gayagenda/datasets/ex_kws.csv") 


# exds %>%
#  group_by(keyword,quarter,year) %>% arrange(desc(year)) %>% summarise(n=n()) -> summed

mega_ds %>%
  group_by(keyword,quarter,year) %>% arrange(desc(year)) %>% summarise(n=n()) -> summed

# install.packages("devtools")
# install.packages("paletteer")

c("www.dailysignal.com/",
  "www.christanpost.com/",
  "www.washingtonexaminer.com/",
  "www.dailywire.com/",
  "www.foxnews.com/",
  "www.breitbart.com/"
) -> six_sources


# big_six %>% 

mega_ds %>%
  filter(year!="2026") %>%
  filter(keyword!="gender confusion") %>%
  filter(pullURL=="www.cnn.com/") %>%
  ggplot()+
geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
  facet_grid(.~year)+
  scale_fill_paletteer_d("yarrr::cars")+
  theme_dark()+
  theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
        panel.background = element_rect("black"),
        legend.background = element_rect("black"),
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
#  coord_cartesian(ylim = c(0,12000))+
  theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
        panel.background = element_rect("black"),
        legend.background = element_rect("black"),
        legend.box.background = element_rect("black"),legend.key = element_rect("black"),
        text = element_text(colour = "white"),
        legend.position = "right") -> b


gridExtra::grid.arrange(a,b,widths=c(2,3))

mega_ds %>%
  filter(year!="2026") %>%
    filter(keyword!="gender confusion") %>%
    filter(keyword!="transgenderism") %>%
    filter(keyword!="gender identity") %>%
  ggplot()+
  geom_bar(aes(x=month,fill=keyword),position = position_dodge())+
  facet_grid(.~year)+
  scale_fill_paletteer_d("yarrr::up")+
  theme_dark()+
  theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
        panel.background = element_rect("black"),
        legend.background = element_rect("black"),
        legend.box.background = element_rect("black"),legend.key = element_rect("black"),
        text = element_text(colour = "white"),
        legend.position = "bottom")
