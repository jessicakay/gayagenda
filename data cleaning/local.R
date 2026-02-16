# new local cable analysis, started jan 2026

mega_ds %>% filter(year!="2026") %>% 
  filter(keyword=="transgender") %>%
  filter(str_detect(pullURL, '[a-z][a-z][a-z][0-9]+.....com|[0-9]+[a-z][a-z][a-z].com|channel[0-9]+.com'))%>%
  select(pullURL, year) %>% table()
  
#  filter(str_detect(pullURL, 'fox[0-9]+.....com')) %>% 
#  filter(str_detect(pullURL, '[a-z][a-z][a-z][0-9]+.....com|[0-9]+[a-z][a-z][a-z].com')) %>% 
#  filter(str_detect(pullURL, '[a-z][a-z][a-z][0-9]+.....com|[0-9]+[a-z][a-z][a-z].com|channel[0-9]+.com')) %>% 

x<-"https://abc7news.com/riley-gaines-attacked-san-francisco-state-assaulted-ambushed/13099865/"
rvest::read_html_live(x) %>% 
  html_elements("meta") %>% as.array() -> y

rvest::read_html_live(x) %>% html_elements("head")
