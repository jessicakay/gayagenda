{
  c("Alabama",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Connecticut",
    "DC",
    "Delaware",
    "Florida",
    "Georgia",
    "Hawaii",
    "Idaho",
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Kentucky",
    "Lousiana",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Montana",
    "Nebraska",
    "Nevada",
    "New Hampshire",
    "New Jersey",
    "New Mexico",
    "New York",
    "North Carolina",
    "North Dakota",
    "Ohio",
    "Oklahoma",
    "Oregon",
    "Pennsylvania",
    "Puerto Rico",
    "Rhode Island",
    "South Carolina",
    "South Dakota",
    "Tennesee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "West Virginia",
    "Wisconsin",
    "Wyoming") -> states }

ds %>%
  mutate(in_state = 
           case_when(
             str_detect(ds$textcontent, pattern="^Alabama")        == TRUE ~ "Alabama",
             str_detect(ds$textcontent, pattern="^Arizona")        == TRUE ~ "Arizona",
             str_detect(ds$textcontent, pattern="^Arkansas")       == TRUE ~ "Arkansas",
             str_detect(ds$textcontent, pattern="^California")     == TRUE ~ "California",
             str_detect(ds$textcontent, pattern="^Colorado")       == TRUE ~ "Colorado",
             str_detect(ds$textcontent, pattern="^Connecticut")    == TRUE ~ "Connecticut",
             str_detect(ds$textcontent, pattern="^DC")             == TRUE ~ "DC",
             str_detect(ds$textcontent, pattern="^Delaware")       == TRUE ~ "Delaware",
             str_detect(ds$textcontent, pattern="^Florida")        == TRUE ~ "Florida",
             str_detect(ds$textcontent, pattern="^Georgia")        == TRUE ~ "Georgia",
             str_detect(ds$textcontent, pattern="^Hawaii")         == TRUE ~ "Hawaii",
             str_detect(ds$textcontent, pattern="^Idaho")          == TRUE ~ "Idaho",
             str_detect(ds$textcontent, pattern="^Illinois")       == TRUE ~ "Illinois",
             str_detect(ds$textcontent, pattern="^Indiana")        == TRUE ~ "Indiana",
             str_detect(ds$textcontent, pattern="^Iowa")           == TRUE ~ "Iowa",
             str_detect(ds$textcontent, pattern="^Kansas")         == TRUE ~ "Kansas",
             str_detect(ds$textcontent, pattern="^Kentucky")       == TRUE ~ "Kentucky",
             str_detect(ds$textcontent, pattern="^Lousiana")       == TRUE ~ "Lousiana",
             str_detect(ds$textcontent, pattern="^Maine")          == TRUE ~ "Maine",
             str_detect(ds$textcontent, pattern="^Maryland")       == TRUE ~ "Maryland",
             str_detect(ds$textcontent, pattern="^Massachusetts")  == TRUE ~ "Massachusetts",
             str_detect(ds$textcontent, pattern="^Michigan")       == TRUE ~ "Michigan",
             str_detect(ds$textcontent, pattern="^Minnesota")      == TRUE ~ "Minnesota",
             str_detect(ds$textcontent, pattern="^Mississippi")    == TRUE ~ "Mississippi",
             str_detect(ds$textcontent, pattern="^Montana")        == TRUE ~ "Montana",
             str_detect(ds$textcontent, pattern="^Nebraska")       == TRUE ~ "Nebraska",
             str_detect(ds$textcontent, pattern="^Nevada")         == TRUE ~ "Nevada",
             str_detect(ds$textcontent, pattern="^New Hampshire")  == TRUE ~ "New Hampshire",
             str_detect(ds$textcontent, pattern="^New Jersey")     == TRUE ~ "New Jersey",
             str_detect(ds$textcontent, pattern="^New Mexico")     == TRUE ~ "New Mexico",
             str_detect(ds$textcontent, pattern="^New York")       == TRUE ~ "New York",
             str_detect(ds$textcontent, pattern="^North Carolina") == TRUE ~ "North Carolina",
             str_detect(ds$textcontent, pattern="^North Dakota")   == TRUE ~ "North Dakota",
             str_detect(ds$textcontent, pattern="^Ohio")           == TRUE ~ "Ohio",
             str_detect(ds$textcontent, pattern="^Oklahoma")       == TRUE ~ "Oklahoma",
             str_detect(ds$textcontent, pattern="^Oregon")         == TRUE ~ "Oregon",
             str_detect(ds$textcontent, pattern="^Pennsylvania")   == TRUE ~ "Pennsylvania",
             str_detect(ds$textcontent, pattern="^Puerto Rico")    == TRUE ~ "Puerto Rico",
             str_detect(ds$textcontent, pattern="^Rhode Island")   == TRUE ~ "Rhode Island",
             str_detect(ds$textcontent, pattern="^South Carolina") == TRUE ~ "South Carolina",
             str_detect(ds$textcontent, pattern="^South Dakota")   == TRUE ~ "South Dakota",
             str_detect(ds$textcontent, pattern="^Tennessee")      == TRUE ~ "Tennessee",
             str_detect(ds$textcontent, pattern="^Texas")          == TRUE ~ "Texas",
             str_detect(ds$textcontent, pattern="^Utah")           == TRUE ~ "Utah",
             str_detect(ds$textcontent, pattern="^Vermont")        == TRUE ~ "Vermont",
             str_detect(ds$textcontent, pattern="^Virginia")       == TRUE ~ "Virginia",
             str_detect(ds$textcontent, pattern="^Washington")     == TRUE ~ "Washington",
             str_detect(ds$textcontent, pattern="^West Virginia")  == TRUE ~ "West Virginia",
             str_detect(ds$textcontent, pattern="^Wisconsin")      == TRUE ~ "Wisconsin",
             str_detect(ds$textcontent, pattern="^Wyoming")        == TRUE ~ "Wyoming"
           )) %>%
  filter(region == "USA" & !is.na(in_state)) -> states_data

states_data %>%
  group_by(the_day,in_state) %>%
  mutate(ct=n()) %>%
  ggplot()+
  geom_line(aes(x=the_day,y=ct,color=in_state, colour="daily"))+
  geom_point(aes(x=the_day,y=ct,color=in_state, colour="daily"))+
  labs(title = "Articles mentioning a US state",
       subtitle = paste(round(dim(states_data)[1]/dim(ds)[1]*100,2),"% out of ",dim(ds)[1]," articles."),
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_light()+
  theme(legend.position = "none", 
        panel.grid = element_blank())+
  facet_wrap(in_state~.) -> state1

states_data %>% group_by(in_state) %>%mutate(ct=n()) %>%
  ggplot()+
  geom_bar(aes(x=ct,fill=in_state))+
  theme_light()+
  theme(legend.position = "right")+
  labs(y="number of articles mentioning a state",x=element_blank()) -> state2

{
ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=topic, colour="daily"))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  theme(legend.position = "none")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  facet_grid(keyword~region) -> a
ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=topic, colour="daily"),alpha = 0.05)+
  geom_point(aes(x=the_day,y=ct,color=topic, colour="daily",size=ct))+
  labs(title = " ",
       subtitle = "",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  scale_size_continuous(guide = "none")+
  scale_y_continuous()+
  theme(legend.position = "none")+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed"))+
  facet_grid(keyword~region)-> b
ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=region, colour="daily"))+
  geom_point(aes(x=the_day,y=ct,color=region, colour="daily"))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  theme(legend.position = "bottom")+
  facet_grid(keyword~region) -> kw
}

grid.arrange(
  grid.arrange(a,b,kw,ncol=3),
  grid.arrange(state2,state1,ncol=2,widths=c(1,1))
)
