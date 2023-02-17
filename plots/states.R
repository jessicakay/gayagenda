
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

paste("(?i)",paste(states,sep = " | ",collapse="|(?i)"),sep="")->state_list
ds[which(grepl(state_list,ds$textcontent)),] -> state_dataframe


ds %>%
  mutate(in_state = 
           case_when(
             str_detect(textcontent, pattern="Alabama")        == TRUE ~ "Alabama",
             str_detect(textcontent, pattern="Arizona")        == TRUE ~ "Arizona",
             str_detect(textcontent, pattern="Arkansas")       == TRUE ~ "Arkansas",
             str_detect(textcontent, pattern="California")     == TRUE ~ "California",
             str_detect(textcontent, pattern="Colorado")       == TRUE ~ "Colorado",
             str_detect(textcontent, pattern="Connecticut")    == TRUE ~ "Connecticut",
             str_detect(textcontent, pattern="DC")             == TRUE ~ "DC",
             str_detect(textcontent, pattern="Delaware")       == TRUE ~ "Delaware",
             str_detect(textcontent, pattern="Florida")        == TRUE ~ "Florida",
             str_detect(textcontent, pattern="Georgia")        == TRUE ~ "Georgia",
             str_detect(textcontent, pattern="Hawaii")         == TRUE ~ "Hawaii",
             str_detect(textcontent, pattern="Idaho")          == TRUE ~ "Idaho",
             str_detect(textcontent, pattern="Illinois")       == TRUE ~ "Illinois",
             str_detect(textcontent, pattern="Indiana")        == TRUE ~ "Indiana",
             str_detect(textcontent, pattern="Iowa")           == TRUE ~ "Iowa",
             str_detect(textcontent, pattern="Kansas")         == TRUE ~ "Kansas",
             str_detect(textcontent, pattern="Kentucky")       == TRUE ~ "Kentucky",
             str_detect(textcontent, pattern="Lousiana")       == TRUE ~ "Lousiana",
             str_detect(textcontent, pattern="Maine")          == TRUE ~ "Maine",
             str_detect(textcontent, pattern="Maryland")       == TRUE ~ "Maryland",
             str_detect(textcontent, pattern="Massachusetts")  == TRUE ~ "Massachusetts",
             str_detect(textcontent, pattern="Michigan")       == TRUE ~ "Michigan",
             str_detect(textcontent, pattern="Minnesota")      == TRUE ~ "Minnesota",
             str_detect(textcontent, pattern="Mississippi")    == TRUE ~ "Mississippi",
             str_detect(textcontent, pattern="Montana")        == TRUE ~ "Montana",
             str_detect(textcontent, pattern="Nebraska")       == TRUE ~ "Nebraska",
             str_detect(textcontent, pattern="Nevada")         == TRUE ~ "Nevada",
             str_detect(textcontent, pattern="New Hampshire")  == TRUE ~ "New Hampshire",
             str_detect(textcontent, pattern="New Jersey")     == TRUE ~ "New Jersey",
             str_detect(textcontent, pattern="New Mexico")     == TRUE ~ "New Mexico",
             str_detect(textcontent, pattern="New York")       == TRUE ~ "New York",
             str_detect(textcontent, pattern="North Carolina") == TRUE ~ "North Carolina",
             str_detect(textcontent, pattern="North Dakota")   == TRUE ~ "North Dakota",
             str_detect(textcontent, pattern="Ohio")           == TRUE ~ "Ohio",
             str_detect(textcontent, pattern="Oklahoma")       == TRUE ~ "Oklahoma",
             str_detect(textcontent, pattern="Oregon")         == TRUE ~ "Oregon",
             str_detect(textcontent, pattern="Pennsylvania")   == TRUE ~ "Pennsylvania",
             str_detect(textcontent, pattern="Puerto Rico")    == TRUE ~ "Puerto Rico",
             str_detect(textcontent, pattern="Rhode Island")   == TRUE ~ "Rhode Island",
             str_detect(textcontent, pattern="South Carolina") == TRUE ~ "South Carolina",
             str_detect(textcontent, pattern="South Dakota")   == TRUE ~ "South Dakota",
             str_detect(textcontent, pattern="Tennessee")      == TRUE ~ "Tennessee",
             str_detect(textcontent, pattern="Texas")          == TRUE ~ "Texas",
             str_detect(textcontent, pattern="Utah")           == TRUE ~ "Utah",
             str_detect(textcontent, pattern="Vermont")        == TRUE ~ "Vermont",
             str_detect(textcontent, pattern="Virginia")       == TRUE ~ "Virginia",
             str_detect(textcontent, pattern="Washington")     == TRUE ~ "Washington",
             str_detect(textcontent, pattern="West Virginia")  == TRUE ~ "West Virginia",
             str_detect(textcontent, pattern="Wisconsin")      == TRUE ~ "Wisconsin",
             str_detect(textcontent, pattern="Wyoming")        == TRUE ~ "Wyoming"
           )) -> states_data

states_data %>%
  group_by(the_day,in_state) %>%
  filter(!is.na(in_state)) %>%
  mutate(ct=n()) %>%
  ggplot()+
  geom_line(aes(x=the_day,y=ct,color=in_state, colour="daily"))+
  geom_point(aes(x=the_day,y=ct,color=in_state, colour="daily"))+
  labs(title = "Articles mentioning a US state",
       subtitle = paste(round(dim(states_data[which(!is.na(states_data$in_state)),])[1]/as.numeric(table(ds$region)[3][1])*100,2),"% out of ",
                        as.numeric(table(ds$region)[3][1])," articles.(N=",dim(ds)[1],")"),
       caption=paste("updated",Sys.time()," \ngithub.com/jessicakay/gayagenda\njkant@bu.edu"))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_dark()+
  theme(legend.position = "none", 
        panel.grid = element_blank())+
  facet_wrap(in_state~.) #-> state1

  states_data %>% group_by(in_state) %>%mutate(ct=n()) %>%
  filter(!is.na(in_state)) %>%
  ggplot()+
  geom_bar(aes(x=ct,fill=in_state))+
  theme_bw()+
  theme(legend.position = "bottom",
       legend.title=element_blank())+
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
  theme(legend.position = "bottom")+
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
  theme(legend.position = "none")+
  facet_grid(keyword~region) -> kw
}

grid.arrange(
  grid.arrange(a,b,kw,ncol=3),
  grid.arrange(state1,state2,ncol=2,widths=c(1.5,1))
)


grid.arrange(state1,state2,ncol=2,widths=c(1.5,1))


paste("(?i)",paste(states,sep = " | ",collapse="|(?i)"),sep="")->state_list
ds[which(grepl(state_list,ds$textcontent)),]

ds %>%
  mutate(in_state = 
           case_when(
             str_detect(textcontent, pattern="Alabama")        == TRUE ~ "Alabama",
             str_detect(textcontent, pattern="Arizona")        == TRUE ~ "Arizona",
             str_detect(textcontent, pattern="Arkansas")       == TRUE ~ "Arkansas",
             str_detect(textcontent, pattern="California")     == TRUE ~ "California",
             str_detect(textcontent, pattern="Colorado")       == TRUE ~ "Colorado",
             str_detect(textcontent, pattern="Connecticut")    == TRUE ~ "Connecticut",
             str_detect(textcontent, pattern="DC")             == TRUE ~ "DC",
             str_detect(textcontent, pattern="Delaware")       == TRUE ~ "Delaware",
             str_detect(textcontent, pattern="Florida")        == TRUE ~ "Florida",
             str_detect(textcontent, pattern="Georgia")        == TRUE ~ "Georgia",
             str_detect(textcontent, pattern="Hawaii")         == TRUE ~ "Hawaii",
             str_detect(textcontent, pattern="Idaho")          == TRUE ~ "Idaho",
             str_detect(textcontent, pattern="Illinois")       == TRUE ~ "Illinois",
             str_detect(textcontent, pattern="Indiana")        == TRUE ~ "Indiana",
             str_detect(textcontent, pattern="Iowa")           == TRUE ~ "Iowa",
             str_detect(textcontent, pattern="Kansas")         == TRUE ~ "Kansas",
             str_detect(textcontent, pattern="Kentucky")       == TRUE ~ "Kentucky",
             str_detect(textcontent, pattern="Lousiana")       == TRUE ~ "Lousiana",
             str_detect(textcontent, pattern="Maine")          == TRUE ~ "Maine",
             str_detect(textcontent, pattern="Maryland")       == TRUE ~ "Maryland",
             str_detect(textcontent, pattern="Massachusetts")  == TRUE ~ "Massachusetts",
             str_detect(textcontent, pattern="Michigan")       == TRUE ~ "Michigan",
             str_detect(textcontent, pattern="Minnesota")      == TRUE ~ "Minnesota",
             str_detect(textcontent, pattern="Mississippi")    == TRUE ~ "Mississippi",
             str_detect(textcontent, pattern="Montana")        == TRUE ~ "Montana",
             str_detect(textcontent, pattern="Nebraska")       == TRUE ~ "Nebraska",
             str_detect(textcontent, pattern="Nevada")         == TRUE ~ "Nevada",
             str_detect(textcontent, pattern="New Hampshire")  == TRUE ~ "New Hampshire",
             str_detect(textcontent, pattern="New Jersey")     == TRUE ~ "New Jersey",
             str_detect(textcontent, pattern="New Mexico")     == TRUE ~ "New Mexico",
             str_detect(textcontent, pattern="New York")       == TRUE ~ "New York",
             str_detect(textcontent, pattern="North Carolina") == TRUE ~ "North Carolina",
             str_detect(textcontent, pattern="North Dakota")   == TRUE ~ "North Dakota",
             str_detect(textcontent, pattern="Ohio")           == TRUE ~ "Ohio",
             str_detect(textcontent, pattern="Oklahoma")       == TRUE ~ "Oklahoma",
             str_detect(textcontent, pattern="Oregon")         == TRUE ~ "Oregon",
             str_detect(textcontent, pattern="Pennsylvania")   == TRUE ~ "Pennsylvania",
             str_detect(textcontent, pattern="Puerto Rico")    == TRUE ~ "Puerto Rico",
             str_detect(textcontent, pattern="Rhode Island")   == TRUE ~ "Rhode Island",
             str_detect(textcontent, pattern="South Carolina") == TRUE ~ "South Carolina",
             str_detect(textcontent, pattern="South Dakota")   == TRUE ~ "South Dakota",
             str_detect(textcontent, pattern="Tennessee")      == TRUE ~ "Tennessee",
             str_detect(textcontent, pattern="Texas")          == TRUE ~ "Texas",
             str_detect(textcontent, pattern="Utah")           == TRUE ~ "Utah",
             str_detect(textcontent, pattern="Vermont")        == TRUE ~ "Vermont",
             str_detect(textcontent, pattern="Virginia")       == TRUE ~ "Virginia",
             str_detect(textcontent, pattern="Washington")     == TRUE ~ "Washington",
             str_detect(textcontent, pattern="West Virginia")  == TRUE ~ "West Virginia",
             str_detect(textcontent, pattern="Wisconsin")      == TRUE ~ "Wisconsin",
             str_detect(textcontent, pattern="Wyoming")        == TRUE ~ "Wyoming"
           )) -> sd_ds

sd_ds %>% filter(in_state %in% states) %>% select(in_state) %>% table()

sd_ds %>% filter(in_state %in% states) %>% 
  select(in_state, pullURL, keyword) %>%
  group_by(in_state, pullURL, keyword) %>%
  summarize(num_urls = n()) %>% filter(num_urls>0) %>%
  filter(in_state=="Texas") %>% as_tibble()

sd_ds %>% filter(in_state %in% states) %>%
select(region, the_day, pullURL, in_state) %>% 
  filter(!is.na(pullURL) & pullURL != "www.youtube.com/") %>%
  group_by(in_state, .drop=FALSE) %>%
  summarize(count=n()) %>% filter(count>min_arts) %>% select(in_state, count, pullURL)
