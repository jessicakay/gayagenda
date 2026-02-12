
mega_ds %>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  mutate(dayow=factor(dayweek,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ggplot()+
  geom_bar(aes(x=dayow, fill=as.factor(year)))+
  facet_grid(.~pullURL)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45))+
  scale_fill_paletteer_d("palettetown::wobbuffet") -> plot_a

mega_ds %>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  mutate(dayow=factor(dayweek,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ggplot()+
  geom_bar(aes(x=dayow, fill=as.factor(year)))+
  facet_grid(pullURL~year)+ 
  theme_classic()+
  #    theme(axis.text.x = element_blank())+
  scale_fill_paletteer_d("palettetown::wobbuffet") -> plot_b

mega_ds %>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  mutate(dayow=factor(dayweek,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ggplot()+
  geom_bar(aes(x=dayow, fill=as.factor(year)),position = position_dodge())+
  facet_grid(pullURL~.)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45))+
  scale_fill_paletteer_d("palettetown::wobbuffet") -> plot_c

mega_ds %>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  mutate(dayow=factor(dayweek,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ggplot()+
  geom_bar(aes(x=dayow, fill=as.factor(year)))+
  facet_grid(year~pullURL)+ 
  theme_classic()+
  theme(axis.text.x = element_text(angle=45),
        panel.grid.minor = element_blank())+
  scale_fill_paletteer_d("palettetown::wobbuffet") -> plot_d

grid.arrange(plot_a,plot_b,plot_c,plot_d)
grid.arrange(plot_a,plot_d,ncol=2)
grid.arrange(plot_a,plot_a,plot_a,plot_a)

mega_ds %>% select(pullURL,year,dayweek)%>%reshape(idvar="pullURL",timevar="dayweek", v.names="year", direction="wide")



mega_ds %>%
  group_by(pullURL,year,dayweek) %>%
  summarize(n=n()) -> tiny_ds 

#  tiny_ds |> reshape(idvar="pullURL",timevar="year", direction="wide")



tiny_ds$year_change[tiny_ds$year=='2024',]<-

#  geom_bar(aes(x=relevel(dayweek,
#                         c("Sunday", Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")), fill=as.factor(year)))+
#    facet_grid(pullURL~year) 

mega_ds %>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  ggplot()+
  geom_bar(aes(x=dayweek, fill=keyword))+
  facet_grid(year~pullURL)+ 
  theme_classic()+
  theme(axis.text.x = element_text(angle=45),
        panel.grid.minor = element_blank())+
  scale_fill_paletteer_d("palettetown::wobbuffet") 


mega_ds %>%
  mutate(y_alpha=case_when( year == "2023" ~ 0.1,
                            year == "2024" ~ 0.5,
                            year == "2025" ~ 0.9
                            )
         )%>%
  mutate(y_alpha=as.numeric(y_alpha))%>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  mutate(dayow=factor(dayweek,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ggplot()+
  geom_bar(aes(x=dayow, fill=keyword))+
  facet_grid(year~pullURL)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45))+
  scale_fill_paletteer_d("palettetown::wobbuffet") -> plot_f

#  mutate(y_alpha=case_when( year == "2023" ~ 0.1,
#                            year == "2024" ~ 0.5,
#                            year == "2025" ~ 0.9
#  )
#  )%>%


mega_ds %>%
  mutate(y_alpha=as.numeric(y_alpha))%>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  mutate(dayow=factor(dayweek,
                      levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
         )%>%
  ggplot()+
  geom_bar(aes(x=dayow, fill=keyword))+
  facet_grid(year~pullURL)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45))+
  scale_fill_paletteer_d("palettetown::wobbuffet") -> plot_f
