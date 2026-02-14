
# stacked plot, top 5 outlets by weekday over year
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

# stacked plot, top 5 outlets by year over weekday
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

# weekday year, pullURL horizontal
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

# more plots
mega_ds %>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  mutate(dayow=factor(dayweek,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ggplot()+
  geom_bar(aes(x=dayow, fill=as.factor(year)),position = position_dodge())+
  facet_grid(keyword~pullURL)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45))+
  scale_fill_paletteer_d("palettetown::ivysaur")+ 
  scale_color_paletteer_d("palettetown::ivysaur") -> plot_e

mega_ds %>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  arrange(dayweek,year,pullURL) %>% 
  mutate(dayow=factor(dayweek,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ggplot()+
  geom_bar(aes(x=dayow, fill=as.factor(year)),position = position_dodge())+
  facet_grid(keyword~pullURL)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45))+
  scale_fill_paletteer_d("palettetown::wobbuffet")+ 
  scale_color_paletteer_d("palettetown::wobbuffet") -> plot_f

mega_ds %>%
  filter(pullURL %in% topouts) %>%
  select(dayweek, year, region, keyword, pullURL) %>% 
  filter(region=="all regions") %>%
  filter(keyword %in% main_kws) %>%
  arrange(dayweek,year,pullURL) %>% 
  mutate(dayow=factor(dayweek,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ggplot()+
  geom_bar(aes(x=dayow, fill=as.factor(year)),position = position_dodge())+
  facet_grid(keyword~pullURL)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45))+
  scale_fill_paletteer_d("palettetown::wobbuffet")+ 
  scale_color_paletteer_d("palettetown::wobbuffet") -> plot_g


grid.arrange(plot_a,plot_b,plot_c,plot_d)
grid.arrange(plot_a,plot_d,ncol=2)
grid.arrange(plot_a,plot_a,plot_a,plot_a)

grid.arrange(plot_a,plot_f,ncol=2)

grid.arrange(
  grid.arrange(plot_f,plot_g,ncol=1),
  plot_a,ncol=2)


grid.arrange(
  grid.arrange(plot_c+
                 theme(legend.position = "none",
                       axis.text.x = element_blank()),plot_b+theme(legend.position = "none",
                       axis.text.x = element_blank()),ncol=1),plot_a,ncol=2)

# mega_ds %>% select(pullURL,year,dayweek)%>%reshape(idvar="pullURL",timevar="dayweek", v.names="year", direction="wide")

mega_ds %>%
  group_by(pullURL,dayweek, year) %>%
  arrange(pullURL,dayweek, year)%>%
  summarize(year_count=n()) %>%
  tidyr::pivot_wider(names_from=dayweek,values_from=year_count,values_fill=0) %>%
    filter(pullURL %in% topouts)-> tiny_ds

tiny_ds$yc_Monday   <-0
tiny_ds$yc_Tuesday  <-0
tiny_ds$yc_Wednesday<-0
tiny_ds$yc_Thursday <-0
tiny_ds$yc_Friday   <-0
tiny_ds$yc_Saturday <-0
tiny_ds$yc_Sunday   <-0

tiny_ds$Monday[tiny_ds$year=="2024"] -    tiny_ds$Monday[tiny_ds$year=="2023"] ->     tiny_ds$yc_Monday[(tiny_ds$year=="2024")]
tiny_ds$Monday[tiny_ds$year=="2025"] -    tiny_ds$Monday[tiny_ds$year=="2024"] ->     tiny_ds$yc_Monday[(tiny_ds$year=="2025")]
tiny_ds$Tuesday[tiny_ds$year=="2024"] -   tiny_ds$Tuesday[tiny_ds$year=="2023"] ->    tiny_ds$yc_Tuesday[(tiny_ds$year=="2024")]
tiny_ds$Tuesday[tiny_ds$year=="2025"] -   tiny_ds$Tuesday[tiny_ds$year=="2024"] ->    tiny_ds$yc_Tuesday[(tiny_ds$year=="2025")]
tiny_ds$Wednesday[tiny_ds$year=="2024"] - tiny_ds$Wednesday[tiny_ds$year=="2023"] ->  tiny_ds$yc_Wednesday[(tiny_ds$year=="2024")]
tiny_ds$Wednesday[tiny_ds$year=="2025"] - tiny_ds$Wednesday[tiny_ds$year=="2024"] ->  tiny_ds$yc_Wednesday[(tiny_ds$year=="2025")]
tiny_ds$Thursday[tiny_ds$year=="2024"] -  tiny_ds$Thursday[tiny_ds$year=="2023"] ->   tiny_ds$yc_Thursday[(tiny_ds$year=="2024")]
tiny_ds$Thursday[tiny_ds$year=="2025"] -  tiny_ds$Thursday[tiny_ds$year=="2024"] ->   tiny_ds$yc_Thursday[(tiny_ds$year=="2025")]
tiny_ds$Friday[tiny_ds$year=="2024"] -    tiny_ds$Friday[tiny_ds$year=="2023"] ->     tiny_ds$yc_Friday[(tiny_ds$year=="2024")]
tiny_ds$Friday[tiny_ds$year=="2025"] -    tiny_ds$Friday[tiny_ds$year=="2024"] ->     tiny_ds$yc_Friday[(tiny_ds$year=="2025")]
tiny_ds$Saturday[tiny_ds$year=="2024"] -  tiny_ds$Saturday[tiny_ds$year=="2023"] ->   tiny_ds$yc_Saturday[(tiny_ds$year=="2024")]
tiny_ds$Saturday[tiny_ds$year=="2025"] -  tiny_ds$Saturday[tiny_ds$year=="2024"] ->   tiny_ds$yc_Saturday[(tiny_ds$year=="2025")]
tiny_ds$Sunday[tiny_ds$year=="2024"] -    tiny_ds$Sunday[tiny_ds$year=="2023"] ->     tiny_ds$yc_Sunday[(tiny_ds$year=="2024")]
tiny_ds$Sunday[tiny_ds$year=="2025"] -    tiny_ds$Sunday[tiny_ds$year=="2024"] ->     tiny_ds$yc_Sunday[(tiny_ds$year=="2025")]



#  geom_bar(aes(x=relevel(dayweek,
#                         c("Sunday", Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")), 
#  fill=as.factor(year)))+facet_grid(pullURL~year) 

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
  scale_fill_paletteer_d("palettetown::wobbuffet") -> plot_kws

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
  scale_fill_paletteer_d("palettetown::wobbuffet") -> plot_e

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
