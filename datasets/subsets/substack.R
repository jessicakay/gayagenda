colnames(sub2)[2]<-"Substack_CDN"
sub2 %>% arrange(desc(as.integer(subscribers))) %>% View()

sub2 %>% mutate(CDN_detected=
                  case_when(
                    Substack_CDN==FALSE ~ "metadata",
                    Substack_CDN==TRUE ~ "headers",
                    Substack_CDN=="error" & str_detect(profile_data, "subscriber") ~ "metadata" 
                    ))%>%
        mutate(verified=
                 case_when(
                  Substack_CDN==TRUE ~ "verified",
                  !is.integer(subscribers) ==TRUE ~ "verified",
                 )
               ) -> sub2

sub2$Substack_CDN[(sub2$Substack_CDN==FALSE & sub2$profile_data=="error")]<-"not detected"


mega_ds %>% filter(pullURL %in% substacks) -> substack_subset

install.packages("viridis")
library(viridis)


mega_ds %>%
  filter(pullURL %in% c(sub_list$test_url)) %>%
  filter((region == "all regions") | (is.na(region)) )%>%
  group_by(keyword,pullURL)%>%
  mutate(count=n())%>%
  ungroup() %>% 
  ggplot(aes(keyword,the_day))+
  geom_point(aes(colour = count), position = position_dodge(width = 1))+
  theme_minimal()+
  labs(title="Articles per Substack x date")+
  scale_colour_viridis(discrete=F,option="D")+
  xlab("")+
  ylab("")+
  theme(panel.grid.major.x = element_line("white"),
        panel.grid.major.y = element_line("white"),
        panel.grid = element_line("white"),axis.text.x = element_blank(),legend.position = "bottom")  -> a

mega_ds %>%
  filter(pullURL %in% c(sub_list$test_url)) %>%
  filter((region == "all regions") | (is.na(region)) )%>%
  group_by(keyword,pullURL)%>%
  mutate(count=n())%>%
  ungroup() %>% 
  ggplot(aes(keyword,count))+
  geom_point(aes(colour = count))+
  labs(title="Per Subtack")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  scale_colour_viridis(discrete=F,option="D")+
  theme(panel.grid.major.x = element_line("white"),
        panel.grid.major.y = element_line("white"),
        panel.grid = element_line("white"),,axis.text.x = element_text(angle = 90),legend.position = "none") -> b


grid.arrange(b,a,ncol=1)


mega_ds %>% filter(pullURL %in% substacks) -> substack_subset

install.packages("viridis")
library(viridis)


mega_ds %>%
  filter(pullURL %in% c(sub_list$test_url)) %>%
  filter((region == "all regions") | (is.na(region)) )%>%
  group_by(pullURL,year,month)%>%
  mutate(count=n())%>%
  ungroup() %>% 
  ggplot(aes(keyword,count))+
  geom_point(aes(colour = count,size=1),position = position_dodge(width = 1))+
  theme_minimal()+
  labs(title="Articles per outlet, by date")+
  scale_colour_viridis(discrete=F,option="D")+
  xlab("")+
  ylab("")+
  theme(panel.grid.major.x = element_line("white"),
        panel.grid.major.y = element_line("white"),
        panel.grid = element_line("white"),axis.text.x = element_blank(),legend.position = "bottom")+
  facet_grid(.~keyword)



substack %>% 
  filter(region=="all regions" | is.na(region)==TRUE) %>%
  filter(year!=2026)%>%
  mutate(type=
           case_when(
             str_detect(EntryURL,"/post/")==TRUE ~ "post",
             !str_detect(EntryURL,"/post/")==TRUE & str_detect(EntryURL,"/p/[a-zA-Z0-9_-]+") ~ "article"
           )) %>% 
  filter(!is.na(type))%>%
  ggplot()+
  labs(title = "\nSubstack entries in Google News index by search term, region: all",
       subtitle = "jessk.org/blog")+
  geom_bar(aes(x=year,group=type,fill=type),position = position_dodge(preserve = "single")) -> p


p+theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
      panel.background = element_rect("black"),legend.background = element_rect("black"),
      legend.box.background = element_rect("black"),legend.key = element_rect("black"),
      text = element_text(colour = "white"),
      legend.position = "bottom")+
  scale_fill_paletteer_d("yarrr::info")+
  facet_grid(.~keyword)

p+scale_fill_paletteer_d("yarrr::info")+
  ylab(label = "entries per keyword\n")+
  theme(plot.background=element_rect("white", colour = "white"),
        panel.grid = element_line("white"),  
        panel.background = element_rect("white"),
        legend.background = element_rect("white"),
        legend.box = element_blank(),
        legend.key = element_rect("white"),
        text = element_text(colour = "black"))+
  facet_grid(.~keyword)


substack_posts 
