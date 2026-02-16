# frequency stats
# jessdkant.bsky.social

     # start outlet comparison bar plots
     
     {
       
       mega_ds %>% filter(keyword!="gender confusion") %>% filter(year!="2026") -> mega_ds

       #  filter(pullURL %in% six_sources) %>%
       
       mega_ds %>%
         filter(pullURL=="www.cnn.com/") %>%
         ggplot()+
         geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
         facet_grid(.~year)+
         scale_fill_paletteer_d("yarrr::cars")+
         theme_dark()+
         ylab(label = "jessica kant")+xlab(label = " ")+
         labs(title = "cnn.com")+
         coord_cartesian(ylim = c(0,300))+
         theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
               panel.background = element_rect("black"),legend.background = element_rect("black"),
               legend.box.background = element_rect("black"),legend.key = element_rect("black"),
               text = element_text(colour = "white"),
               legend.position = "none") -> a
       
       mega_ds %>%
         filter(pullURL=="www.foxnews.com/") %>%
         ggplot()+
         geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
         facet_grid(.~year)+scale_fill_paletteer_d("yarrr::cars")+theme_dark()+
         labs(title = "foxnews.com")+xlab(label = "")+ylab(label = "")+
         coord_cartesian(ylim = c(0,300))+
         theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
               panel.background = element_rect("black"), legend.background = element_rect("black"),
               legend.box.background = element_rect("black"),legend.key = element_rect("black"),
               text = element_text(colour = "white"),
               legend.position = "right") -> c
       
       mega_ds %>%
         filter(pullURL=="www.nytimes.com/") %>%
         ggplot()+
         geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+
         facet_grid(.~year)+
         scale_fill_paletteer_d("yarrr::cars")+theme_dark()+
         labs(title = "nytimes.com")+xlab(label = "")+ylab(label = "")+
         coord_cartesian(ylim = c(0,300))+
         theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
               panel.background = element_rect("black"), legend.background = element_rect("black"),
               legend.box.background = element_rect("black"),legend.key = element_rect("black"),
               text = element_text(colour = "white"),
               legend.position = "none") -> b
       
       mega_ds %>%
         filter(pullURL=="www.advocate.com/") %>%
         ggplot()+
         geom_bar(aes(x=quarter,fill=keyword),position = position_dodge())+facet_grid(.~year)+
         scale_fill_paletteer_d("yarrr::cars")+theme_dark()+
         ylab(label = "")+xlab(label = "jessk.org/blog")+
         labs(title = "advocate.com")+
         coord_cartesian(ylim = c(0,300))+
         theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
               panel.background = element_rect("black"),legend.background = element_rect("black"),
               legend.box.background = element_rect("black"),legend.key = element_rect("black"),
               text = element_text(colour = "hotpink"), legend.position = "none", strip.text= element_text(colour="black"),
               strip.background = element_rect(fill="purple")) -> d
     }
     
    # build plots
      gridExtra::grid.arrange(a,c,widths=c(4,5.5))         -> cnn_to_foxnews
      gridExtra::grid.arrange(a,b,c,widths=c(4,4,5))       -> cnn_to_nyt_to_Fox
      gridExtra::grid.arrange(a,b,d,c,widths=c(4,4,4,5.5)) -> cnn_nyt_advocate_fox
     
   # youtube
   {
     ds %>% 
       distinct(EntryURL, .keep_all = TRUE) %>%
       mutate(month=month(lubridate::as_date(the_day))) %>% 
       mutate(year = year(lubridate::as_date(the_day))) %>% 
       filter(pullURL=="youtube.com") %>%
       filter(region!="UK") %>%
       group_by(year,month,keyword,region) %>%
       arrange(desc(the_day)) %>%
       mutate(n=n()) %>% 
       filter(year!="2026") %>%
       ggplot()+
       geom_line(aes(x=month,y=n,color=keyword))+
       geom_point(aes(x=month,y=n,color=keyword))+
       facet_grid(region~year)+
       ylab(label = "jessica kant")+
       scale_color_paletteer_d("NineteenEightyR::miami2")+
       theme_dark()+
       labs(title = "\nYouTube hits in Google News search results\njessk.org/blog\n")+
       theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
             panel.background = element_rect("black"),legend.background = element_rect("black"),
             legend.box.background = element_rect("black"),legend.key = element_rect("black"),
             text = element_text(colour = "white"),
             legend.position = "bottom") 
      }
   
   # substack
   {
     ds %>% 
       distinct(EntryURL, .keep_all = TRUE) %>%
       mutate(month=month(lubridate::as_date(the_day))) %>% 
       mutate(year = year(lubridate::as_date(the_day))) %>% 
       filter(pullURL=="substack.com") %>%
       filter(region=="all regions") %>%
       group_by(year,month,keyword,region) %>%
       arrange(desc(the_day)) %>%
       mutate(n=n()) %>% 
       filter(year!="2026") %>%
       ggplot()+
       geom_line(aes(x=month,y=n,color=keyword))+
       geom_point(aes(x=month,y=n,color=keyword))+
       facet_grid(region~year)+
       ylab(label = "jessica kant")+
       scale_color_paletteer_d("NineteenEightyR::miami2")+
       theme_dark()+
       labs(title = "\nYouTube hits in Google News search results\njessk.org/blog\n")+
       theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
             panel.background = element_rect("black"),legend.background = element_rect("black"),
             legend.box.background = element_rect("black"),legend.key = element_rect("black"),
             text = element_text(colour = "white"),
             legend.position = "bottom") 
   }
   

   c( "msn.com",
      "yahoo.com") -> portals
   
   c( "youtube.com",
      "x.com",
      "instagram.com",
      "substack",
      "facebook.com") ->socials
   
#png(filename = '~/gayagenda/plots/top_ten__uk_trans_keyword.png', width= 800, height=500,)
#dev.off()

cust_pal <- c("#F6F0D4FF")

{
   ds %>% 
       filter(year!="2026") %>%
       filter(pullURL %in% top10uk) %>%
       filter(region=="all regions") %>%
       filter(keyword=="transgender") %>%
       distinct(EntryURL, .keep_all = TRUE) %>%
       group_by(year,month,keyword,pullURL) %>%
       arrange(desc(the_day)) %>%
       mutate(n=n())%>%
       ungroup()%>%
       ggplot()+
       geom_line(aes(x=month,y=n,color=keyword))+
       geom_point(aes(x=month,y=n,color=keyword))+
       facet_grid(year~pullURL)+
       ylab(label = "jessica kant")+
       scale_color_manual(values = cust_pal)+
       theme_dark()+
       labs(title = "\nArticles added to index by search term in Google News search results\ntop 10 UK .co.uk outlets\tregion: \"all\"\n",
            subtitle = "jessk.org/blog")+
       theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
             panel.background = element_rect("black"),legend.background = element_rect("black"),
             legend.box.background = element_rect("black"),legend.key = element_rect("black"),
             text = element_text(colour = "white"),
             legend.position = "bottom") 

}

   {
   ds %>% 
     filter(year!="2026") %>%
     filter(pullURL %in% top10us) %>%
     filter(region=="all regions") %>%
     filter(keyword!="transgender") %>%
     distinct(EntryURL, .keep_all = TRUE) %>%
     group_by(year,month,keyword,pullURL) %>%
     arrange(desc(the_day)) %>%
     mutate(n=n())%>%
     ungroup()%>%
     ggplot()+
     geom_line(aes(x=month,y=n,color=keyword))+
     geom_point(aes(x=month,y=n,color=keyword))+
     facet_grid(year~pullURL)+
     ylab(label = "jessica kant")+
     scale_color_paletteer_d("yarrr::info")+
     theme_dark()+
     labs(title = "\nArticles added to index by search term in Google News search results\ntop 10 US .com outlets\tregion: \"all\"\n",
          subtitle = "jessk.org/blog")+
     theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
           panel.background = element_rect("black"),legend.background = element_rect("black"),
           legend.box.background = element_rect("black"),legend.key = element_rect("black"),
           text = element_text(colour = "white"),
           legend.position = "bottom") 
   
   }
   # socal media posts in news index
   
   ds %>% 
     filter(year!="2026") %>%
     filter(pullURL %in% socials) %>%
     filter(region=="all regions") %>%
     #     filter(keyword!="transgender") %>%
     distinct(EntryURL, .keep_all = TRUE) %>%
     group_by(year,month,keyword,pullURL) %>%
     arrange(desc(the_day)) %>%
     mutate(n=n())%>%
     ggplot()+
     geom_line(aes(x=month,y=n,color=keyword))+
     geom_point(aes(x=month,y=n,color=keyword))+
     facet_grid(year~pullURL)+
     ylab(label = "jessica kant")+
     scale_color_paletteer_d("yarrr::info")+
     #     scale_color_paletteer_d("nord::mountain_forms")+
     #     scale_color_paletteer_d("nord::silver_mine")+
     theme_dark()+
     labs(title = "\nArticles added to index by search term in Google News search results: top 10 outlets\njessk.org/blog\n")+
     theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
           panel.background = element_rect("black"),legend.background = element_rect("black"),
           legend.box.background = element_rect("black"),legend.key = element_rect("black"),
           text = element_text(colour = "white"),
           legend.position = "bottom") 
   
   # asdsad
   

   ds %>% 
     filter(year!="2026") %>%
     filter(pullURL %in% top10uk) %>%
     filter(region=="all regions") %>%
     distinct(EntryURL, .keep_all = TRUE) %>%
     group_by(year,quarter,pullURL) %>%
     mutate(ct=n()) %>%
     ungroup()%>%
     mutate(maxkw=max(ct)) %>%
     mutate(topkw=
              case_when(
                ct==maxkw ~ "0.5",
                ct!=maxkw ~ "0"
              ))%>%
        ggplot()+
     geom_line(aes(x=quarter,y=ct,alpha=topkw,color=as.factor(year)))+
     geom_point(aes(x=quarter,y=ct,color=as.factor(year)))+
     facet_grid(keyword~pullURL)+
     ylab(label = "jessica kant")+
     scale_color_paletteer_d("yarrr::info")+
     theme_dark()+
     labs(title = "\nArticles added to index by search term in Google News search results\ntop 10 outlets\tregion: \"all\"\n",
          subtitle = "jessk.org/blog")+
     theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
           panel.background = element_rect("black"),legend.background = element_rect("black"),
           legend.box.background = element_rect("black"),legend.key = element_rect("black"),
           text = element_text(colour = "white"),
           legend.position = "bottom") 

   
   ### plot for post 3
   
   
   
   
     mega_ds %>% filter(is.na(pullURL)==FALSE) %>%
       filter(!(pullURL %in% portals)) %>%
       filter(!(pullURL %in% socials)) %>%
       filter(region!="USA")
       distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
       arrange(by_group=desc(n)) %>% head(n=5) %>% select(pullURL) -> top_10_us
     as.vector(top_10_us$pullURL) -> top10all
     # as.list(top_10_us$pullURL) -> top_ten_us
   
   mega_ds %>% 
     filter(year!="2026") %>%
     filter(pullURL %in% top10uk) %>%
     filter(region=="all regions") %>%
     distinct(EntryURL, .keep_all = TRUE) %>%
     group_by(month,year,keyword,pullURL) %>%
     mutate(ct=n()) %>%
     ungroup()%>%
     group_by(month,year,pullURL) %>%
     mutate(maxkw=max(ct)) %>%
     mutate(topkw=case_when(
                ct==maxkw ~ 1,
                ct!=maxkw ~ 0.2)) %>% 
     ungroup() %>%
   ggplot()+
     geom_line(aes(x=month,
                   y=ct,
                   alpha=topkw,
                   color=keyword,
                   group=interaction(keyword,year)))+
     geom_point(aes(x=month,
                    y=ct,
                    alpha=topkw,
                    color=keyword,
                    group=interaction(keyword,year)))+
     scale_alpha_identity()+
     facet_grid(year~pullURL)+
     ylab(label = "jessica kant")+
     scale_color_paletteer_d("yarrr::info")+
     theme_dark()+
     labs(title = "\nArticles added to index by search term in Google News search results\n
          top 10 outlets\tregion: \"all\"\n",
          subtitle = "jessk.org/blog")+
     theme(plot.background=element_rect("black", colour = "black"),panel.grid = element_line("black"),  
           panel.background = element_rect("black"),legend.background = element_rect("black"),
           legend.box.background = element_rect("black"),legend.key = element_rect("black"),
           text = element_text(colour = "white"),
           legend.position = "bottom")
   
   