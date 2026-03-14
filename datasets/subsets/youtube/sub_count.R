yt_2023to2025 %>% distinct(yt_long_ID) -> channel_ID_list 

channel_ID_list <- as.vector(channel_ID_list$yt_long_ID)

subscriber_scrape <-NULL
i<-0 ; for (i in 1:length(channel_ID_list)){
  get_channel_stats(channel_ID_list[as.numeric(i)]) ->> x
  rbind(subscriber_scrape,
        c(channel_ID_list[i],
          x$snippet$customUrl,
          x$snippet$publishedAt,
          x$snippet$country,
          x$snippet$title,
          x$snippet$description,
          x$statistics$viewCount,
          x$statistics$subscriberCount,
          x$statistics$hiddenSubscriberCount,
          x$statistics$viewCount,
          x$statistics$videoCount))->> subscriber_scrape
          sample(5)[1]->sleepy ; Sys.sleep(sleepy) 
          } 

          subscriber_scrape |> 
            as.data.frame() |> 
            as_tibble()
          
          names(subscriber_scrape)[ 1]<-"channel_unique_ID"
          names(subscriber_scrape)[ 2]<-"publishedAt"
          names(subscriber_scrape)[ 3]<-"country"
          names(subscriber_scrape)[ 4]<-"title"
          names(subscriber_scrape)[ 5]<-"description"
          names(subscriber_scrape)[ 6]<-"view_count"
          names(subscriber_scrape)[ 7]<-"subscriber_count"
          names(subscriber_scrape)[ 8]<-"hiddenSubscriberCount"
          names(subscriber_scrape)[ 9]<-"viewCount"
          names(subscriber_scrape)[10]<-"videoCount"
          
          
          
          
          