# frequency stats
# jessdkant.bsky.social


ds %>% filter(is.na(pullURL)==FALSE) %>%
  distinct(EntryURL, .keep_all = TRUE) %>%
  group_by(pullURL) %>%
   summarize(n=n()) %>% 
   arrange(by_group=desc(n)) %>%
   head(n=20) %>% select(pullURL) -> top_20

# top 10 / top 20

gettop <- function(){
  ds %>% filter(is.na(pullURL)==FALSE) %>%
    filter(!(pullURL %in% portals)) %>% filter(!(pullURL %in% socials)) %>% filter(!str_detect(pullURL,".co.uk|uk")) %>% 
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>% head(n=10) %>% select(pullURL) -> top_10_us
  as.vector(top_10_us$pullURL) -> top10us
  # as.list(top_10_us$pullURL) -> top_ten_us
  
  ds %>% filter(is.na(pullURL)==FALSE) %>%
    filter(!(pullURL %in% portals)) %>%filter(!(pullURL %in% socials)) %>%filter(str_detect(pullURL,".co.uk")) %>%
    distinct(EntryURL, .keep_all = TRUE) %>% group_by(pullURL) %>% summarize(n=n()) %>% 
    arrange(by_group=desc(n)) %>% head(n=10) %>% select(pullURL) -> top_10_uk
  as.vector(top_10_uk$pullURL) -> top10uk
  #as.list(top_10_uk$pullURL) -> top_ten_uk
}
