# fixed TLD grabber
# jessdkant.bsky.social

read.csv("~/gayagenda/datasets/Jan2026.csv") -> ds

{
  # new pullURL string
  ds %>%
    mutate(no_w=str_remove(EntryURL,"www.")) %>%
    mutate(pullURL=
             str_remove(
               str_remove(
                 str_extract(
                   str_remove(EntryURL,"www."),
                   pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z]+/" ),
                 "https:\\/\\/"),"/")) %>%
    select(EntryURL,pullURL) %>% View()
}

# improved data cleaning

ds %>% 
  mutate(the_day=as.Date(mdy(str_extract(
    EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")))) %>%
  mutate(month=month(lubridate::as_date(the_day))) %>% 
  mutate(year = year(lubridate::as_date(the_day))) %>%
  mutate(quarter = quarter(lubridate::as_date(the_day))) %>%
  mutate(pullURL=
           str_remove(
             str_remove(
               str_extract(
                 str_remove(EntryURL,"www."),
                 pattern="http?s:\\/\\/[a-z0-9A-Z]+[a-z0-9A-Z.-]+/" ),
               "http?s:\\/\\/"),"/")) -> ds


  #  group_by(pullURL) %>%
  #  mutate(n=n()) %>%
  #  ungroup(pullURL) %>%
  #  select(pullURL,EntryURL,n) -> ds


  ds$pullURL[which(str_detect(ds$EntryURL,"yahoo"))] <- "yahoo.com"
  
  #  ds[which(ds$pullURL=="news.yahoo.com"),]      -> y_a ; dim(a)
  #  ds[which(ds$pullURL=="yahoo.com"),]           -> y_b ; dim(b)
  #  ds[which(ds$pullURL=="uk.yahoo.com"),]        -> y_c ; dim(c)
  #  ds[which(ds$pullURL=="uk.news.yahoo.com"),]   -> y_d ; dim(d)
  #  ds[which(ds$pullURL=="sports.yahoo.com"),]    -> y_e ; dim(e)
  #  dim(y_a)[1]+ dim(y_b)[1]+ dim(y_c)[1]+dim(y_d)[1]+dim(y_e)[1]
  
