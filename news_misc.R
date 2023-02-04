states <- c("Alabama",
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
            "Wyoming")

paste(paste(states,sep = " | ",collapse="|"))->state_string

ds[which(grepl(paste(paste(states,sep = " | ",collapse="|")),paste(ds$EntryContent,ds$EntryTitle))),] %>%
  mutate(in_state = )


# experimental NLP section, keywords used to further tag items

ds$tag_sports <- ifelse(grepl("(?i)sport|(?i)athletic|(?i)athlete|(?i)competition", ds$EntryContent),1,0)
ds$tag_leg    <- ifelse(grepl("(?i)bill|(?i)legislat",                              ds$EntryContent),1,0)
ds$tag_school <- ifelse(grepl("(?i)school|(?i)educat|(?i)universit",                ds$EntryContent),1,0)
ds$tag_sglsex <- ifelse(grepl("(?i)women\\sonly|(?i)single-sex|(?i)sex-based",      ds$EntryContent),1,0)


colIDs <- names(select(ds, contains("tag_")))

ds[which(ds$tag_leg==1),] %>% select(tag_leg, region,the_day) %>% ggplot()+
  geom_bar(aes(x=the_day,fill=region))


ds %>% mutate(timetime=as.POSIXct(thetime)) %>%
  ggplot()+
  geom_point(aes(x=the_day,y=timetime,color=topic))+
  facet_grid(keyword~region)


ds %>% tidyr::unite("tags",colIDs, sep = ",", remove = FALSE) %>% select(tags) %>% View()

# NLP using TM package

library(tm)

gsub("<b>|</b>|&nbsp;|;|&#39;|(|)|\\...|&quo","",paste(ds$EntryContent,ds$EntryTitle,collapse = " ")) -> x

as.data.frame(strsplit(x," ")) -> z
VCorpus(VectorSource(z)) -> zx
tm_map(zx,removeWords,stopwords(kind = "en")) -> z
findFreqTerms(term_matrix,lowfreq = 150)
TermDocumentMatrix(Corpus(VectorSource(x))) -> term_matrix
findAssocs(term_matrix, term="transgender",corlimit = 0.1)

# RSS directly

install.packages("tidyRSS")
library(tidyRSS)

tidyRSS::tidyfeed(feed = "https://www.google.com/alerts/feeds/02717371275706320887/1916116072231156194")


ds$EntryPublished


