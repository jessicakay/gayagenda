{
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
}


(ds %>%
  mutate(in_state = 
           case_when(
             str_detect(ds$textcontent, pattern="^Alabama") == TRUE ~ "Alabama",
             str_detect(ds$textcontent, pattern="^Arizona") == TRUE ~ "Arizona",
             str_detect(ds$textcontent, pattern="^Arkansas") == TRUE ~ "Arkansas",
             str_detect(ds$textcontent, pattern="^California") == TRUE ~ "California",
             str_detect(ds$textcontent, pattern="^Colorado") == TRUE ~ "Colorado",
             str_detect(ds$textcontent, pattern="^Connecticut") == TRUE ~ "Connecticut",
             str_detect(ds$textcontent, pattern="^DC") == TRUE ~ "DC",
             str_detect(ds$textcontent, pattern="^Delaware") == TRUE ~ "Delaware",
             str_detect(ds$textcontent, pattern="^Florida") == TRUE ~ "Florida",
             str_detect(ds$textcontent, pattern="^Georgia") == TRUE ~ "Georgia",
             str_detect(ds$textcontent, pattern="^Hawaii") == TRUE ~ "Hawaii",
             str_detect(ds$textcontent, pattern="^Idaho") == TRUE ~ "Idaho",
             str_detect(ds$textcontent, pattern="^Illinois") == TRUE ~ "Illinois",
             str_detect(ds$textcontent, pattern="^Indiana") == TRUE ~ "Indiana",
             str_detect(ds$textcontent, pattern="^Iowa") == TRUE ~ "Iowa",
             str_detect(ds$textcontent, pattern="^Kansas") == TRUE ~ "Kansas",
             str_detect(ds$textcontent, pattern="^Kentucky") == TRUE ~ "Kentucky",
             str_detect(ds$textcontent, pattern="^Lousiana") == TRUE ~ "Lousiana",
             str_detect(ds$textcontent, pattern="^Maine") == TRUE ~ "Maine",
             str_detect(ds$textcontent, pattern="^Maryland") == TRUE ~ "Maryland",
             str_detect(ds$textcontent, pattern="^Massachusetts") == TRUE ~ "Massachusetts",
             str_detect(ds$textcontent, pattern="^Michigan") == TRUE ~ "Michigan",
             str_detect(ds$textcontent, pattern="^Minnesota") == TRUE ~ "Minnesota",
             str_detect(ds$textcontent, pattern="^Mississippi") == TRUE ~ "Mississippi",
             str_detect(ds$textcontent, pattern="^Montana") == TRUE ~ "Montana",
             str_detect(ds$textcontent, pattern="^Nebraska") == TRUE ~ "Nebraska",
             str_detect(ds$textcontent, pattern="^Nevada") == TRUE ~ "Nevada",
             str_detect(ds$textcontent, pattern="^New Hampshire") == TRUE ~ "New Hampshire",
             str_detect(ds$textcontent, pattern="^New Jersey") == TRUE ~ "New Jersey",
             str_detect(ds$textcontent, pattern="^New Mexico") == TRUE ~ "New Mexico",
             str_detect(ds$textcontent, pattern="^New York") == TRUE ~ "New York",
             str_detect(ds$textcontent, pattern="^North Carolina") == TRUE ~ "North Carolina",
             str_detect(ds$textcontent, pattern="^North Dakota") == TRUE ~ "North Dakota",
             str_detect(ds$textcontent, pattern="^Ohio") == TRUE ~ "Ohio",
             str_detect(ds$textcontent, pattern="^Oklahoma") == TRUE ~ "Oklahoma",
             str_detect(ds$textcontent, pattern="^Oregon") == TRUE ~ "Oregon",
             str_detect(ds$textcontent, pattern="^Pennsylvania") == TRUE ~ "Pennsylvania",
             str_detect(ds$textcontent, pattern="^Puerto Rico") == TRUE ~ "Puerto Rico",
             str_detect(ds$textcontent, pattern="^Rhode Island") == TRUE ~ "Rhode Island",
             str_detect(ds$textcontent, pattern="^South Carolina") == TRUE ~ "South Carolina",
             str_detect(ds$textcontent, pattern="^South Dakota") == TRUE ~ "South Dakota",
             str_detect(ds$textcontent, pattern="^Tennessee") == TRUE ~ "Tennessee",
             str_detect(ds$textcontent, pattern="^Texas") == TRUE ~ "Texas",
             str_detect(ds$textcontent, pattern="^Utah") == TRUE ~ "Utah",
             str_detect(ds$textcontent, pattern="^Vermont") == TRUE ~ "Vermont",
             str_detect(ds$textcontent, pattern="^Virginia") == TRUE ~ "Virginia",
             str_detect(ds$textcontent, pattern="^Washington") == TRUE ~ "Washington",
             str_detect(ds$textcontent, pattern="^West Virginia") == TRUE ~ "West Virginia",
             str_detect(ds$textcontent, pattern="^Wisconsin") == TRUE ~ "Wisconsin",
             str_detect(ds$textcontent, pattern="^Wyoming") == TRUE ~ "Wyoming"
           )))[which(grepl(paste(paste(states,sep = " | ",collapse="|")),paste(ds$EntryContent,ds$EntryTitle))),]



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


