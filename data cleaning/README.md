As I've talked about extensively in the documentation on the site, [here](https://jessk.org/blog/anatomy-of-a-moral-panic), [here](https://jessk.org/blog/hate-in-a-post-information-age), and [here](https://jessk.org/blog/architecture-of-an-outrage-factory), the data collected here was not initially collected with the intention of widespread dissemination. Rather, it began because I thought "wouldn't it be neat if..." which is how most things happen on the internet. As the project has grown, and I've been poked by colleauges to revisit it or recompile it, I've tried to take careful notes of what data is the most reliable, and what should be discarded.

A couple of observations on "noise" in the data:

* the data is served initially by Google Alerts, which are themselves always being refined and subject to changes in the platform's algorithm.

* at times, the settings for some of the alerts have come undone on their own due to errors in the software used to bridge platforms.

* the regional settings appear to be fairly imperfect, and are at best unreliable.

  * for this reason, I restrict the majority of the digging I do in terms of frequency to "all regions" or non-regionally coded data.

  * to get an idea of how this impacts the data itself:


        > table(mega_ds$region, mega_ds$keyword,exclude = F)

                    biological sex gender confusion gender identity gender ideology transgender transgenderism
        all regions          37435                0           56679               0       58703              0
        UK                    3145                0            5328               0       10735              0
        USA                  32406                0           55325               0       58137              0
        <NA>                     0             4471               0           46568           0          10746


  * as you can see, the NA values for $region only appear in the extended dataset ("exds" in the repo).

* the "pull URL" function has been fraught, and there are examples in the various scripts of manually recoding specific sites which have variations that are best seen in aggregate. For example:


      mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"yahoo"))] <- "yahoo.com"
        mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"twitter.com"))] <- "x.com"
        mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"thetimes.com"))] <- "thetimes.co.uk"
        mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"bbc.com"))] <- "bbc.co.uk"


* the best indicator of unique articles is done by looking at "EntryURL", the unsanitized URL passed from the Google Alert RSS feed to IFTTT and saved in a sheet. There are roughly 230k unique URLs in the dataset.


      [1] 181213      1
      > exds %>% distinct(EntryURL) %>% dim()
      [1] 60018     1
      > mega_ds %>% distinct(EntryURL) %>% dim()
      [1] 230037      1
      >


* this presents some challenges in keyword analysis, however, as can be seen here:


      > mega_ds %>% select(keyword, region) %>% table(exclude=F)
                        region
      keyword            all regions    UK   USA  <NA>
        biological sex         37435  3145 32406     0
        gender confusion           0     0     0  4471
        gender identity        56679  5328 55325     0
        gender ideology            0     0     0 46568
        transgender            58703 10735 58137     0
        transgenderism             0     0     0 10746
      >


* compared this to:


      > mega_ds %>% distinct(EntryURL, .keep_all=T)  %>% select(keyword, region) %>% table(exclude=F)
                        region
      keyword            all regions    UK   USA  <NA>
        biological sex         20032  1979 19931     0
        gender confusion           0     0     0  3449
        gender identity        26425  4422 33384     0
        gender ideology            0     0     0 36491
        transgender            28944  9696 36400     0
        transgenderism             0     0     0  8884

* overlapping entries are surprisingly not a huge part of the data. As can be seen by the 230k unique URLs above (compared to 380k observations )
* to check for yourself, the following will show you the raw counts and proportions

      > mega_ds %>% filter(region=="all regions") %>% distinct(EntryURL, .keep_all = T) %>% select(keyword) -> ds_only ; table(ds_only) ; round(prop.table(table(ds_only)),2)
      > mega_ds %>% filter(region=="all regions" | is.na(region)) %>% select(keyword) -> ds_only ; table(ds_only) ; round(prop.table(table(ds_only)),2)

      Should yield:

      biological sex gender identity     transgender
            34782           50510           52954
            0.25            0.37            0.38

      biological sex gender confusion  gender identity  gender ideology      transgender   transgenderism
            37435             4471            56679            46568            58703            10746
            0.17             0.02             0.26             0.22             0.27             0.05
