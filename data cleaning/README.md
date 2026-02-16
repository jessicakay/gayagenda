As I've talked about extensively in the documentation on the site, [here](https://jessk.org/blog/anatomy-of-a-moral-panic), [here](https://jessk.org/blog/hate-in-a-post-information-age), and [here](https://jessk.org/blog/architecture-of-an-outrage-factory), the data collected here was not initially collected with the intention of widespread dissemination. Rather, it began because I thought "wouldn't it be neat if..." which is how most things happen on the internet. As the project has grown, and I've been poked by colleauges to revisit it or recompile it, I've tried to take careful notes of what data is the most reliable, and what should be discarded.

A couple of observations on "noise" in the data:

* the data is served initially by Google Alerts, which are themselves always being refined and subject to changes in the platform's algorithm.

* at times, the settings for some of the alerts have come undone on their own due to errors in the software used to bridge platforms.

* the regional settings appear to be fairly imperfect, and are at best unreliable.

  * for this reason, I restrict the majority of the digging I do in terms of frequency to "all regions" or non-regionally coded data.

* the "pull URL" function has been fraught, and there are examples in the various scripts of manually recoding specific sites which have variations that are best seen in aggregate. For example:

      mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"yahoo"))] <- "yahoo.com"
        mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"twitter.com"))] <- "x.com"
        mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"thetimes.com"))] <- "thetimes.co.uk"
        mega_ds$pullURL[which(str_detect(mega_ds$EntryURL,"bbc.com"))] <- "bbc.co.uk"
