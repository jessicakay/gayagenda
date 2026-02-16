As I've talked about extensively in the documentation on the site, [here](https://jessk.org/blog/anatomy-of-a-moral-panic), [here](https://jessk.org/blog/hate-in-a-post-information-age), and [here](https://jessk.org/blog/architecture-of-an-outrage-factory), the data collected here was not initially collected with the intention of widespread dissemination. Rather, it began because I thought "wouldn't it be neat if..." which is how most things happen on the internet. As the project has grown, and I've been poked by colleauges to revisit it or recompile it, I've tried to take careful notes of what data is the most reliable, and what should be discarded.

A couple of observations on "noise" in the data:

* the data is served initially by Google Alerts, which are themselves always being refined and subject to changes in the platform's algorithm.
* at times, the settings for some of the alerts have come undone on their own due to errors in the software used to bridge Google -> RSS -> IFTTT.
* the regional settings appear to be fairly imperfect, and are at best unreliable.
  * for this reason, I restrict the majority of the digging I do in terms of frequency to "all regions" or non-regionally coded data.

When cleaned, data should appear as:

    , , year = 2023
    
                 keyword
    region        biological sex gender identity transgender
      all regions           9910           16048       17186
      UK                     435             140        3235
      USA                   8482           15747       16940
    
    , , year = 2024
    
                 keyword
    region        biological sex gender identity transgender
      all regions          11202           19549       19701
      UK                     657            2630        3328
      USA                   9487           18985       19645
    
    , , year = 2025
    
                 keyword
    region        biological sex gender identity transgender
      all regions          15408           19888       20510
      UK                    1992            2424        3946
      USA                  13609           19430       20296
    
    > 
