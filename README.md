# gayagenda


Initially starting as a way to generate slides for scicomm presentations, this project quickly grew into a way to aggregate, analyze and visualize news coverage around trans people.

The core of these data are primarily generated from IFTTT applets that monitor RSS feeds for news items, adding them to google sheets. These R scripts use open source packages like googlesheets4 to create a direct link between Google Drive and the RStudio IDE. Visualizations in ggplot2.

Hope you like it.

_Jess_

## analysis

[Anatomy of a moral panic](https://jessk.org/blog/anatomy-of-a-moral-panic)
[Hate in a post-information age](https://jessk.org/blog/hate-in-a-post-information-age)
[disinformation in the age of generative AI](https://jessk.org/blog/ai-and-disinformation)

### Follow its progress on Mastodon: 
_[tech.lgbt/@jessdkant](https://tech.lgbt/@jessdkant)_

### updates

February 2024: 

* the first year of data is UP! go to "datasets" to see them.
* note: one of the feeds disconnected over the course of the year leading to an incomplete dataset for the keyword “gender identity” set to UK regional settings in the primary dataset. The data before and after the outage remains in the dataset, but anyone doing regional analysis should exclude that keyword/region combination. The feed is back up and running, and every half year or so I’ll upload a new dataset.
* I have also included a set of keywords that were presumed (correctly it would seem) to lead to inflamatory news results. This comprises around 8 months of data and is also available.
* new dataviz is up in plots, now compatible with ggdark

Some _very_ preliminary analysis is available here [jessk.org/blog](https://jessk.org/blog/anatomy-of-a-moral-panic), more coming soon...
