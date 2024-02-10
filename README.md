# gayagenda

Initially starting as a way to generate slides for scicomm presentations, this project quickly grew into a way to aggregate, analyze and visualize news coverage around trans people.

The core of these data are primarily generated from IFTTT applets that monitor RSS feeds for news items, adding them to google sheets. These R scripts use open source packages like googlesheets4 to create a direct link between Google Drive and the RStudio IDE. Visualizations rendered by ggplot2. NLP coming soon.

My hope is that while I continue my own day-to-day advocacy work and research, I can continue to develop tools that will help automate some of the more draining tasks involved in monitoring and responding to new threats in the landscape. 

Hope you like it.

_Jess_

### Follow its progress on Mastodon: 
_[tech.lgbt/@jessdkant](https://tech.lgbt/@jessdkant)_

==== 

Datasets uploaded: 

* the first year of data is UP! go to "datasets" to see them.
* note: one of the feeds disconnected over the course of the year leading to an incomplete dataset for the keyword “gender identity” set to UK regional settings in the primary dataset. The data before and after the outage remains in the dataset, but anyone doing regional analysis should exclude that keyword/region combination. The feed is back up and running, and every half year or so I’ll upload a new dataset.
