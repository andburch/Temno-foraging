READ ME!! 
	-from Andrew Burchill (the guy who did this data)

WARNING:

So this is the "prettied-up" version of my data, made so the format resembles the colony-level records that Stephen has. Because I did all the formatting with my homemade R program, there could definitely be some bugs or problems. Let me know if you see anything too fishy.
--------------------------------

DIFFERENCES BETWEEN COLONY AND INDIVIDUAL:

Unlike the colony-level data, I have the starting and stopping times for the different events of interest. Additionally, "choices" are defined slightly differently here. If the focal ant drinks from any feeder, I count that as a choice. Another "choice event" occurs if 1) the ant feeds at a different feeder, 2) the ant enters, exits the nest and feeds again, 3) 10+ minutes go by and the ant feeds at the same feeder. I record 6 events, then switch one of the bad quality feeders to the best quality, then record 6 more events.
--------------------------------

HOW TO READ CSVs:

The "min" column has the time in minutes since the experiment started. 
The "start" column (perhaps it should be named better) indicates whether the recorded event started or stopped. 
The "feeder" column indicates what type of event occurred. This could be which quality feeder the ant was at (bad, bad-but-will-later-be-good, medium, or really good), whether it entered the nest, whether it started a tandem run, or when the qualities of food were switched. NOTE: tandem runs either have a "stop" event or are presumed to stop if another "start" event occurs (like drinking). All tandem runs leading to a drinking event were stopped immediately before drinking occurred. 
--------------------------------

HOW TO READ IMAGE:

I've also included my own visual representation of the trials. Each horizontal line represents the timeline for individual ant's foraging excursions after locating a feeder. Blue represents that the ant is inside of the nest, green is drinking, red is tandem-running. The switch event is indicated with a vertical black line, and arrows under feeding events indicate the quality of the food (bronze, silver, gold). If a feeding event has no arrow underneath, assume it is the quality of the most recent, previous arrow.
