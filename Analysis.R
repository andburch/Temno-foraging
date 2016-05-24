setwd("Z:/Documents/Temno foraging")
setwd("Z:/Documents/Temno foraging/Ant data analysis")
source("Graphing functions.R")
source("Count functions.R")
library("magrittr", lib.loc="Z:/Programs/R-3.2.3/R-3.2.3v2/library")

par(ask=FALSE)
###################################################################################

# These are plots for each replicate, showing either a histogram of the number
# of visits at each feeder, or a density plot of the same data. The latter may
# have some display value, but the histograms are more precise. The blue diamond
# on each plot show the time of the switch. The red diamond showns when the number
# of visits after the switch equaled that before the switch. The red circles
# show tandem runs.

# Dynamic condition
{
#graphics.off()
dat <- read.csv("E8 dynamic.csv")
visits <- calc.visits(dat, poor=2, good=1, best=3)
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E8 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E8 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic")
cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")


#graphics.off()
dat <- read.csv("E14 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E14 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E14 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic"); cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")


#graphics.off()
dat <- read.csv("E19 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E19 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E19 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic"); cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")

#graphics.off()
dat <- read.csv("E20 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E20 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E20 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic")
cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")

#graphics.off()
dat <- read.csv("E22 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E22 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E22 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic"); cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")

  #graphics.off()
  dat <- read.csv("E46 dynamic.csv")
  milestones <- calc.milestones(dat)
  switch.time <- milestones[1]
  end.time <- milestones[2]
  pre.visits <- milestones[3]
  pre.visits
  last.time <- max(dat$minutes)
  #dynamic.hist(dat, switch.time, end.time, "E46 dynamic")
  #dynamic.density(dat, switch.time, end.time, last.time, "E46 dynamic")
  dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic");
  cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")


#graphics.off()
dat <- read.csv("E48 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E48 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E48 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic");

cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")


#graphics.off()
dat <- read.csv("E49 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E49 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E49 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic"); cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")


#graphics.off()
dat <- read.csv("E57 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E57 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E57 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic"); cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")


#graphics.off()
dat <- read.csv("E65 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E65 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E65 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic"); cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")


#graphics.off()
dat <- read.csv("E68 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E68 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E68 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic"); cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")


#graphics.off()
dat <- read.csv("E69 dynamic.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
#dynamic.hist(dat, switch.time, end.time, "E69 dynamic")
#dynamic.density(dat, switch.time, end.time, last.time, "E69 dynamic")
dynamic.den2(dat, switch.time, end.time, last.time, "E14 dynamic"); cumulative.regret(dat, switch.time, end.time, last.time, "E14 dynamic")

}


# Static condition
{
#graphics.off()
dat <- read.csv("E8 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E8 static")
static.density(dat, switch.time, end.time, last.time, "E8 static")

#graphics.off()
dat <- read.csv("E14 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E14 static")
static.density(dat, switch.time, end.time, last.time, "E14 static")

#graphics.off()
dat <- read.csv("E19 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E19 static")
static.density(dat, switch.time, end.time, last.time, "E19 static")

#graphics.off()
dat <- read.csv("E20 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E20 static")
static.density(dat, switch.time, end.time, last.time, "E20 static")

#graphics.off()
dat <- read.csv("E22 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E22 static")
static.density(dat, switch.time, end.time, last.time, "E22 static")

#graphics.off()
dat <- read.csv("E46 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E46 static")
static.density(dat, switch.time, end.time, last.time, "E46 static")

#graphics.off()
dat <- read.csv("E48 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E48 static")
static.density(dat, switch.time, end.time, last.time, "E48 static")

#graphics.off()
dat <- read.csv("E49 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E49 static")
static.density(dat, switch.time, end.time, last.time, "E49 static")

#graphics.off()
dat <- read.csv("E57 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E57 static")
static.density(dat, switch.time, end.time, last.time, "E57 static")

#graphics.off()
dat <- read.csv("E65 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E65 static")
static.density(dat, switch.time, end.time, last.time, "E65 static")

#graphics.off()
dat <- read.csv("E68 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E68 static")
static.density(dat, switch.time, end.time, last.time, "E68 static")

#graphics.off()
dat <- read.csv("E69 static.csv")
milestones <- calc.milestones(dat)
switch.time <- milestones[1]
end.time <- milestones[2]
pre.visits <- milestones[3]
pre.visits
last.time <- max(dat$minutes)
static.hist(dat, switch.time, end.time, "E69 static")
static.density(dat, switch.time, end.time, last.time, "E69 static")
}

###################################################################################

# This is a (not very successful) effort to display all of the colony
# replicates on the same scale. I use feeder visits as the time scale, 
# with an interval of 5 visits. For each interval, the code determines how many
# of the 5 visits were at each feeder, and then plots the average for each
# feeder over all replicates.

# Dynamic condition
counts <- data.frame(
	treatment="dynamic", 
	phase="before", 
	sequence=0, 
	feeder=0, 
	colony="E0", 
	visits=-1,
	stringsAsFactors=F
)
interval <- 5
dat <- read.csv("E8 dynamic.csv")
counts <- count.visits(dat, counts, "E8", "dynamic", int=interval)
dat <- read.csv("E14 dynamic.csv")
counts <- count.visits(dat, counts, "E14", "dynamic", int=interval)
dat <- read.csv("E19 dynamic.csv")
counts <- count.visits(dat, counts, "E19", "dynamic", int=interval)
dat <- read.csv("E20 dynamic.csv")
counts <- count.visits(dat, counts, "E20", "dynamic", int=interval)
dat <- read.csv("E22 dynamic.csv")
counts <- count.visits(dat, counts, "E22", "dynamic", int=interval)
dat <- read.csv("E46 dynamic.csv")
counts <- count.visits(dat, counts, "E46", "dynamic", int=interval)
dat <- read.csv("E48 dynamic.csv")
counts <- count.visits(dat, counts, "E48", "dynamic", int=interval)
dat <- read.csv("E49 dynamic.csv")
counts <- count.visits(dat, counts, "E49", "dynamic", int=interval)
dat <- read.csv("E57 dynamic.csv")
counts <- count.visits(dat, counts, "E57", "dynamic", int=interval)
dat <- read.csv("E65 dynamic.csv")
counts <- count.visits(dat, counts, "E65", "dynamic", int=interval)
dat <- read.csv("E68 dynamic.csv")
counts <- count.visits(dat, counts, "E68", "dynamic", int=interval)
dat <- read.csv("E69 dynamic.csv")
counts <- count.visits(dat, counts, "E69", "dynamic", int=interval)
counts <- counts[-1,]
counts$visits <- as.numeric(counts$visits)
counts$sequence <- as.numeric(counts$sequence)
counts.best <- subset(counts, sequence > -51 & sequence < 120 & feeder=="best")
counts.good <- subset(counts, sequence > -51 & sequence < 120 & feeder=="good")
counts.poor <- subset(counts, sequence > -51 & sequence < 120 & feeder=="poor")
best <- with(counts.best, tapply(visits, sequence, mean, na.rm=T))
good <- with(counts.good, tapply(visits, sequence, mean, na.rm=T))
poor <- with(counts.poor, tapply(visits, sequence, mean, na.rm=T))
summary <- data.frame(sequence=as.numeric(names(best)), best=best, good=good, poor=poor)
plot(best~sequence, data=summary, type="l", ylim=c(0, interval), col="black", lty=2)
lines(best~sequence, data=subset(summary, sequence>0), type="l", col="gold", lty=1, lwd=2.5)
lines(good~sequence, data=summary, type="l", col="gray", lty=1)
lines(poor~sequence, data=summary, type="l", col="brown", lty=2)

# Static condition
counts <- data.frame(
	treatment="static", 
	phase="before", 
	sequence=0, 
	feeder=0, 
	colony="E0", 
	visits=-1,
	stringsAsFactors=F
)
interval <- 5
dat <- read.csv("E8 static.csv")
counts <- count.visits(dat, counts, "E8", "static", int=interval)
dat <- read.csv("E14 static.csv")
counts <- count.visits(dat, counts, "E14", "static", int=interval)
dat <- read.csv("E19 static.csv")
counts <- count.visits(dat, counts, "E19", "static", int=interval)
dat <- read.csv("E20 static.csv")
counts <- count.visits(dat, counts, "E20", "static", int=interval)
dat <- read.csv("E22 static.csv")
counts <- count.visits(dat, counts, "E22", "static", int=interval)
dat <- read.csv("E46 static.csv")
counts <- count.visits(dat, counts, "E46", "static", int=interval)
dat <- read.csv("E48 static.csv")
counts <- count.visits(dat, counts, "E48", "static", int=interval)
dat <- read.csv("E49 static.csv")
counts <- count.visits(dat, counts, "E49", "static", int=interval)
dat <- read.csv("E57 static.csv")
counts <- count.visits(dat, counts, "E57", "static", int=interval)
dat <- read.csv("E65 static.csv")
counts <- count.visits(dat, counts, "E65", "static", int=interval)
dat <- read.csv("E68 static.csv")
counts <- count.visits(dat, counts, "E68", "static", int=interval)
dat <- read.csv("E69 static.csv")
counts <- count.visits(dat, counts, "E69", "static", int=interval)
counts <- counts[-1,]
counts$visits <- as.numeric(counts$visits)
counts$sequence <- as.numeric(counts$sequence)
counts.best <- subset(counts, sequence > -51 & sequence < 100 & feeder=="best")
counts.good <- subset(counts, sequence > -51 & sequence < 100 & feeder=="good")
counts.poor <- subset(counts, sequence > -51 & sequence < 100 & feeder=="poor")
best <- with(counts.best, tapply(visits, sequence, mean, na.rm=T))
good <- with(counts.good, tapply(visits, sequence, mean, na.rm=T))
poor <- with(counts.poor, tapply(visits, sequence, mean, na.rm=T))
summary <- data.frame(sequence=as.numeric(names(best)), best=best, good=good, poor=poor)
plot(best~sequence, data=summary, type="l", ylim=c(0, interval), col="black", lty=2)
lines(best~sequence, data=subset(summary, sequence>0), type="l", col="black", lty=1, lwd=2.5)
lines(good~sequence, data=summary, type="l", col="black", lty=1)
lines(poor~sequence, data=summary, type="l", col="black", lty=2)


###################################################################################


# Calculate the number of visits to each feeder, before and after the switch.
	dat <- read.csv("E8 dynamic.csv")
	visits <- calc.visits(dat, "E8", "dynamic")
	dat <- read.csv("E14 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E14", "dynamic"))
	dat <- read.csv("E19 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E19", "dynamic"))
	dat <- read.csv("E20 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E20", "dynamic"))
	dat <- read.csv("E22 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E22", "dynamic"))
	dat <- read.csv("E46 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E46", "dynamic"))
	dat <- read.csv("E48 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E48", "dynamic"))
	dat <- read.csv("E49 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E49", "dynamic"))
	dat <- read.csv("E57 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E57", "dynamic"))
	dat <- read.csv("E65 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E65", "dynamic"))
	dat <- read.csv("E68 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E68", "dynamic"))
	dat <- read.csv("E69 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E69", "dynamic"))
	dat <- read.csv("E8 dynamic.csv")
	visits <- rbind(visits, calc.visits(dat, "E8", "static"))
	dat <- read.csv("E14 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E14", "static"))
	dat <- read.csv("E19 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E19", "static"))
	dat <- read.csv("E20 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E20", "static"))
	dat <- read.csv("E22 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E22", "static"))
	dat <- read.csv("E46 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E46", "static"))
	dat <- read.csv("E48 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E48", "static"))
	dat <- read.csv("E49 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E49", "static"))
	dat <- read.csv("E57 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E57", "static"))
	dat <- read.csv("E65 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E65", "static"))
	dat <- read.csv("E68 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E68", "static"))
	dat <- read.csv("E69 static.csv")
	visits <- rbind(visits, calc.visits(dat, "E69", "static"))

	# Change NAs to zeroes.
	visits$count[is.na(visits$count)] <- 0
	



