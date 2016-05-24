count.visits <- function(dat, counts, colony, treatment, int) {
	poor <- dat$feeder[dat$event == "poor"]
	good <- dat$feeder[dat$event == "good"]
	best <- dat$feeder[dat$event == "best"]
	dat <- dat[4:dim(dat)[1],]
	sorted.dat <- dat[order(dat$minutes),]
	switch.time <- dat$minutes[dat$event == "switch"]
	before <- subset(sorted.dat, event == "start" & minutes < switch.time)
	before <- before[order(before$minutes, decreasing=T),]
	num.before <- nrow(before) - nrow(before) %% int
	for (i in seq(1, num.before-(int-1), int)) {
		visits <- c(0, 0, 0)
		for (j in seq(i, i+(int-1))) {
			if (before$feeder[j] == poor) {
				visits[1] <- visits[1] + 1
			}
			else if (before$feeder[j] == good) {
				visits[2] <- visits[2] + 1
			}
			else if (before$feeder[j] == best) {
				visits[3] <- visits[3] + 1
			}
		}
		new.row <- c(treatment, "before", -i, "poor", colony, visits[1])
		counts <- rbind(counts, new.row)
		new.row <- c(treatment, "before", -i, "good", colony, visits[2])
		counts <- rbind(counts, new.row)
		new.row <- c(treatment, "before", -i, "best", colony, visits[3])
		counts <- rbind(counts, new.row)
	}
	
	after <- subset(sorted.dat, event == "start" & minutes > switch.time)
	num.after <- nrow(after) - nrow(after) %% int
	for (i in seq(1, num.after-(int-1), int)) {
		visits <- c(0, 0, 0)
		for (j in seq(i, i+(int-1))) {
			if (after$feeder[j] == poor) {
				visits[1] <- visits[1] + 1
			}
			else if (after$feeder[j] == good) {
				visits[2] <- visits[2] + 1
			}
			else if (after$feeder[j] == best) {
				visits[3] <- visits[3] + 1
			}
		}
		new.row <- c(treatment, "after", i, "poor", colony, visits[1])
		counts <- rbind(counts, new.row)
		new.row <- c(treatment, "after", i, "good", colony, visits[2])
		counts <- rbind(counts, new.row)
		new.row <- c(treatment, "after", i, "best", colony, visits[3])
		counts <- rbind(counts, new.row)
	}
	return(counts)
}

calc.visits <- function(dat, colony, treatment) {
	poor <- dat$feeder[dat$event == "poor"]
	good <- dat$feeder[dat$event == "good"]
	best <- dat$feeder[dat$event == "best"]
	dat <- dat[4:dim(dat)[1],]
	output <- data.frame(
		colony = rep(colony, 15),
		treatment = rep(treatment, 15),
		measure = c(rep("pre", 3), rep("pre_last_20", 3), rep("post", 3), rep("post_last_20a", 3), rep("post_last_20b", 3)),
		feeder = rep(c("poor", "good", "best"), 5),
		count = rep(0, 15)
	)
	switch.time <- dat$minutes[dat$event=="switch"]
	pre <- subset(dat, dat$minutes<switch.time & dat$event == "start")
	post <- subset(dat, dat$minutes>switch.time & dat$event == "start")
	pre <- pre[order(pre$minutes),]
	post <- post[order(post$minutes),]
	# Total number of visits before switch.
		tab <- table(pre$feeder)
		output$count[output$measure == "pre"] <- c(tab[as.character(poor)], tab[as.character(good)], tab[as.character(best)])
	# Last 20 visits before switch.
		high <- dim(pre)[1]
		low <- high-19
		tab <- table(pre$feeder[low:high])
		output$count[output $measure == "pre_last_20"] <- c(tab[as.character(poor)], tab[as.character(good)], tab[as.character(best)])
	# Total number of visits after switch.	
		tab <- table(post$feeder)
		output$count[output $measure == "post"] <- c(tab[as.character(poor)], tab[as.character(good)], tab[as.character(best)])
	# Last 20 visits after switch, after truncating to equalize visits before and after switch.
		tab <- table(post$feeder[low:high])
		output$count[output $measure == "post_last_20a"] <- c(tab[as.character(poor)], tab[as.character(good)], tab[as.character(best)])
	# Last 20 visits after switch.
		high <- dim(post)[1]
		low <- high-19
		tab <- table(post$feeder[low:high])
		output$count[output $measure == "post_last_20b"] <- c(tab[as.character(poor)], tab[as.character(good)], tab[as.character(best)])
	output
}

