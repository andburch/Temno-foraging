
dynamic.hist <- function(dat, switch.time, end.time, name) {
  poor <- dat$feeder[dat$event == "poor"]
  good <- dat$feeder[dat$event == "good"]
  best <- dat$feeder[dat$event == "best"]
  dat <- dat[4:dim(dat)[1],]
  max.time <- max(dat$minutes)
  histo.poor <- hist(
    dat$minutes[dat$feeder==poor & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5),
    plot=F
  )
  histo.good <- hist(
    dat$minutes[dat$feeder==good & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5),
    plot=F
  )
  histo.best <- hist(
    dat$minutes[dat$feeder==best & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5),
    plot=F
  )
  max.visits <- max(histo.poor$counts, histo.good$counts, histo.best$counts) + 2
  
  font.size=1
  hist(
    dat$minutes[dat$feeder==poor & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5),
    cex.main=font.size,
    cex.lab=font.size,
    cex.axis=font.size,
    ylim=c(0, max.visits), 
    xlab="Minutes since start", 
    ylab="Visits", 
    main="Poor feeder (0.01M)"
  )
  tr.poor <- subset(dat, event=="tandem" & feeder==poor)
  if (nrow(tr.poor) > 0) {
    tr.poor$tr <- max.visits
    points(tr.poor$tr ~ tr.poor$minutes, pch=19, col="red")
  }
  hist(dat$minutes[dat$feeder==good & dat$event=="start"], 
       breaks=seq(0, max.time+5, 5), 
       cex.main=font.size,
       cex.lab=font.size,
       cex.axis=font.size,
       ylim=c(0, max.visits), 
       xlab="Minutes since start", 
       ylab="Visits", 
       main="Good feeder (0.1M)"
  )
  tr.good <- subset(dat, event=="tandem" & feeder==good)
  if (nrow(tr.good) > 0) {
    tr.good$tr <- max.visits
    points(tr.good$tr ~ tr.good$minutes, pch=19, col="red")
  }
  hist(
    dat$minutes[dat$feeder==best & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5), 
    cex.main=font.size,
    cex.lab=font.size,
    cex.axis=font.size,
    ylim=c(0, max.visits), 
    xlab="Minutes since start", 
    ylab="Visits", 
    main="Changing feeder (0.01M to 1.0M)"
  )
  tr.best <- subset(dat, event=="tandem" & feeder==best)
  if (nrow(tr.best) > 0) {
    tr.best$tr <- max.visits
    points(tr.best$tr ~ tr.best$minutes, pch=19, col="red")
  }
  points(x=switch.time, y=1, pch=18, cex=2, col="blue")
  points(x=end.time, y=1, pch=18, cex=2, col="red")
}

static.hist <- function(dat, switch.time, end.time, name) {
  poor <- dat$feeder[dat$event == "poor"]
  good <- dat$feeder[dat$event == "good"]
  best <- dat$feeder[dat$event == "best"]
  dat <- dat[4:dim(dat)[1],]
  windows()
  par(mfrow=c(3,1))
  max.time <- max(dat$minutes)
  histo.poor <- hist(
    dat$minutes[dat$feeder==poor & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5),
    plot=F
  )
  histo.good <- hist(
    dat$minutes[dat$feeder==good & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5),
    plot=F
  )
  histo.best <- hist(
    dat$minutes[dat$feeder==best & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5),
    plot=F
  )
  max.visits <- max(histo.poor$counts, histo.good$counts, histo.best$counts) + 2
  font.size=1.5
  hist(
    dat$minutes[dat$feeder==poor & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5), 
    cex.main=font.size,
    cex.lab=font.size,
    cex.axis=font.size,
    ylim=c(0, max.visits), 
    xlab="Minutes since start", 
    ylab="Visits", 
    main="Poor feeder (0.01M)"
  )
  tr.poor <- subset(dat, event=="tandem" & feeder==poor)
  if (nrow(tr.poor) > 0) {
    tr.poor$tr <- max.visits
    points(tr.poor$tr ~ tr.poor$minutes, pch=19, col="red")
  }
  hist(dat$minutes[dat$feeder==good & dat$event=="start"], 
       breaks=seq(0, max.time+5, 5), 
       cex.main=font.size,
       cex.lab=font.size,
       cex.axis=font.size,
       ylim=c(0, max.visits), 
       xlab="Minutes since start", 
       ylab="Visits", 
       main="Good feeder (0.1M)"
  )
  tr.good <- subset(dat, event=="tandem" & feeder==good)
  if (nrow(tr.good) > 0) {
    tr.good$tr <- max.visits
    points(tr.good$tr ~ tr.good$minutes, pch=19, col="red")
  }
  hist(
    dat$minutes[dat$feeder==best & dat$event=="start"], 
    breaks=seq(0, max.time+5, 5), 
    cex.main=font.size,
    cex.lab=font.size,
    cex.axis=font.size,
    ylim=c(0, max.visits), 
    xlab="Minutes since start", 
    ylab="Visits", 
    main="Poor feeder (0.01M)"
  )
  tr.best <- subset(dat, event=="tandem" & feeder==best)
  if (nrow(tr.best) > 0) {
    tr.best$tr <- max.visits
    points(tr.best$tr ~ tr.best$minutes, pch=19, col="red")
  }
  points(x=switch.time, y=1, pch=18, cex=2, col="blue")
  points(x=end.time, y=1, pch=18, cex=2, col="red")
}

dynamic.density <- function(dat, switch.time, end.time, last.time, name) {
  poor <- dat$feeder[dat$event == "poor"]
  good <- dat$feeder[dat$event == "good"]
  best <- dat$feeder[dat$event == "best"]
  dat <- dat[4:dim(dat)[1],]
  windows()
  dens.poor <- density(subset(dat$minutes, dat$feeder==poor & dat$event=="start"), from=0, to=last.time)
  
  dens.good <- density(subset(dat$minutes, dat$feeder==good & dat$event=="start"), from=0, to=last.time)
  dens.best <- density(subset(dat$minutes, dat$feeder==best & dat$event=="start"), from=0, to=last.time)
  prop <- table(dat$feeder[dat$event=="start"])/length(dat$feeder[dat$event=="start"])
  dens.poor$y <- prop[poor]*dens.poor$y
  dens.good$y <- prop[good]*dens.good$y
  dens.best$y <- prop[best]*dens.best$y
  max.dens <- max(dens.poor$y, dens.good$y, dens.best$y)
  font.size=1.5
  plot(
    dens.poor$y ~ dens.poor$x, 
    type="l", 
    col="black",
    lty=2,
    cex.main=font.size,
    cex.lab=font.size,
    cex.axis=font.size,
    ylim=c(0, max.dens),
    xlab="Minutes since start",
    ylab="Probability density of visit",
    main=name
  )
  points(
    dens.good$y ~ dens.good$x, 
    type="l", 
    col="black",
    lty=1)
  points(
    dens.best$y[dens.best$x<switch.time] ~ dens.best$x[dens.best$x<switch.time], 
    type="l", 
    col="black",
    lty=2)
  points(
    dens.best$y[dens.best$x>=switch.time] ~ dens.best$x[dens.best$x>=switch.time], 
    type="l", 
    col="black",
    lty=1,
    lwd=2.5)
  legend(
    x="topleft",
    legend=c("poor", "good", "best"),
    lty=c(2,1,1),
    lwd=c(1,1,2.5),
    col="black"
  )
  points(x=switch.time, y=0, pch=18, cex=2, col="blue")
  points(x=end.time, y=0, pch=18, cex=2, col="red")
}

static.density <- function(dat, switch.time, end.time, last.time, name) {
  poor <- dat$feeder[dat$event == "poor"]
  good <- dat$feeder[dat$event == "good"]
  best <- dat$feeder[dat$event == "best"]
  dat <- dat[4:dim(dat)[1],]
  windows()
  dens.poor <- density(subset(dat$minutes, dat$feeder==poor & dat$event=="start"), from=0, to=last.time)
  dens.good <- density(subset(dat$minutes, dat$feeder==good & dat$event=="start"), from=0, to=last.time)
  dens.best <- density(subset(dat$minutes, dat$feeder==best & dat$event=="start"), from=0, to=last.time)
  prop <- table(dat$feeder[dat$event=="start"])/length(dat$feeder[dat$event=="start"])
  dens.poor$y <- prop[poor]*dens.poor$y
  dens.good$y <- prop[good]*dens.good$y
  dens.best$y <- prop[best]*dens.best$y
  max.dens <- max(dens.poor$y, dens.good$y, dens.best$y)
  font.size=1.5
  plot(
    dens.poor$y ~ dens.poor$x, 
    type="l", 
    col="black", 
    lty=2,
    cex.main=font.size,
    cex.lab=font.size,
    cex.axis=font.size,
    ylim=c(0, max.dens),
    xlab="Minutes since start",
    ylab="Probability density of visit",
    main=name
  )
  points(
    dens.good$y ~ dens.good$x, 
    type="l", 
    col="black",
    lty=1)
  points(
    dens.best$y ~ dens.best$x, 
    type="l", 
    col="black",
    lty=2)
  legend(
    x="topleft",
    legend=c("poor", "good"),
    lty=c(2,1),
    lwd=c(1,1),
    col="black"
  )
  points(x=dat$minutes[dat$event=="switch"], y=0, pch=18, cex=2, col="blue")
  points(x=end.time, y=0, pch=18, cex=2, col="red")
}

calc.milestones <- function(dat) {
  switch.time <- dat$minutes[dat$event=="switch"]
  pre.visits <- length(dat$event[dat$event=="start" & dat$minutes<switch.time])
  post.visits <- length(dat$event[dat$event=="start" & dat$minutes>switch.time])
  starts <- subset(dat, event=="start")
  starts <- starts[order(starts$minutes),]
  end.time <- starts$minutes[2*pre.visits]
  list(switch.time, end.time, pre.visits)
}

dynamic.den2 <- function(dat, switch.time, end.time, last.time, name) {
  require(bde)
  poor <- dat$feeder[dat$event == "poor"]
  good <- dat$feeder[dat$event == "good"]
  best <- dat$feeder[dat$event == "best"]
  dat <- dat[4:dim(dat)[1],]
  #windows()
  print(subset(dat$minutes, dat$feeder==poor & dat$event=="start"), lbound=0, ubound=last.time)
  #   dens.poor <- logspline(subset(dat$minutes, dat$feeder==poor & dat$event=="start"), lbound=0, ubound=last.time)
  #   
  #   dens.good <- logspline(subset(dat$minutes, dat$feeder==good & dat$event=="start"), lbound=0, ubound=last.time)
  #   dens.best <- logspline(subset(dat$minutes, dat$feeder==best & dat$event=="start"), lbound=0, ubound=last.time)
  
  prop <- table(dat$feeder[dat$event=="start"])/length(dat$feeder[dat$event=="start"])

  font.size=1.5
  ytop<-0.01;
  plot(bde((subset(dat$minutes, dat$feeder==poor & dat$event=="start")), lower.limit=0, upper.limit = last.time, estimator="boundarykernel"), ylim=c(0,ytop/prop[poor]), xlim=c(0,last.time), col="brown", lwd=2, xlab="Minutes since start", main=name)
  
  par(new=T)
  
  plot(bde((subset(dat$minutes, dat$feeder==good & dat$event=="start")), lower.limit=0, upper.limit = last.time, estimator="boundarykernel"), ylim=c(0,ytop/prop[good]), xlim=c(0,last.time), col="gray", lwd=2, xlab="Minutes since start", main=name)
  par(new=T)
  
  plot(bde((subset(dat$minutes, dat$feeder==best & dat$event=="start")), lower.limit=0, upper.limit = last.time, estimator="boundarykernel"), ylim=c(0,ytop/prop[best]), xlim=c(0,last.time), col="gold", lwd=2, xlab="Minutes since start", main=name)
  par(new=T)
  
  tr.good <- subset(dat, event=="tandem" & feeder==good)
  if (nrow(tr.good) > 0) {
    points(tr.good$minutes,y=rep(0.0025, times=length(tr.good$minutes)), bg="gray", pch=23, cex=1.75)
  }
  
  
  tr.best <- subset(dat, event=="tandem" & feeder==best)
  if (nrow(tr.best) > 0) {
    points(tr.best$minutes,y=rep(0.005, times=length(tr.best$minutes)), bg="gold", pch=23, cex=1.75)
  }
  tr.poor <- subset(dat, event=="tandem" & feeder==poor)
  if (nrow(tr.poor) > 0) {
    points(tr.poor$minutes,y=rep(0, times=length(tr.poor$minutes)), bg="brown", pch=23, cex=1.75)
  }
  
  
  abline(v=switch.time, col="blue")
  abline(v=end.time, col="red")
}

# uhhh, this one is actually for calculating success or something
dynamic.den3 <- function(dat, switch.time, end.time, last.time, name) {
  require(bde)
  poor <- dat$feeder[dat$event == "poor"]
  good <- dat$feeder[dat$event == "good"]
  best <- dat$feeder[dat$event == "best"]
  dat <- dat[4:dim(dat)[1],]
  #windows()
  print(subset(dat$minutes, dat$feeder==poor & dat$event=="start"), lbound=0, ubound=last.time)
  #   dens.poor <- logspline(subset(dat$minutes, dat$feeder==poor & dat$event=="start"), lbound=0, ubound=last.time)
  #   
  #   dens.good <- logspline(subset(dat$minutes, dat$feeder==good & dat$event=="start"), lbound=0, ubound=last.time)
  #   dens.best <- logspline(subset(dat$minutes, dat$feeder==best & dat$event=="start"), lbound=0, ubound=last.time)
  
  prop <- table(dat$feeder[dat$event=="start"])/length(dat$feeder[dat$event=="start"])
  
  
  tr.poor <- subset(dat, event=="tandem" & feeder==poor)
  if (nrow(tr.poor) > 0) {
    points(tr.poor$minutes,y=rep(0, times=length(tr.poor$minutes)), bg="brown", pch=23, cex=1.75)
  }
  
  
  total.pre = dim(subset(dat, event=="tandem" & minutes<switch.time))[1]
  
  pre.good = dim(subset(dat, event=="tandem" & feeder==good & minutes<switch.time))[1] / total.pre
  pre.eh =  dim(subset(dat, event=="tandem" & (feeder==poor | feeder==best)  & minutes<switch.time))[1] / total.pre
  pre=c(pre.eh, pre.good)
  names(pre)<-c("eh","good")
  
  
  total.mid = dim(subset(dat, event=="tandem" & minutes<end.time & minutes>switch.time))[1]
  mid.good = dim(subset(dat, event=="tandem" & feeder==good & minutes<end.time & minutes>switch.time))[1] / total.mid
  mid.best =  dim(subset(dat, event=="tandem" & feeder==best  & minutes<end.time & minutes>switch.time))[1] / total.mid
  mid.poor =  dim(subset(dat, event=="tandem" & feeder==poor  & minutes<end.time & minutes>switch.time))[1] / total.mid
  mid=c(mid.poor, mid.good, mid.best)
  names(mid)<-c("poor","good","best")
  
  total.end = dim(subset(dat, event=="tandem" & minutes>end.time))[1]
  end.good = dim(subset(dat, event=="tandem" & feeder==good & minutes>end.time))[1] / total.end
  end.best =  dim(subset(dat, event=="tandem" & feeder==best & minutes>end.time))[1] / total.end
  end.poor =  dim(subset(dat, event=="tandem" & feeder==poor  & minutes>end.time))[1] / total.end
  end=c(end.poor, end.good, end.best)
  names(end)<-c("poor","good","best")
  print(pre)
  print("------------------")
  print(mid)
  print("------------------")
  
  print(end)
  print("------------------")
  #noquote(paste(round(as.numeric(c(pre,mid,end)), digits=4), sep="\t"))
}

cumulative.regret <- function(dat, switch.time, end.time, last.time, name) {
  milestones <- calc.milestones(dat)
  pre.visits <- milestones[3]
  v.p =0.001
  v.g = 0.01
  v.b = 0.1
  poor <- dat$feeder[dat$event == "poor"]
  good <- dat$feeder[dat$event == "good"]
  best <- dat$feeder[dat$event == "best"]
  dat <- dat[4:dim(dat)[1],]
  #I need to remove tandem runs
  #
  dat_clean<-NULL
  
  dat_clean<-subset(dat[,1:4], event!="tandem" & event!="switch")
  dat_clean =dat_clean[with(dat_clean, order(minutes)),]
  rownames(dat_clean) <- 1:nrow(dat_clean)
  dat_clean[which(dat_clean$feeder==poor & dat_clean$minutes<switch.time),5]<-v.g-v.p
  dat_clean[which(dat_clean$feeder==good & dat_clean$minutes<switch.time),5]<-v.g-v.g
  dat_clean[which(dat_clean$feeder==best & dat_clean$minutes<switch.time),5]<-v.g-v.p
  
  dat_clean[which(dat_clean$feeder==poor & dat_clean$minutes>switch.time),5]<-v.b-v.p
  dat_clean[which(dat_clean$feeder==good & dat_clean$minutes>switch.time),5]<-v.b-v.g
  dat_clean[which(dat_clean$feeder==best & dat_clean$minutes>switch.time),5]<-v.b-v.b
  colnames(dat_clean)[5]<-"r"
  
  dat_clean$r=as.numeric(dat_clean$r)
  dat_clean$cum_r=0
  
  for(i in 1:nrow(dat_clean)) {
    i
    if(i==1) {dat_clean$cum_r[i]<-dat_clean$r[i]} 
    else {
      dat_clean$cum_r[i] <- dat_clean$r[i]+dat_clean$cum_r[i-1]
    }
  }
  
  
  p1<-ggplot(dat_clean[1:pre.visits[[1]],], aes(1:pre.visits[[1]],cum_r)) +
    geom_line(colour = "red")
  p2<-ggplot(dat_clean[pre.visits[[1]]:nrow(dat_clean),], aes(pre.visits[[1]]:nrow(dat_clean),cum_r)) +
    geom_line(colour = "red")
  plot(p1)
  plot(p2)
}



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}