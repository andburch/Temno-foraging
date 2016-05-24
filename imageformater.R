library("grid", lib.loc="Z:/Programs/R-3.1.1/library")



library("magrittr", lib.loc="Z:/Programs/R-3.2.2/library")

fixer1<-NULL

formarter <- function(filenames){
  
  for (i in filenames){
    fixer1<-NULL
    fixer1<-read.csv(paste("JWatcher data runs/",i,sep=""), skip=24, header=FALSE, col.names=c("time", "button"))
    fixer1<-data.frame(fixer1$time,substring(as.character.factor(fixer1$button),2,2),fixer1$time/1000*1/60)
    names(fixer1)<-c("time","button","min")
    fixer1<-check_os(fixer1)
    fixer1$start<-FALSE
    fixer1$feeder<-FALSE
    fixer1 <- fixer1 %>% ss_d %>% ss_i %>% ss_t
    View(fixer1)
    #fixer1$start[which(fixer1$button=="d")]<-"d"
    #fixer1$start<-fixer1$button=="d"
    fixer1<- fixer1 %>% make_pretty
    
    print(fixer1[which(fixer1$start!=FALSE),3:5])
    print(fixer1$button[1])
    write.csv(fixer1[which(fixer1$start!=FALSE),3:5], file=paste(gsub('.{4}$', '', i)," indvd.csv",sep=""))
  }
  }

ss_d <- function (df) {
  sum_d<-0;
  i<-1;
  while (i <= length(df$time)-1) {
    if (df$button[i]=="d"){
      sum_d<-sum_d+1
      if (sum_d%%2){  #YES if odd (the first)
        df$start[i]<-"start";
        if(df$button[i+1] %in% c("b","g","x","m")) {
          current_qual<-as.character(df$button[i+1]);
          df$feeder[i]<-current_qual
        } else {df$feeder[i]<-current_qual}
      } else {df$start[i]<-"stop";}
    }
    i<-i+1;
  }
  return(df)
}
ss_i <- function (df) {
  sum_d<-0;
  i<-1;
  while (i <= length(df$time)-1) {
    if (df$button[i]=="i"){
      sum_d<-sum_d+1
      if (sum_d%%2){  #YES if odd (the first)
        df$start[i]<-"start";
        df$feeder[i]<-"inside nest"
      } else {df$start[i]<-"stop";}
    } else if (df$button[i]=="s") {           #also mentions the switching event here
      df$start[i]<-"switch";
      df$feeder[i]<-"switch";
    }
    i<-i+1;
  }
  return(df)
}
ss_t <- function (df) {
  going<-FALSE;
  for (i in 1:length(df$time)) {
    if (df$button[i]=="t" & !going) {
      df$start[i]<-"start"
      df$feeder[i]<-"tandem run"
      going<-TRUE;
    } else if ((df$button[i]=="t" & going) | (df$button[i]=="c" & going))  {
      df$start[i]<-"stop"
      going<-FALSE;
      
    } else if ((df$button[i]=="s" & going) | (df$button[i]=="d" & going) | (df$button[i]=="l" & going) )  {
      going<-FALSE;
    }
    i<-i+1;
  }  
  return(df)
}
make_pretty <- function(df){
  if (!identical(df[(df$feeder=="b"),]$feeder, character(0))) df[which(df$feeder=="b"),]$feeder<-"bad";
  if (!identical(df[(df$feeder=="m"),]$feeder, character(0))) df[which(df$feeder=="m"),]$feeder<-"medium"
  if (!identical(df[(df$feeder=="g"),]$feeder, character(0))) df[which(df$feeder=="g"),]$feeder<-"bad (will be good)"
  if (!identical(df[(df$feeder=="x"),]$feeder, character(0))) df[which(df$feeder=="x"),]$feeder<-"really good"
  if (!identical(df[(df$feeder==FALSE),]$feeder, character(0))) df[which(df$feeder==FALSE),]$feeder<-""
  
  return(df)
  
}

timeconverter <- function(csv) {
  start<-csv$time[1];
  stp<-csv$time[length(csv$time)-1];
  total_time<-stp-start;
  unit<-1/total_time;
  #unit<-1/(total_time/2);
  
  newtime<-(csv$time-start)*unit;
  #newtime<-(csv$time-(total_time/2))*unit;
  newdf<-data.frame(newtime,substring(as.character.factor(csv$button),2,2))
  names(newdf)<-c("time","button")
  return(newdf)
}

check_os <- function(df) {
  x<-0;
  i<-1;
  while (i <= length(df$time)-1) {
      if (df$button[i+1]=="o") {
      df<-df[-i:-(i+1),];
      i<-i-2;
      x<-x+2;
          }
    i<-i+1;
  }
  row.names(df)<-1:length(df$time)
  print(x);
  return(df)
}

graph_feeding <- function(df,y) {
  going<-FALSE;
  for (i in 1:length(df$time)) {
    if (df$button[i]=="d" & !going) {
      x1<-df$time[i];
      going<-TRUE;
    } else if (df$button[i]=="d" & going){
      x2<-df$time[i];
      going<-FALSE;
      grid.lines(c(x1,x2), c(y,y), gp=gpar(col="green", lwd=5));
    }
    i<-i+1;
  }  
}

graph_inside <- function(df,y) {
  going<-FALSE;
  for (i in 1:length(df$time)) {
    if (df$button[i]=="i" & !going) {
      x1<-df$time[i];
      going<-TRUE;
    } else if (df$button[i]=="i" & going){
      x2<-df$time[i];
      going<-FALSE;
      grid.lines(c(x1,x2), c(y-.001,y-.001), gp=gpar(col="blue", lwd=5));
    }
    i<-i+1;
  }  
}

graph_tandem <- function(df,y) {
  going<-FALSE;
  for (i in 1:length(df$time)) {
    if (df$button[i]=="t" & !going) {
      x1<-df$time[i];
      going<-TRUE;
    } else if ((df$button[i]=="t" & going) | (df$button[i]=="s" & going) | (df$button[i]=="d" & going) | (df$button[i]=="l" & going) | (df$button[i]=="c" & going))  {
      x2<-df$time[i];
      going<-FALSE;
      grid.lines(c(x1,x2), c(y+.001,y+.001), gp=gpar(col="red", lwd=3));
    }
    i<-i+1;
  }  
}

graph_keys <- function(df,y,include=TRUE) {
  if(include) {
  for (i in 1:length(df$time)) {
    if(df$button[i]=="s"){         #this is just to get switching events
    grid.lines(c(df$time[i], df$time[i]), c(y-.02,y+.02), gp=gpar(col="black", lwd=.5, lty=3))
    #if (i %% 2) {grid.text(i, x=df$time[i], y=y+.025, gp=gpar(cex=.6))}
    i<-i+1;
    }
  }}
}

graph_quality <- function(df, y){
#   going<-FALSE;
#   for (i in 1:length(df$time)) {
#     if (df$button[i]=="b" & !going) {
#       x1<-df$time[i];
#       going<-TRUE;
#     } else if ((df$button[i]=="m" & going) | (df$button[i]=="g" & going) | (df$button[i]=="x" & going) | i==length(df$time))  {
#       x2<-df$time[i];
#       going<-FALSE;
#       grid.lines(c(x1,x2), c(y-.01,y-.01), gp=gpar(col="orange", lwd=1));
#     }
#     i<-i+1;
#   }  
  
    for (i in 1:length(df$time)) {
      if (df$button[i]=="b" | df$button[i]=="g") {
      grid.lines(c(df$time[i], df$time[i]), c(y-.02,y-.01), arrow=arrow(length=unit(0.13, "inches")), gp=gpar(col="brown", lwd=.5))
      } else if (df$button[i]=="m") {
        grid.lines(c(df$time[i], df$time[i]), c(y-.02,y-.01), arrow=arrow(length=unit(0.13, "inches")), gp=gpar(col="gray", lwd=.5))
      }
      else if (df$button[i]=="x") {
        grid.lines(c(df$time[i], df$time[i]), c(y-.02,y-.01), arrow=arrow(length=unit(0.13, "inches")), gp=gpar(col="gold", lwd=.5))
      }
      i<-i+1;
      
    }
}  


multiprint<- function(listnames,keys=TRUE){
  y<-.95;
  for (i in listnames){
    testdb1<-read.csv(paste("JWatcher data runs/",i,sep=""), skip=24, header=FALSE, col.names=c("time", "button"))
    total_time_hrs<-(testdb1$time[length(testdb1$time)]*1/1000*1/60*1/60)
    new<-timeconverter(testdb1);
    new<-check_os(new);
    
    #graphing stuff
    grid.text(paste(i,"  (",round(total_time_hrs, digits=2)," hrs)", sep=""), x=.5, y=y+.03, gp=gpar(cex=.7))
    grid.lines(c(.0,1), c(y,y), gp=gpar(col="gray", lwd=4))
    graph_feeding(new,y);
    graph_inside(new,y);
                ##grid.lines(c(df$time[which(new$button=="q")], df$time[which(new$button=="q")]), c(.85,.95), gp=gpar(col="gray"))
    graph_tandem(new,y)
    graph_quality(new,y)
    
    graph_keys(new,y,keys)
    y<-y-.07;
    print(i)
  }
  
}

frame();
multiprint(filenames)

multiprint(c("e46-1.dat"),FALSE)

#
#
#
#
#
#






filename<-"e11-1.dat"
testdb1<-read.csv(paste("JWatcher data runs/",filename,sep=""), skip=24, header=FALSE, col.names=c("time", "button"))
new<-timeconverter(testdb1);
new<-check_os(new);


grid.lines(c(.0,1), c(.90,.90), gp=gpar(col="gray", lwd=4))
graph_feeding(new);
graph_inside(new);
##grid.lines(c(df$time[which(new$button=="q")], df$time[which(new$button=="q")]), c(.85,.95), gp=gpar(col="gray"))
graph_tandem(new)

graph_keys(new)



grid.lines(c(.0,1), c(.90,.90), gp=gpar(col="gray", lwd=4))
grid.lines(c(timeconverter(testdb1)[69,1],timeconverter(testdb1)[70,1]), c(.90,.90), gp=gpar(col="green", lwd=4))