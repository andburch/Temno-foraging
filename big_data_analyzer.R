
#todo: 
#adapt these functions for ant-data
#   maybe first run the old function for each colony, and append the clean-data to the end of one big file (along with colony ID), then do analysis

{
  ant_dynamic <- read.csv("Z:/Documents/Temno foraging/Ant data analysis/ant_dynamic.csv", stringsAsFactors=FALSE)
  ant_dynamic$r=0
  ant_dynamic$cum_r="NA"
  
  v.p =0.001
  v.g = 0.01
  v.b = 0.1
  
  ant_dynamic$r[which( (ant_dynamic$feeder2=="poor" | ant_dynamic$feeder2=="best") & ant_dynamic$environment=="environment 1" & ant_dynamic$event=="start")]<-v.g-v.p
  
  ant_dynamic$r[which( (ant_dynamic$feeder2=="poor") & ant_dynamic$environment=="environment 2" & ant_dynamic$event=="start")]<-v.b-v.p
  ant_dynamic$r[which( (ant_dynamic$feeder2=="good") & ant_dynamic$environment=="environment 2" & ant_dynamic$event=="start")]<-v.b-v.g
  ant_dynamic$r[which(ant_dynamic$event!="start")]<-"NA"
  
  
  ant_dynamic$cum_r="NA"
  ant_dynamic$t_step="NA"
  ant_dynamic$r<-as.numeric(ant_dynamic$r)
  ant_dynamic$cum_r<-as.numeric(ant_dynamic$cum_r)
  ant_dynamic$t_step<-as.numeric(ant_dynamic$t_step)
  
  for (i in unique(ant_dynamic$groupID)){
    print(i)
    temp<-NULL
    temp<-subset(ant_dynamic, groupID==i & event=="start" & environment=="environment 1")
    
    for(j in 1:nrow(temp)) {
      if(j==1) {ant_dynamic$cum_r[as.numeric(rownames(temp[j,]))]<-ant_dynamic$r[as.numeric(rownames(temp[j,]))]; ant_dynamic$t_step[as.numeric(rownames(temp[j,]))]<-j} 
      else {
        ant_dynamic$cum_r[as.numeric(rownames(temp[j,]))] <- ant_dynamic$r[as.numeric(rownames(temp[j,]))]+ant_dynamic$cum_r[as.numeric(rownames(temp[j-1,]))]
        ant_dynamic$t_step[as.numeric(rownames(temp[j,]))]<-j
      }
    }
  }
  
  for (i in unique(ant_dynamic$groupID)){
    print(i)
    temp<-NULL
    temp<-subset(ant_dynamic, groupID==i & event=="start" & environment=="environment 2")
    
    for(j in 1:nrow(temp)) {
      if(j==1) {ant_dynamic$cum_r[as.numeric(rownames(temp[j,]))]<-ant_dynamic$r[as.numeric(rownames(temp[j,]))]; ant_dynamic$t_step[as.numeric(rownames(temp[j,]))]<-j} 
      else {
        ant_dynamic$cum_r[as.numeric(rownames(temp[j,]))] <- ant_dynamic$r[as.numeric(rownames(temp[j,]))]+ant_dynamic$cum_r[as.numeric(rownames(temp[j-1,]))]
        ant_dynamic$t_step[as.numeric(rownames(temp[j,]))]<-j
        
      }
    }
  }
  for(j in 1:nrow(ant_dynamic)) {
    if (ant_dynamic$event[j]=="tandem"){
      ant_dynamic$cum_r[j]<- -.03
    }
  }
  
  plot(subset(ant_dynamic$cum_r, ant_dynamic$environment=="environment 1"), ylab="Cuml. Regret", main="Dynamic Colony Cuml. Regret, Envmt. 1")
  plot(subset(ant_dynamic$cum_r, ant_dynamic$environment=="environment 2"), ylab="Cuml. Regret", main="Dynamic Colony Cuml. Regret, Envmt. 2")
}
#



#####################################################################

{
  human_dynamic <- read.csv("Z:/Documents/Temno foraging/Ant data analysis/human_dynamic.csv", stringsAsFactors=FALSE)
  human_dynamic$r=0
  human_dynamic$cum_r="NA"
  
  v.p =0.1
  v.g = 0.8
  v.b = 1.6
  
  human_dynamic$r[which( human_dynamic$performance=="P11" | human_dynamic$performance=="P12" )]<-v.g-v.p
  human_dynamic$r[which( human_dynamic$performance=="P2")]<-v.b-v.p
  human_dynamic$r[which( human_dynamic$performance=="G2" | human_dynamic$performance=="P12" )]<-v.b-v.g
  
  
  human_dynamic$r<-as.numeric(human_dynamic$r)
  human_dynamic$cum_r<-as.numeric(human_dynamic$cum_r)
  
  for (i in unique(human_dynamic$groupID)){
    print(i)
    temp<-NULL
    temp<-subset(human_dynamic, groupID==i)
    
    for(j in 1:nrow(temp)) {
      if(j==1) {human_dynamic$cum_r[as.numeric(rownames(temp[j,]))]<-human_dynamic$r[as.numeric(rownames(temp[j,]))]} 
      else {
        human_dynamic$cum_r[as.numeric(rownames(temp[j,]))] <- human_dynamic$r[as.numeric(rownames(temp[j,]))]+human_dynamic$cum_r[as.numeric(rownames(temp[j-1,]))]
      }
    }
  }
  plot(subset(human_dynamic$cum_r, human_dynamic$round<151 ), ylab="Cuml. Regret", main = "Human Dynamic Cuml Regret, Envmt. 1")
  plot(subset(human_dynamic$cum_r, human_dynamic$round>150 ), ylab="Cuml. Regret", main = "Human Dynamic Cuml Regret, Envmt. 2")
  
}




#####################################################################
######################

indvd_cum_r <- function(X){
  
  indvd_big <- read.csv("Z:/Documents/Temno foraging/Ant data analysis/indvd_cum_r.csv", stringsAsFactors=FALSE)
  indvd_big$r=0
  indvd_big$cum_r="NA"
  
  v.p =0.001
  v.g = 0.01
  v.b = 0.1
  
  indvd_big$r[which( (indvd_big$type=="p" | indvd_big$type=="x") & indvd_big$environment=="1")]<-v.g-v.p
  indvd_big$r[which( indvd_big$type=="p" & indvd_big$environment=="2")]<-v.b-v.p
  indvd_big$r[which( indvd_big$type=="g" & indvd_big$environment=="2")]<-v.b-v.g
  indvd_big$r[which( indvd_big$type=="t")]<-"NA"
  
  
  
  indvd_big$cum_r="NA"
  indvd_big$r<-as.numeric(indvd_big$r)
  indvd_big$cum_r<-as.numeric(indvd_big$cum_r)
  
  for (i in unique(indvd_big$groupID)){
    print(i)
    temp<-NULL
    temp<-subset(indvd_big, groupID==i & type!="t")
    
    for(j in 1:nrow(temp)) {
      if(j==1) {indvd_big$cum_r[as.numeric(rownames(temp[j,]))]<-indvd_big$r[as.numeric(rownames(temp[j,]))]} 
      else {
        indvd_big$cum_r[as.numeric(rownames(temp[j,]))] <- indvd_big$r[as.numeric(rownames(temp[j,]))]+indvd_big$cum_r[as.numeric(rownames(temp[j-1,]))]
      }
    }
  }
  
  for(j in 1:nrow(indvd_big)) {
    if (indvd_big$type[j]=="t"){
      indvd_big$cum_r[j]<- -.03
    }
  }
  
  plot(subset(indvd_big$cum_r, indvd_big$environment=="1" & indvd_big$type!="t"), ylab="Cuml. Regret", main="Dynamic Indvd Cuml. Regret, Envmt. 1")
  plot(subset(indvd_big$cum_r, indvd_big$environment=="2"), ylab="Cuml. Regret", main="Dynamic Colony Cuml. Regret, Envmt. 2")
}


###########################################

#plot regret curves in ggplot

p1<-ggplot(data=subset(ant_dynamic, event=="start" & environment=="environment 1"), aes(x=min_since, y=cum_r, group = groupID, colour = groupID)) +
  geom_line() + labs(title = "Cumulative Regret in First Environment (with minutes)") +
  xlab("Time in minutes") +ylab("Cumulative Regret")

p2<-ggplot(data=subset(ant_dynamic, event=="start" & environment=="environment 2" & visit2<60), aes(x=min_since, y=cum_r, group = groupID, colour = groupID)) +
  geom_line() + labs(title = "Cumulative Regret in 2nd Environment (with cutoff, in minutes)") +
  xlab("Time in Minutes") +ylab("Cumulative Regret")
 
multiplot(p1, p2, cols=2)

direct.label(p1, list(last.points, hjust = 0.7,
                     vjust = 1))
direct.label(p2, list(last.points, hjust = 0.7,
                      vjust = 1))


dyn_envmt1_cumr_visit


#####################################000000000000000000000000000###
#this is stuff about the curveness of the regret curve

test<-subset(ant_dynamic[,c(1,12)], ant_dynamic$environment=="environment 2" & ant_dynamic$groupID=="E48" & ant_dynamic$event=="start" & ant_dynamic$visit2<=60)
test$time<-1:nrow(test)

fm<-nls(cum_r ~ g1*(1-exp(time*-1*g2)), data=test, start=list(g1=1000, g2=-1), trace=TRUE)
g<-coef(fm)
plot(test)
curve(g[1]*(1-exp(x*g[2])), add=TRUE)

{
#actually start here
curveness<-data.frame(row.names = unique(ant_dynamic$groupID))

for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic[,c(1,12)], ant_dynamic$environment=="environment 1" & ant_dynamic$groupID==i & ant_dynamic$event=="start" & ant_dynamic$visit2<=70)
  test$time<-1:nrow(test)
  print(i)
    #fm<-nls(cum_r ~ g1*time/(g2+time), data=test, start=list(g1=1, g2=2), trace=TRUE)
  fm<-nls(cum_r ~ g1*(1-exp(time*-1*g2)), data=test, start=list(g1=1, g2=1), trace=TRUE)
  g<-coef(fm)
  plot(test)
  #curve(g[1]*x/(g[2]+x), add=TRUE, col="green")
  curve(g[1]*(1-exp(x*-1*g[2])), add=TRUE, col="red")
  
  curveness[i,1]<-g[1]
  curveness[i,2]<-g[2]
  
  test<-subset(ant_dynamic[,c(1,12)], ant_dynamic$environment=="environment 2" & ant_dynamic$groupID==i & ant_dynamic$event=="start" & ant_dynamic$visit2<=60)
  test$time<-1:nrow(test)
  
  #fm<-nls(cum_r ~ g1*time/(g2+time), data=test, start=list(g1=1, g2=2), trace=TRUE)
  try(fm<-nls(cum_r ~ g1*(1-exp(time*-1*g2)), data=test, start=list(g1=1, g2=1), trace=TRUE), silent=TRUE)
  g<-coef(fm)
  plot(test)
  #curve(g[1]*x/(g[2]+x), add=TRUE, col="green")
  curve(g[1]*(1-exp(x*-1*g[2])), add=TRUE, col="green")
 
  curveness[i,3]<-g[1]
  curveness[i,4]<-g[2]
}


#tallying tandem runs
for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem")
  curveness[i,5]<-dim(test)[1]
}
#tallying tandem runs2
for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic, environment=="environment 2" & groupID==i & event=="tandem")
  curveness[i,6]<-dim(test)[1]
}

#tallying GOOD tandem runs
for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem" & feeder2=="good")
  test2<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem" & (feeder2=="poor" | feeder2=="best"))
  curveness[i,7]<-dim(test)[1]-dim(test2)[1]
}
#tallying BEST tandem runs2
for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic, environment=="environment 2" & groupID==i & event=="tandem" & feeder2=="best")
  test2<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem" & (feeder2=="poor" | feeder2=="good"))
  curveness[i,8]<-dim(test)[1]-dim(test2)[1]
}

colnames(curveness)<-c("Vmax1","Km1","Vmax2","Km2", "tandems1","tandems2","goodtans1","goodtans2")




#curveness$Vmax1<-log(curveness$Vmax1)
#curveness$Km2<-log(curveness$Km2)
#curveness<-curveness[-7,]
#curveness<-curveness[-2,]

}




{
#testing with hyperbolic
curveness<-data.frame(row.names = unique(ant_dynamic$groupID))

for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic[,c(1,12)], ant_dynamic$environment=="environment 1" & ant_dynamic$groupID==i & ant_dynamic$event=="start" & ant_dynamic$visit2<=70)
  test$time<-1:nrow(test)
  print(i)
  fm<-nls(cum_r ~ g1*time/(g2+time), data=test, start=list(g1=1, g2=2), trace=TRUE)
  #fm<-nls(cum_r ~ g1*(1-exp(time*g2)), data=test, start=list(g1=1, g2=-1), trace=TRUE)
  g<-coef(fm)
  plot(test, main=i)
  curve(g[1]*x/(g[2]+x), add=TRUE, col="green")
  #curve(g[1]*(1-exp(x*g[2])), add=TRUE, col="red")
  
  curveness[i,1]<-g[1]
  curveness[i,2]<-g[2]
  
  test<-subset(ant_dynamic[,c(1,12)], ant_dynamic$environment=="environment 2" & ant_dynamic$groupID==i & ant_dynamic$event=="start" & ant_dynamic$visit2<=60)
  test$time<-1:nrow(test)
  
  try(fm<-nls(cum_r ~ g1*time/(g2+time), data=test, start=list(g1=1, g2=2), trace=TRUE), silent=TRUE)
  #try(fm<-nls(cum_r ~ g1*(1-exp(time*g2)), data=test, start=list(g1=1, g2=-1), trace=TRUE), silent=TRUE)
  g<-coef(fm)
  plot(test)
  curve(g[1]*x/(g[2]+x), add=TRUE, col="green")
  #curve(g[1]*(1-exp(x*g[2])), add=TRUE, col="green")
  
  curveness[i,3]<-g[1]
  curveness[i,4]<-g[2]
}


#tallying tandem runs
for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem")
  curveness[i,5]<-dim(test)[1]
}
#tallying tandem runs2
for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic, environment=="environment 2" & groupID==i & event=="tandem")
  curveness[i,6]<-dim(test)[1]
}


#tallying GOOD tandem runs
for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem" & feeder2=="good")
  test2<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem" & (feeder2=="poor" | feeder2=="best"))
  curveness[i,7]<-dim(test)[1]-dim(test2)[1]
}
#tallying BEST tandem runs2
for (i in unique(ant_dynamic$groupID)) {
  test<-subset(ant_dynamic, environment=="environment 2" & groupID==i & event=="tandem" & feeder2=="best")
  test2<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem" & (feeder2=="poor" | feeder2=="good"))
  curveness[i,8]<-dim(test)[1]-dim(test2)[1]
}
for (i in unique(ant_dynamic$groupID)) {
    curveness[i,9]<-curveness[i,8]-curveness[i,7]
}
colnames(curveness)<-c("Vmax1","Km1","Vmax2","Km2", "tandems1","tandems2","goodtans1","goodtans2")

#curveness<-curveness[-7,]
#curveness<-curveness[-2,]

}


p1<-ggplot(data=curveness, aes(log(Vmax1),log(Vmax2), label=rownames(curveness))) +geom_text()
p2<-ggplot(data=curveness, aes(log(Km1), log(Km2), label=rownames(curveness))) +geom_text()
p3<-ggplot(data=curveness, aes((Vmax1),(Vmax2), label=rownames(curveness))) +geom_text()
p4<-ggplot(data=curveness, aes((Km1), (Km2), label=rownames(curveness))) +geom_text()
multiplot(p1, p2, p3,p4, cols=2)

p<-ggplot(data=curveness, aes(Km1, log(Vmax1), label=rownames(curveness))) +geom_text()
pp<-ggplot(data=curveness, aes((Km1), Vmax1, label=rownames(curveness))) +geom_text()

multiplot(p, pp, cols=2)
corr.test(curveness)
  
######################## below is get "time since switch or beginning 
  
  
  ant_dynamic$min_since<-0
  ant_dynamic$min_since[which(ant_dynamic$environment=="environment 1")]<- subset(ant_dynamic, environment=="environment 1")$minutes
  
  
  for (i in unique(ant_dynamic$groupID)) {
    ant_dynamic$min_since[which(ant_dynamic$environment=="environment 2" & ant_dynamic$groupID==i)]<- subset(ant_dynamic, environment=="environment 2"& groupID==i)$minutes -  ant_dynamic$minutes[which(ant_dynamic$event=="switch" & ant_dynamic$groupID==i)]  
  }
  

  
  
  
  
  ###############################################################
  #for individual ants
  
  test<-subset(indvd_big, environment=="2" & groupID=="e11-1" & type!="t")
  test$time<-1:nrow(test)
  
  fm<-nls(cum_r ~ g1*(1-exp(time*-1*g2)), data=test, start=list(g1=100, g2=.1), trace=TRUE)
  fm<-nls(cum_r ~ g1*time/(g2+time), data=test, start=list(g1=100, g2=200), trace=TRUE)
  g<-coef(fm)
  plot(cum_r ~ time, data=test)
  curve(g[1]*(1-exp(x*g[2])), add=TRUE)
  
  
  
  {
    #actually start here
    curveness<-data.frame(row.names = unique(ant_dynamic$groupID))
    
    for (i in unique(ant_dynamic$groupID)) {
      test<-subset(ant_dynamic[,c(1,12)], ant_dynamic$environment=="environment 1" & ant_dynamic$groupID==i & ant_dynamic$event=="start" & ant_dynamic$visit2<=70)
      test$time<-1:nrow(test)
      print(i)
      #fm<-nls(cum_r ~ g1*time/(g2+time), data=test, start=list(g1=1, g2=2), trace=TRUE)
      fm<-nls(cum_r ~ g1*(1-exp(time*-1*g2)), data=test, start=list(g1=1, g2=1), trace=TRUE)
      g<-coef(fm)
      plot(test)
      #curve(g[1]*x/(g[2]+x), add=TRUE, col="green")
      curve(g[1]*(1-exp(x*-1*g[2])), add=TRUE, col="red")
      
      curveness[i,1]<-g[1]
      curveness[i,2]<-g[2]
      
      test<-subset(ant_dynamic[,c(1,12)], ant_dynamic$environment=="environment 2" & ant_dynamic$groupID==i & ant_dynamic$event=="start" & ant_dynamic$visit2<=60)
      test$time<-1:nrow(test)
      
      #fm<-nls(cum_r ~ g1*time/(g2+time), data=test, start=list(g1=1, g2=2), trace=TRUE)
      try(fm<-nls(cum_r ~ g1*(1-exp(time*-1*g2)), data=test, start=list(g1=1, g2=1), trace=TRUE), silent=TRUE)
      g<-coef(fm)
      plot(test)
      #curve(g[1]*x/(g[2]+x), add=TRUE, col="green")
      curve(g[1]*(1-exp(x*-1*g[2])), add=TRUE, col="green")
      
      curveness[i,3]<-g[1]
      curveness[i,4]<-g[2]
    }
    
    
    #tallying tandem runs
    for (i in unique(ant_dynamic$groupID)) {
      test<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem")
      curveness[i,5]<-dim(test)[1]
    }
    #tallying tandem runs2
    for (i in unique(ant_dynamic$groupID)) {
      test<-subset(ant_dynamic, environment=="environment 2" & groupID==i & event=="tandem")
      curveness[i,6]<-dim(test)[1]
    }
    
    #tallying GOOD tandem runs
    for (i in unique(ant_dynamic$groupID)) {
      test<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem" & feeder2=="good")
      curveness[i,7]<-dim(test)[1]
    }
    #tallying BEST tandem runs2
    for (i in unique(ant_dynamic$groupID)) {
      test<-subset(ant_dynamic, environment=="environment 2" & groupID==i & event=="tandem" & feeder2=="best")
      curveness[i,8]<-dim(test)[1]
    }
    
    colnames(curveness)<-c("Vmax1","Km1","Vmax2","Km2", "tandems1","tandems2","goodtans1","goodtans2")
    
    
    
    
    #curveness$Vmax1<-log(curveness$Vmax1)
    #curveness$Km2<-log(curveness$Km2)
    #curveness<-curveness[-7,]
    #curveness<-curveness[-2,]
    
  }
  
  
  
  {
  #testing with hyperbolic
  curveness<-data.frame(row.names = unique(ant_dynamic$groupID))
  
  for (i in unique(ant_dynamic$groupID)) {
    test<-subset(ant_dynamic[,c(1,12)], ant_dynamic$environment=="environment 1" & ant_dynamic$groupID==i & ant_dynamic$event=="start" & ant_dynamic$visit2<=70)
    test$time<-1:nrow(test)
    print(i)
    fm<-nls(cum_r ~ g1*time/(g2+time), data=test, start=list(g1=1, g2=2), trace=TRUE)
    #fm<-nls(cum_r ~ g1*(1-exp(time*g2)), data=test, start=list(g1=1, g2=-1), trace=TRUE)
    g<-coef(fm)
    plot(test, main=i)
    curve(g[1]*x/(g[2]+x), add=TRUE, col="green")
    #curve(g[1]*(1-exp(x*g[2])), add=TRUE, col="red")
    
    curveness[i,1]<-g[1]
    curveness[i,2]<-g[2]
    
    test<-subset(ant_dynamic[,c(1,12)], ant_dynamic$environment=="environment 2" & ant_dynamic$groupID==i & ant_dynamic$event=="start" & ant_dynamic$visit2<=60)
    test$time<-1:nrow(test)
    
    try(fm<-nls(cum_r ~ g1*time/(g2+time), data=test, start=list(g1=1, g2=2), trace=TRUE), silent=TRUE)
    #try(fm<-nls(cum_r ~ g1*(1-exp(time*g2)), data=test, start=list(g1=1, g2=-1), trace=TRUE), silent=TRUE)
    g<-coef(fm)
    plot(test)
    curve(g[1]*x/(g[2]+x), add=TRUE, col="green")
    #curve(g[1]*(1-exp(x*g[2])), add=TRUE, col="green")
    
    curveness[i,3]<-g[1]
    curveness[i,4]<-g[2]
  }
  
  
  #tallying tandem runs
  for (i in unique(ant_dynamic$groupID)) {
    test<-subset(ant_dynamic, environment=="environment 1" & groupID==i & event=="tandem")
    curveness[i,5]<-dim(test)[1]
  }
  #tallying tandem runs2
  for (i in unique(ant_dynamic$groupID)) {
    test<-subset(ant_dynamic, environment=="environment 2" & groupID==i & event=="tandem")
    curveness[i,6]<-dim(test)[1]
  }
  colnames(curveness)<-c("Vmax1","Km1","Vmax2","Km2", "tandems1","tandems2")
  
  #curveness<-curveness[-7,]
  #curveness<-curveness[-2,]
  
  }
  
  
