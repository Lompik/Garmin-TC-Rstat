library(ggplot2)
library("ascii")
library(gtools)
library(scales)
library(doBy)
library(plyr)
library(zoo)
library(vcd)
library(Hmisc)
library(lubridate)

## Load data with "load.R" first

############################################################################
## helpers                                                                ##
############################################################################


image_path="../plots/"
myplot = function(gplot,title="",xlab="",ylab="", printg="print", width=8,height=4, dpi=300){
    plotg = gplot+xlab(xlab)+ylab(ylab)+ggtitle(title)
    print(plotg)
    if(printg != "print") {
        printg = paste0(image_path, printg)
        ggsave(filename = printg, height=height, width=width, plot=plotg, dpi=dpi)
    }
}


##############################
##########PLOTS START#########
##############################

#PLOT# weekly disance ##id=1
p=ggplot(Totals,aes(x=Week,y=cwTotDistance))+
    geom_point()+
    facet_grid(Sport~., scales = "free")+mytheme
myplot(p, "Total Weekly Distance", "week number","Distance, km", "id1_total_weekly_distance.png")


symnum(cor(scale(na.omit(Results[nums][Results$AverageHeartRateBpm>120 & Results$Sport==levels(Results$Sport)[1] & Results$AvgSpeed<20 & Results$DistanceMeters<2000,]))))

pairs(Results[nums][Results$AverageHeartRateBpm>120 & Results$Sport==levels(Results$Sport)[1] & Results$AvgSpeed<20 & Results$DistanceMeters<2000,])

#PLOT# Monthly Activty Count ##id=2
p = ggplot(Results[!duplicated(Results$ActivityId),],aes(x=ActivityId,fill=Sport))+
    geom_bar(position="dodge",binwidth=60*60*24*30,color="black")+
    scale_x_datetime(breaks=date_breaks("months"),labels=date_format("%b %Y"))+mytheme+
    scale_fill_brewer(palette="Pastel2")+theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
myplot(p, "Activity Count by Month", "Month","Counts", "id2_Monthly_act_count.png")

#PLOT# HR vs Startime by actvity ##id=3
p=ggplot(Results[Results$Sport %in% c("Running","Biking") & Results$AverageHeartRateBpm>120 & Results$AvgSpeed>10 & Results$DistanceMeters>980 & !is.na(Results$AverageHeartRateBpm),],
         aes(x=StartTime,y=AverageHeartRateBpm,color=AvgSpeed))+
    geom_point()+
    facet_grid(Sport~., drop = TRUE)+
    ##scale_color_gradient2(limits=c(10,35),midpoint=14, mid="chocolate", low="white")+
    scale_color_gradientn(limits=c(10,35),colors=c("green3","chocolate","chocolate4",muted("blue")), values=c(0,3/(35-10),20/(35-10),1))+
    scale_x_datetime(breaks=date_breaks("months"),labels=date_format("%B"))+
    geom_smooth()+
    mytheme+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
myplot(p, "HeartRate vs StartTime by activity", "Month","Average Heart Rate, bpm", "id3_HR_vs_Starttime.png")


#PLOT# Weekly Distance by Sport ##id=4
# May change scale label to WeekDesc
ggplot(Totals[!Totals$Sport=="Other",],aes(x=Week,y=cwTotDistance))+
    geom_point()+
    facet_grid(Sport~.,scales="free")+
    scale_x_continuous(breaks=c(Totals[Totals$Week%%4==0,]$Week),labels=c(as.character(format(Totals[Totals$Week%%4==0,]$ActivityId,"%d%b%y"))))+
    geom_smooth(method="loess")
myplot(p, "Weekly Distance by Sport", "Week","Distance, km", "id4_Weekly_distance_bySport.png")


#PLOT# Weekly Distance +AvgSpeed by Sport ##id=5
ggplot(Totals[!Totals$Sport=="Other",],aes(x=Week))+
    geom_point(aes(y=cwTotDistance),color="blue")+
    facet_grid(Sport~.,scales="free")+
    scale_x_continuous(breaks=c(Totals[Totals$Week%%4==0,]$Week),labels=c(as.character(format(Totals[Totals$Week%%4==0,]$ActivityId,"%d%b%y"))))+
    geom_smooth(aes(y=cwTotDistance))+
    stat_summary(aes(y=cwTotDistance,alpha=0.5),fun.y = "mean", fill = "grey50", geom = "bar")+
    stat_summary(aes(y=cwAvgSpeed,alpha=0.5,size=0.5),fun.y = "mean", fill = "red", geom = "bar")+
    geom_line(aes(y=cwTotDistance))

#PLOT# day of week bar plot by Sport ##id=6
p=ggplot(Totals,aes(x=DayOfWeek,fill=Sport))+
    geom_bar()+mytheme+
  scale_fill_brewer(palette = "Pastel2") #+facet_grid(MonthFac~.)
myplot(p, "Activity count by Day of week", "Day","Count", "id6_Activity_dayofWeek.png")


#PLOT# all HR vs duration superposed by month ##id=7
p=ggplot(Results[with(Results,AverageHeartRateBpm>120& AvgSpeed<18 & AvgSpeed>10 & DistanceMeters>300 & !is.na(AverageHeartRateBpm)),],aes(x=DurationMinute,y=AverageHeartRateBpm,color=AvgSpeed))+
    geom_line(aes(group=ActivityId))+
    scale_color_gradient2(midpoint = 14,mid=muted("green"),high="red", low="blue")+
    geom_point(alpha=0.9)+
    facet_grid(Month~.)+coord_cartesian(xlim=c(04,60))+
    scale_y_continuous(breaks=c(135,146,162,176,192))+
    mytheme
myplot(p, "Heat Rate by activity vs Elapsed Time", "Elapsed Time (Minutes)","HR (BPM)", "id7_HR_etime.png", width=8,height=16,dpi=90)

#PLOT# Plot all biking superposed with respect to duration ##id=8
ggplot(Results[FILTER_BIKING,],aes(x=DurationMinute,y=AvgSpeed,group=ActivityId,color=AverageHeartRateBpm))+
    geom_line()

ggplot(Results[FILTER_BIKING & Results$TotDuration<100,],aes(x=DurationMinute,y=AvgSpeed,group=ActivityId))+
    geom_line(aes(color=AverageHeartRateBpm),size=2)+
    geom_point(aes(y=AvgSpeed,size=3,color=AverageHeartRateBpm))+
    facet_grid(Month~.)

ggplot(Results[FILTER_BIKING & Results$DurationMax<100,],aes(x=Month,y=AvgSpeed))+
    geom_boxplot()

#PLOT# Same plot as on garmin Connect: Activity Count vs Time ##id:25
p = ggplot(Results[!duplicated(Results$ActivityId),],aes(x=Month,y=..count..,color=Sport))+
    geom_line(aes(group=Sport),stat="count")+
    facet_grid(~year)+
    mytheme+
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
myplot(p, "Activity count per year", "Month","Count", "id25_Activity_peryear.png")


#PLOT# plot Temperatur by duration by month color by Speed ##id=9
# - Need Resdults$Month
ggplot(Results[Results$AverageHeartRateBpm>120& Results$AvgSpeed<18   & Results$AvgSpeed>10 & Results$DistanceMeters>300,],aes(x=DurationMinute,y=Temp,color=AvgSpeed))+
    geom_line(aes(group=ActivityId))+
    geom_point(alpha=0.9)+
    facet_grid(Month~.)+coord_cartesian(xlim=c(04,100))


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

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

#PLOT# Bike boxplot Multiplot ##id=10
p1=ggplot(Results[FILTER_BIKING & Results$TotDuration<100,],aes(x=MonthFac,y=AvgSpeed))+
    geom_boxplot(outlier.colour = "red",aes(fill=MonthFac))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)
p2=ggplot(Results[FILTER_BIKING & Results$TotDuration<100,],aes(x=MonthFac,y=AverageHeartRateBpm))+
    geom_boxplot(outlier.colour = "red",aes(fill=MonthFac))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)
p3=ggplot(Results[FILTER_BIKING & Results$TotDuration<100,],aes(x=MonthFac,y=AvgSpeed))+
    geom_boxplot(outlier.colour = "red",aes(fill=MonthFac))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)
#p3=ggplot(Totals[Totals$ActivityId %in% Results[FILTER_BIKING & Results$TotDuration<100,"ActivityId"],],aes(x=MonthFac,y=TotDistance))+
geom_bar(aes(y=..sum..,fill=MonthFac))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)
p3=ggplot(Totals[Totals$ActivityId %in% Results[FILTER_BIKING & Results$TotDuration<100,"ActivityId"],],aes(x=as.Date(ActivityId)))+
    geom_bar(stat = "sum",aes(y=TotDistance,fill=MonthFac))+
    scale_x_date(breaks="1 month",labels=date_format("%b %y"))+
    geom_smooth(aes(y=TotDistance))
multiplot(p1,p2,p3)

#PLOT# Run boxplot Multiplot ##id=11
p1=ggplot(Results[FILTER_RUNNING & Results$TotDuration<100,],aes(x=MonthFac,y=AvgSpeed))+
    geom_boxplot(outlier.colour = "red",aes(fill=MonthFac))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)
p2=ggplot(Results[FILTER_RUNNING & Results$TotDuration<100,],aes(x=MonthFac,y=AverageHeartRateBpm))+
    geom_boxplot(outlier.colour = "red",aes(fill=MonthFac))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)
p3=ggplot(Totals[Totals$ActivityId %in% Results[FILTER_RUNNING & Results$TotDuration<100,"ActivityId"],],aes(x=as.Date(ActivityId)))+
    geom_bar(stat = "sum",aes(y=TotDistance,fill=MonthFac))+
    scale_x_date(breaks="1 month",labels=date_format("%b %y"))+
    geom_smooth(aes(y=TotDistance))
multiplot(p1,p2,p3)

#PLOT# Bike & Run boxplot Multiplot ##id=12
p1=ggplot(Results[FILTER_HR & Results$TotDuration<100,],aes(x=MonthFac,y=AvgSpeed))+
    geom_boxplot(outlier.colour = "red",aes(fill=Sport))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)
p2=ggplot(Results[FILTER_HR & Results$TotDuration<100,],aes(x=MonthFac,y=AverageHeartRateBpm))+
    geom_boxplot(outlier.colour = "red",aes(fill=Sport))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)
p3=ggplot(Results[FILTER_HR & Results$TotDuration<100,],aes(x=as.Date(ActivityId)))+
    geom_line(aes(y=TotDistance,color=Sport,group=Sport))+
    scale_x_date(breaks="1 month",labels=date_format("%b %y"))+
    geom_smooth(aes(y=TotDistance,group=Sport))+
    geom_text(data=Records,aes(x=as.Date(ActivityId),y=TotDistance,label=Description,fill=MonthFac,group=Sport))+
    geom_point(data=Records,aes(x=as.Date(ActivityId),y=TotDistance,size=4),color="red")
multiplot(p1,p2,p3)

#PLOT#Test With scale_x_date ##id=13
ggplot(Results[FILTER_BIKING & Results$TotDuration<100,],aes(x=as.Date(StartTime),y=AvgSpeed,group=monthyear))+
    geom_boxplot(outlier.colour = "red",aes(fill=Month))+
    scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b-%Y"))

#
ggplot(Totals[Totals$ActivityId %in% Results[FILTER_BIKING & Results$TotDuration<100,"ActivityId"],],aes(x=as.Date(ActivityId)))+
    geom_bar(stat = "sum",aes(y=TotDistance,fill=MonthFac))+
    scale_x_date(breaks="1 month",labels=date_format("%b %y"))

#
hist(as.numeric(diff(Totals$ActivityId))/3600/24,breaks = 50,col="lightblue",labels=TRUE,border =570)

Totals=(ddply(Totals,.(Sport),transform,cTotDistance=cumsum(TotDistance)))

#PLOT# Distance vs time Cumsum plot ##id=14
p=ggplot(Totals,aes(x=ActivityId,y=cTotDistance,group=Sport))+
    geom_line(aes(color=Sport))+
    mytheme
myplot(p, "Total Distance by Activity", "Date","Distance (km)", "id14_Total_Distance_byact.png")

ggplot(Totals,aes (x=DayOfWeek,y=TotDistance))+
    geom_bar(stat="sum",aes(fill=Sport),position = "dodge") + facet_grid(MonthFac~.,scales = "free")

#PLOT# cwCumDistance vs DayOfWeek Cumsum plot ##id=15
ggplot(Totals,aes (x=DayOfWeek,y=cwCumDistance, size=as.factor((MonthFac=="Jun 13")*Week), color=as.factor((MonthFac=="Jun 13")*Week),group=Week))+
    geom_line()+
    scale_colour_hue(h=c(50, 150))

ggplot(Totals,aes (x=DayOfWeek,y=cwCumDistance,group=Week))+
    geom_line()+
    facet_grid(MonthFac~.,scales = "free")

plot(Totals[Totals$Sport=="Biking",]$ActivityId,cumsum(Totals[Totals$Sport=="Biking",]$TotDistance))

#PLOT# Plot relative time to record = Running - 10k ##id=16
plot(Totals[FILTER_RUNNING_10k,"ActivityId"],1/(Totals[FILTER_RUNNING_10k,"TotDuration"]/Totals[Totals$ActivityId==Records[Records$Active==TRUE&Records$Sport=="Running"&Records$Distance==10,"ActivityId"],"TotDuration"]))
abline(v=Totals[Totals$ActivityId==Records[Records$Active==TRUE&Records$Sport=="Running"&Records$Distance==10,"ActivityId"],"ActivityId"],col="red")

#PLOT# histogram of hour of day by Month ##id=17
ggplot(Totals,aes(x=as.POSIXct((strftime(ActivityId, format="%H:%M:%S")),format="%H:%M:%S")))+
    geom_bar()+
    facet_grid(MonthFac~.)

#PLOT# histogram of Duration/*Record* Speed ##id=18
hist(1/(Totals[FILTER_RUNNING_10k,"TotDuration"]/Totals[Totals$ActivityId==Records[Records$Active==TRUE&Records$Sport=="Running"&Records$Distance==10,"ActivityId"],"TotDuration"]),breaks=20)

Totals$RelTime=1
Totals[FILTER_RUNNING_10k,]$RelTime=(1 /(Totals[FILTER_RUNNING_10k,"TotDuration"]/Totals[Totals$ActivityId==Records[Records$Active==TRUE&Records$Sport=="Running"&Records$Distance==10,"ActivityId"],"TotDuration"]))
#PLOT# Plot avgspeed ~ distance by Sport ##id=19
p=ggplot(Totals[Totals$TotDuration<100 & Totals$Sport %in% c("Running","Biking"),],aes(x=TotDistance,y=AvgSpeed,color=AvgSpeed))+
    geom_point()+
    facet_grid(Sport~., scales = "free")+
    geom_point(data=Totals[Totals$ActivityId %in% Records[Records$Active==TRUE,"ActivityId"],],aes(x=TotDistance,y=AvgSpeed),colour="red",size=1)+
    mytheme
myplot(p, "Average speed vs date", "Date","Average speed (km/h)", "id19_AvgSpeed_date.png")

#PLOT# PLot avgspeed ~ distance for Running + Regression ##id=20
p=ggplot(Totals[Totals$TotDuration<100&Totals$Sport=="Running",],aes(x=TotDistance,y=AvgSpeed,color=TotDuration))+
    geom_point(size=3)+
    geom_point(data=Totals[Totals$ActivityId %in% Records[Records$Active==TRUE&Records$Sport=="Running","ActivityId"],],aes(x=TotDistance,y=AvgSpeed),colour="red",size=5)+
    scale_y_continuous(limit=c(9,15))+
    stat_smooth(data=Totals[Totals$ActivityId %in% Records[Records$Active==TRUE&Records$Sport=="Running","ActivityId"],],aes(x=TotDistance,y=AvgSpeed))+
    mytheme
myplot(p, "Average speed vs date (Running)", "Date","Average speed (km/h)", "id20_AvgSpeed_date_running.png")

p=ggplot(Totals[Totals$TotDuration<100&Totals$Sport=="Running",],aes(x=TotDistance,y=AvgSpeed,color=TotDuration))+
    geom_point(size=3)+
    geom_point(data=Totals[Totals$ActivityId %in% Records[Records$Active==TRUE&Records$Sport=="Running","ActivityId"],],aes(x=TotDistance,y=AvgSpeed),colour="red",size=1)+
    scale_y_continuous(limit=c(9,15))+
    stat_smooth(data=Totals[Totals$ActivityId %in% Records[Records$Active==TRUE&Records$Sport=="Running","ActivityId"],],aes(x=TotDistance,y=AvgSpeed),method="glm",formula = y~exp(-x/10))+
    mytheme
myplot(p, "Average speed vs date (Biking)", "Date","Average speed (km/h)", "id21_AvgSpeed_date_bike.png")

#PLOT# PLot avgspeed ~ distance for Running + Regression + MonthFac ##id=21
ggplot(Totals[Totals$TotDuration<100&Totals$Sport=="Running",],aes(x=TotDistance,y=AvgSpeed,color=TotDuration))+
    geom_point(aes(label=MonthFac),size=3)+
    geom_point(data=Totals[Totals$ActivityId %in% Records[Records$Active==TRUE&Records$Sport=="Running","ActivityId"],],aes(x=TotDistance,y=AvgSpeed),colour="red",size=5)+
    scale_y_continuous(limit=c(9,15))+
    geom_text(aes(label=MonthFac),vjust=1,hjust=1)

glm1=glm(data=Totals[Totals$ActivityId %in% Records[Records$Active==TRUE&Records$Sport=="Running","ActivityId"],],formula=AvgSpeed~exp(-TotDistance/10))
plot(Totals[Totals$Sport=="Running", "TotDistance"],predict(glm1,newdata = Totals[Totals$Sport=="Running", ],x="TotDistance"))
42.2/(as.numeric(glm1$coefficient[1])+as.numeric(glm1$coefficient[2])*exp(-42.2 /10))

Totals=merge(Totals,data.frame(ActivityId=Totals[Totals$Sport=="Running","ActivityId"],RelTimeModel=1/(Totals[Totals$Sport=="Running", "TotDuration"]/(60*Totals[Totals$Sport=="Running", "TotDistance"]/as.numeric(predict(glm1,newdata = Totals[Totals$Sport=="Running", ],x="TotDistance"))))),by="ActivityId",all.x=TRUE,all.y=FALSE)

#PLOT# Relative time to max speed (model) bar plot ##id=22
ggplot(Totals[Totals$Sport=="Running"&Totals$RelTimeModel>0.4,],aes(x=RelTimeModel))+
    geom_bar()+
    facet_grid(MonthFac~.)

#PLOT# Boxplot Relative time to max speed (model)+
geom_smooth ##id=23
ggplot(Totals[Totals$Sport=="Running"&Totals$RelTimeModel>0.4,],aes(x=ActivityId,y=RelTimeModel))+
    geom_point(aes(color=MonthFac))+
    geom_smooth()

#PLOT# Days of Rest vs time ##id=24
ggplot(Totals,aes(x=ActivityId,y=DaysofRest))+
    geom_point()+
    geom_smooth()+
    geom_boxplot(aes(group=MonthFac))#+scale_y_continuous(limits=c(0,10))
aggregate(DaysofRest ~ MonthFac,data=Totals[,c("MonthFac","DaysofRest")],FUN=mean)

# One week Activity output
Totals[Totals$ActivityId>max (Totals$ActivityId)-7*24*3600,c("Sport","ActivityId","DayOfWeek")]
