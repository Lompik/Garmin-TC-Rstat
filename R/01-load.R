library(RSQLite)
library(ggplot2)
library(gtools)
library(scales)
library(doBy)
library(plyr)
library(zoo)
library(vcd)
library(Hmisc)
library(lubridate)

db = dbConnect(dbDriver("SQLite"), dbname = "20140309.sqlite")
data= dbGetQuery(db,paste("SELECT Activities.StartTime ,Activities.SportId ,ActivityLaps.StartTime ,TotalsTable.TotalTime ,TotalsTable.TotalDistance ,TotalsTable.MaxSpeed ,TotalsTable.TotalCalories ,ActivityLaps.IntensityType ,ActivityLaps.TriggerType , TotalsTable.AverageHeartRate ,TotalsTable.MaxHeartRate ,TotalsTable.AverageCadence ,TotalsTable.AverageSpeed ,Activities.ActivityId ,ActivityLaps.LapNumber \n","FROM ActivityLaps\n","INNER JOIN TotalsTable\n","ON TotalsTable.TotalsTableId = ActivityLaps.TotalsTableId-1 \n","INNER JOIN Activities\n","ON Activities.ActivityId = ActivityLaps.ActivityId"))
names(data)=c("ActivityId" ,"Sport" ,"StartTime" ,"TotalTimeSeconds" ,"DistanceMeters" ,"MaximumSpeed" ,"Calories" ,"Intensity" ,"TriggerMethod" ,"AverageHeartRateBpm" ,"MaximumHeartRateBpm" ,"Cadence" ,"AvgSpeed","DBActivityId","DBLapNumber" )
ActivitySum=dbGetQuery(db,paste("SELECT TotalsTable.TotalTime ,TotalsTable.TotalDistance ,TotalsTable.MaxSpeed ,TotalsTable.TotalCalories ,TotalsTable.AverageHeartRate ,TotalsTable.MaxHeartRate ,TotalsTable.AverageCadence ,TotalsTable.AverageSpeed,Activities.ActivityId,Activities.StartTime,Activities.SportId\n ","FROM TotalsTable\n","INNER JOIN Activities\n","ON Activities.TotalsTableId=TotalsTable.TotalsTableId"))

SportTable=dbGetQuery(db,"SELECT * from Sports")
ActSeg=dbGetQuery(db,"SELECT * FROM ActivitySegments")
TriggerTable=dbGetQuery(db,"SELECT * from TriggerTypes")
dbDisconnect(db)
# Get rid of MultiSport Activites - replace by Actual Sport
for(i in 1:nrow(ActSeg)) {
  data[data$StartTime>=ActSeg[i,]$StartTime & data$StartTime<ActSeg[i,]$EndTime & data$DBActivityId==ActSeg[i,]$ActivityId,]$Sport=ActSeg[i,]$Sport
}

# Switch to Results data Frame(historical)
data$ActivityId=as.POSIXct(data$ActivityId,origin = "1970-01-01",tz="UTC")
data$StartTime=as.POSIXct(data$StartTime,origin = "1970-01-01",tz="UTC")
ActivitySum$StartTime=as.POSIXct(ActivitySum$StartTime,origin = "1970-01-01",tz="UTC")
data$Sport=factor(data$Sport,levels=SportTable$SportId,labels=SportTable$Name)
ActivitySum$SportId=factor(ActivitySum$SportId,levels=SportTable$SportId,labels=SportTable$Name)
data$TriggerMethod=factor(data$TriggerMethod,levels=TriggerTable$TriggerType,labels=TriggerTable$Description)
Results=data
Results$AvgSpeed=Results$Distance/Results$TotalTimeSeconds*3.6
nums=c("TotalTimeSeconds","DistanceMeters","MaximumSpeed","Calories","AverageHeartRateBpm","AvgSpeed","Cadence")

#Checks
abc=sapply(names(Results),function(x) length(Results[,x]))
if(count(abc==abc[1])$freq!=length(names(Results))) print("#####ERROR###freq!=no Resu Columns")

with(Results,
     {
       FILTER_HR<<-AverageHeartRateBpm>120 & AverageHeartRateBpm<190
       FILTER_HR[is.na(FILTER_HR)]<<-FALSE
       FILTER_RUNNING<<-FILTER_HR & Sport=="Running" & AvgSpeed<20 & DistanceMeters<2000
       FILTER_BIKING<<-FILTER_HR & Sport==levels(Sport)[2] & AvgSpeed>23 & DistanceMeters>4500
       FILTER_NDUP<<-!duplicated(ActivityId)
     }
)

tolerance=0.05
attr(Results[,"ActivityId"],"tzone")="CST6CDT"
attr(Results[,"StartTime"],"tzone")="CST6CDT"

#plot Heart Rate by duration by month
Results=(Results[with(Results,order(StartTime)),])
Results$Duration=(Results$StartTime-Results$ActivityId+Results$TotalTimeSeconds)
Results$DurationMinute=as.numeric(Results$Duration,units="secs")/60
Results$Month=month(Results$ActivityId,label=T)
Results$MonthFac=(factor(format(Results$StartTime,"%b %y")))
Results$MonthFac=reorder(Results$MonthFac,as.numeric(Results$StartTime))
Results$year=year(Results$ActivityId)
ActivitySum$Month=month(ActivitySum$StartTime,label=T)
Results=transform(Results,WeekOfYear=as.numeric(strftime(ActivityId,format="%W")),DayOfWeek=wday(ActivityId,label=TRUE))
Results=transform(Results,Week=1+52*(year(ActivityId)-year(Results[1,"ActivityId"]))+as.numeric(strftime(ActivityId,format="%W"))-as.numeric(strftime(Results[1,"ActivityId"],format="%W")) )
Results$DayOfWeek=(factor(Results$DayOfWeek,levels=c(levels(Results$DayOfWeek)[-1],levels(Results$DayOfWeek)[1])))

##########TOTALS###################TOTALS###################TOTALS###################TOTALS#########
#Get the maximim duration for each activity
# TotDuration=tapply(Results$DurationMinute,Results$ActivityId,max)
# TotDistance=as.data.frame(tapply(Results$DistanceMeters,Results$ActivityId,sum))
# TotDuration=as.data.frame(TotDuration)
# TotDuration$ActivityId=as.POSIXct(row.names(TotDuration),tz="UTC")
# TotDistance$ActivityId=as.POSIXct(row.names(TotDistance),tz="UTC")
# names(TotDuration)=c("TotDuration","ActivityId")
# names(TotDistance)=c("TotDistance","ActivityId")
# Totals=merge(TotDuration,TotDistance,by=c("ActivityId","ActivityId"))
# Totals$TotDistance=as.numeric(Totals$TotDistance)
# Totals$TotDuration=as.numeric(Totals$TotDuration)
# Totals=transform(Totals,TotDistance=TotDistance/1000)
Totals=(ddply(Results,.(ActivityId),summarise,
              TotDistance=sum(DistanceMeters)/1000,
              TotDuration=max(DurationMinute),
              AvgHR=mean(AverageHeartRateBpm),
              Sport=Sport[1],
              WeekOfYear=WeekOfYear[1],
              DayOfWeek=DayOfWeek[1],
              Week=Week[1]))
Results=(ddply(Results,.(ActivityId),transform,
              TotDistance=sum(DistanceMeters)/1000,
              TotDuration=max(DurationMinute)))
#Results=merge(Results,Totals,all.x=TRUE,by.x="ActivityId",by.y="ActivityId")
Totals=transform(Totals,AvgSpeed=TotDistance/TotDuration*60,
            MonthFac=reorder(factor(format(Totals$ActivityId,"%b %y")),as.numeric(Totals$ActivityId)),
            WeekDesc=paste(Week,month(ActivityId,label=TRUE))
  )
Totals=(Totals[with(Totals,order(ActivityId)),])
Results$monthyear=do.call(gsub,list("20"," ",do.call(paste, c(Results[c("Month", "year")], sep = ""))))
MonthTot=as.data.frame(tapply(Totals$TotDistance,Totals$MonthFac,sum))
MonthTot$MonthFac=factor(row.names(MonthTot),levels=levels(Results$MonthFac))
names(MonthTot)=c("TotDistance","MonthFac")
#Totals=merge(Totals,Results[,c("ActivityId","Sport","WeekOfYear","DayOfWeek","Week")],by=c("ActivityId","ActivityId"),all.x=TRUE,all.y=FALSE)
#Totals=Totals[!duplicated(Totals$ActivityId),]

#Totals=(Totals[!is.na (Totals$TotDuration),])
#an old check of data

Totals[Totals$Sport=="Running"&Totals$AvgSpeed>20,"Sport"]="Biking"
Results[Results$ActivityId %in% Totals[Totals$Sport=="Running" & Totals$AvgSpeed>20,"ActivityId"], "Sport"] ="Biking"
FILTER_RUNNING_10k= Totals$TotDistance<10+10*tolerance & Totals$TotDistance>10-10*tolerance
Totals=ddply(.data=Totals,.(Sport,Week),transform,cwTotDistance=sum(TotDistance),cwAvgSpeed=mean(AvgSpeed),cwTotDuration=sum(TotDuration))

Totals=(ddply(Totals,.(Week),transform,cwCumDistance=cumsum(TotDistance)))
Totals=Totals[order(Totals$ActivityId),]

Totals[,"DaysofRest"]=0
Totals[-1,]$DaysofRest=as.numeric(diff(Totals[,"ActivityId"]),units="days")

##########RECORDS####################RECORDS####################RECORDS####################RECORDS##########
#SpecialDistance in km
SpecialDistance=c(5,10,21.1,42.2)
SpecialDistance=data.frame(dist=SpecialDistance)
#separator is double space
Records="Distance,Unit,Sport,ActivityId,Active
5,km,Running,2012-08-03 19:54:20,FALSE
5,km,Running,2013-06-07 18:23:12,TRUE
10,km,Running,2012-08-25 07:15:55,FALSE
10,km,Running,2012-11-18 20:04:46,FALSE
10,km,Running,2013-10-09 18:26:54,TRUE
21.1,km,Running,2013-02-10 08:36:29,TRUE
40,km,Biking,2013-03-26 19:06:36,TRUE
"
Records=read.table(textConnection(Records),header = TRUE,sep=",")
Records$ActivityId=as.POSIXct(Records$ActivityId,tz="CST6CDT")
attr(Records$ActivityId,"tzone")="CST6CDT"
Records$Description=do.call(paste, c(Records[c("Distance", "Unit")], sep = ""))
Records=merge(Records,Totals[,names(Totals)!="Sport"],by=c("ActivityId","ActivityId"),all.x=TRUE,all.y=FALSE)


#display Results for particular records
Results[Results$ActivityId==Records[Records$Active==TRUE&Records$Sport=="Running"&Records$Distance==5,"ActivityId"],]

Totals$RelTime=1
Totals[FILTER_RUNNING_10k,]$RelTime=(1 /(Totals[FILTER_RUNNING_10k,"TotDuration"]/Totals[Totals$ActivityId==Records[Records$Active==TRUE&Records$Sport=="Running"&Records$Distance==10,"ActivityId"],"TotDuration"]))
