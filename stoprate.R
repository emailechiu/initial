require("RODBC")
library(lubridate)
require(ggplot2)

library(plyr)
gw_beam<-read.csv("Jupiter_GW_BEAM.csv",stringsAsFactor=FALSE);str(gw_beam)
gw_beam[,3]<-NULL
colnames(gw_beam)<-c("GW","GW_NAME","BEAM.1","BEAM.2","BEAM.3","BEAM.4")
	#gw_beam<-rbind(data.frame(gw_beam$GW,gw_beam$GW_NAME,gw_beam$BEAM.1),data.frame(gw_beam$GW,gw_beam$GW_NAME,gw_beam$BEAM.2),data.frame(gw_beam$GW,gw_beam$GW_NAME,gw_beam$BEAM.3),data.frame(gw_beam$GW,gw_beam$GW_NAME,gw_beam$BEAM.4))
	#gw_beam<-rbind.fill(data.frame(gw_beam$GW,gw_beam$GW_NAME,gw_beam$BEAM.1),data.frame(gw_beam$GW,gw_beam$GW_NAME,gw_beam$BEAM.2),data.frame(gw_beam$GW,gw_beam$GW_NAME,gw_beam$BEAM.3),data.frame(gw_beam$GW,gw_beam$GW_NAME,gw_beam$BEAM.4))
	#gw_beam$BEAM<-paste(gw_beam$gw_beam.BEAM.1,gw_beam$gw_beam.BEAM.2,gw_beam$gw_beam.BEAM.3,gw_beam$gw_beam.BEAM.4)
	#gw_beam$BEAM<-NULL
library(reshape);
gw_beam.melt<-melt(gw_beam,id=c(1:2))
gw_beam.melt$variable<-NULL
gw_beam.melt$BEAM<-gsub('([0-9]+)([a-zA-Z]+)',"\\1__\\1",gw_beam.melt$value)
gw_beam.melt$BEAM<-as.numeric(gsub("([0-9]+) \\(([a-zA-Z ]+, [A-Z]{2})\\)","\\1",gw_beam.melt$value));str(gw_beam.melt)#syntex correct
gw_beam.melt$BEAM_NAME<-gsub("([0-9]+) \\(([a-zA-Z ]+, [A-Z]{2})\\)","\\2",gw_beam.melt$value)
gw_beam.melt$value<-NULL
colnames(gw_beam.melt)<-c("GW","GW_LOC","BEAM","BEAM_LOC");head(gw_beam.melt)
gw_beam<-gw_beam.melt
write.csv(gw_beam,file="BEAM_GW_LKUP.csv")

beam_gw<-sqlQuery(odbcConnect("SQLEVAL"),"select * from etl.dbo.BEAM_GW_LKUP")
beam_gw<-data.frame(beam_gw,GW_LOC=rep(NA,52),BEAM_LOC=rep(NA,52))
gb<-rbind(gw_beam,beam_gw)
head(gw_beam);head(beam_gw);gb
gb[duplicated(gb[,c(1,3)]),]  # This synchronized both database and we can use gw_beam from now on
beam_gw<-gw_beam
beam_gw<-do.call(rbind,lapply(split(beam_gw,beam_gw$GW),transform,Color = rank(BEAM,ties.method = "first")))

conn<-odbcConnect("whseprd.world")
stoprate<-sqlQuery(conn,"select PROCESS_DAY,BEAM_ID as BEAM,TYPE2,TYPE3,TYPE10,TERMCNT_FROM_WAREHOUSE as Nom,HT1100_CNT as Denom, TERMCNT_FROM_WAREHOUSE/HT1100_CNT as Rate from QA_REBOOT_NOM join QA_REBOOT_DENOM on COLLECTION_START_TIME_EST=PROCESS_DAY and BEAM_ID=ABSOLUTE_BEAM_ID
where PROCESS_DAY>=SYSDATE-48 order by PROCESS_DAY,BEAM");head(stoprate)
colnames(stoprate)<-c("Date","BEAM","Type2","Type3","Type10","Nom","Denom","Rate")

stoprate<-merge(beam_gw,stoprate,by="BEAM");head(stoprate);

require(plyr)
rollup<-join_all(list(with(stoprate,aggregate(Nom,by=list(Date),FUN=sum)),with(stoprate,aggregate(Denom,by=list(Date),FUN=sum)),with(stoprate,aggregate(Type2,by=list(Date),FUN=sum)), with(stoprate,aggregate(Type3,by=list(Date),FUN=sum)),with(stoprate,aggregate(Type10,by=list(Date),FUN=sum))),by="Group.1", type="inner");head(rollup)
colnames(rollup)<-c("Date","Nom","Denom","Type2","Type3","Type10")
rollup<-transform(rollup, Rate=Nom/Denom);rollup[order(rollup$Rate),]
stoprate<-rbind(stoprate,data.frame(BEAM=rep(0,nrow(rollup)),GW=rep(0,nrow(rollup)),GW_LOC=rep('ALL',nrow(rollup)), BEAM_LOC=rep('ALL',nrow(rollup)),Color=rep(5,nrow(rollup)), rollup))

p<-ggplot(stoprate)+aes(Date,Rate,group=BEAM,colour=factor(Color),shape=NA)+geom_point()+geom_line(aes( label=as.character(BEAM) ))+facet_grid(GW_LOC~.)+ geom_text(aes(label=BEAM),size = 3);p
p+scale_colour_brewer(palette="Set1")

day(stoprate$Date)
p+scale_colour_hue(limits = levels(factor(stoprate$BEAM%%4)))

############Get the problematic reboot time####################
require("RODBC")
conn<-odbcConnect("whseprd.world",uid='echiu',pwd='P@ssw0rd1')
call_1114<-sqlQuery(conn, "select s.SITE_NAME, CASE_OPENED_DATE as CASE_DATETIME, TAD_DESCRIPTION_SUMMARY, HNSBEAMID as BEAM from PS_CASE c join QA_JUPITER_SITE s on c.HNS_SITE_ID=s.SITE_NAME
where CASE_OPENED_DATE between to_date('11/23/2014','mm/dd/yyyy') and to_date('11/24/2014','mm/dd/yyyy') and HNSBEAMID=58")

reboot<-sqlQuery(conn, "select * from JUPITER_HOURLY_TERMINAL_STATS where collection_start_time_est between to_date('11/23/2014','mm/dd/yyyy') and to_date('11/24/2014','mm/dd/yyyy') and LAST_REBOOT_REASON in (2,3,10)" )
hist(reboot$COLLECTION_START_TIME_EST,breaks=24,freq=TRUE)

hist(reboot_1002$COLLECTION_START_TIME_EST,breaks=24,freq=TRUE)
reboot_0909<-sqlQuery(conn, "select * from JUPITER_HOURLY_TERMINAL_STATS where collection_start_time_est between to_date('09/09/2014','mm/dd/yyyy') and to_date('09/10/2014','mm/dd/yyyy') and LAST_REBOOT_REASON in (2,3,10)")
hist(reboot_1114[reboot_1114$LAST_REBOOT_REASON==2,]$COLLECTION_START_TIME_EST,breaks=24,freq=TRUE)
hist(call_1114$CASE_DATETIME,breaks=24,freq=TRUE)
require(ggplot2)
ggplot(reboot_1114[reboot_1114$ABSOLUTE_BEAM_ID==54,])+aes(COLLECTION_START_TIME_EST) +geom_histogram()
ggplot(reboot_1114[reboot_1114$ABSOLUTE_BEAM_ID==46,])+aes(COLLECTION_START_TIME_EST) +geom_histogram()
ggplot(reboot_1114[reboot_1114$ABSOLUTE_BEAM_ID==53,])+aes(COLLECTION_START_TIME_EST) +geom_histogram()
ggplot(reboot_1114[reboot_1114$ABSOLUTE_BEAM_ID<10,])+aes(COLLECTION_START_TIME_EST) +geom_histogram()+facet_grid(ABSOLUTE_BEAM_ID~.)
##############Get the GWs##############

library(stringr)
str_extract_all(gw_beam.melt$value,"\\(?[0-9,.]+\\)?")
gregexpr('\\(?[0-9,.]+', gw_beam.melt$value)

gsub('([0-9]+)([a-zA-Z]+)',"\\1__\\1__\\2__\\2",gw_beam.melt$value)
gsub("\\((.+)\\)","-\\1",gw_beam.melt$value)
unlist(regmatches(gw_beam.melt$value, gregexpr('\\(?[0-9,.]+',gw_beam.melt$value)))
gsub("(\d+)\s+\((\w+, [A-Z]{2})\)","\\1",gw_beam.melt$value)
gsub("([0-9]+)  \\((\\w+, [A-Z]{2})\\)","\\1",gw_beam.melt$value)
gsub("\\((.*?) :: (0\\.[0-9]+)\\)","\\1 \\2", "(sometext :: 0.1231313213)")

regexp("([0-9]+)  \\((\\w+, [A-Z]{2})\\)","\\1",gw_beam.melt$value)


unmelt_(gw_beam.melt, variable = "variable", value = "value") #Incorrect!

########How to partition by GW 2 cols###########reshape,aggregate,do.call,tapply#########33
attach(beam_gw)
tapply(beam_gw$BEAM,beam_gw$GW,list)
tapply(beam_gw$BEAM,beam_gw$GW,order) 
by(beam_gw,GW,paste)
split(beam_gw,GW)
unstack(data.frame(beam_gw,GW))

beam_gw<-beam_gw[!beam_gw$BEAM %in% c(6,3,NA),] #Remove some to make non-rectangular
test.matrix<-matrix(c(1:56),7,8);str(test.matrix)
test.docall<-do.call(rbind,tapply(beam_gw$BEAM,beam_gw$GW,list));str(test.docall) #produces 13x4, Error with Recycling when uneven
test.aggr<-aggregate(beam_gw$BEAM,by=list(beam_gw$GW),list);str(test.aggr) #produces 13x5, fine with uneven length
test.df<-as.data.frame(tapply(beam_gw$BEAM,beam_gw$GW,list));str(test.df) #produces 4x13, stalls with uneven length
test.unstack<-unstack(data.frame(beam_gw,beam_gw$GW));str(test.unstack) #produce 4x13 but heading were 2x!,fine with uneven length

summary(factor(beam_gw$GW)) #produce repeat factors!

wide<-gw_beam;long<-beam_gw;unstack(data.frame(long,long$GW))# produce weired result

head(Indometh.wide<-reshape(Indometh, v.names = "conc", idvar = "Subject",timevar = "time", direction = "wide"))
test.aggr<-aggregate(Indometh$con,by=list(Indometh$Subject),list)
print(long2wide<-reshape(long,v.names="BEAM",idvar='GW',timevar="GW", direction="wide"))

table(beam_gw)
table(1:3,2:4)
data.frame(table(1:3,2:4))
data.frame(1:3,2:4)
table(data.frame(1:3,2:4))
table(c(1,1,1,2,2,3,4))
myobj <- structure(list(a = 1, b = 2), class = "myclass")
print.myclass <- function(x, ...) cat("A: ", x$a, " B: ", x$b, "\n", sep = "")
myobj
`%.%` <- function(o, a) attr(o, as.character(substitute(a))) 

########Surprising! Date Not working!############
R>as.POSIXct(Sys.Date())
[1] "2014-11-23 19:00:00 EST"
R>as.POSIXlt(Sys.Date())
[1] "2014-11-24 UTC"
R>Sys.Date()
[1] "2014-11-24"


library(maps)
state

states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))
choro <- merge(states, arrests, by = "region")

xmajor <- c(0, 10, 20, 30)
xminor <- as.vector(outer(1:10, 10^c(-1, 0)))
ymajor <- c(0, 1000, 5000, 10000)
yminor <- as.vector(outer(1:10, 10^c(2,3,4)))
dplot <- ggplot(subset(diamonds, color %in% c("D","E","G","J")),
aes(carat, price, colour = color)) +
scale_x_log10(breaks = xmajor, labels = xmajor, minor = xminor) +
scale_y_log10(breaks = ymajor, labels = ymajor, minor = yminor) +
scale_colour_hue(limits = levels(diamonds$color)) +
opts(legend.position = "none")

plot(-1:1, -1:1, type = "n", xlab = "Re", ylab = "Im")
K <- 16; text(exp(1i * 2 * pi * (1:K) / K), col = 2)

ggplot(mtcars) +
  geom_histogram(aes(factor(cyl), fill=factor(cyl))) +
  scale_fill_brewer(palette="Set1")

###################merge join###################################
if(!require(data.table)){install.packages("data.table")}
 df1 = data.frame(CustomerId=c(1:6),Product=c(rep("Toaster",3),rep("Radio",3)))
 df2 = data.frame(CustomerId=c(2,4,6),State=c(rep("Alabama",2),rep("Ohio",1)))
 dt1<-data.table(df1,  key="CustomerId") 
 dt2<-data.table(df2, key="CustomerId")
 joined.dt1.dt.2<-dt1[dt2]

if(!require(plyr)){install.packages("plyr")}
join(df1, df2,type="inner")

if(!require(dplyr)){install.packages("dplyr")} #no left_join nor right_join
intersect(df1$CustomerId,df2$CustomerId)


merge(dt1,dt2,by="CustomerId", all.x=TRUE)



df1$CustomerId <- as.numeric(df1$CustomerId)
df2$CustomerId <- as.numeric(df2$CustomerId)


#inner
inner_join(df1, df2)
#left outer
left_join(df1, df2)
#right outer (just reverse argument order)
left_join(df2, df1)
df1 = data.frame(CustomerId=c(1:10),
             Hobby = c(rep("sing", 4), rep("pinppong", 3), rep("hiking", 3)),
             Product=c(rep("Toaster",3),rep("Phone", 2), rep("Radio",3), rep("Stereo", 2)))

df2 = data.frame(CustomerId=c(2,4,6, 8, 10),State=c(rep("Alabama",2),rep("Ohio",1),   rep("Cal", 2)),
             like=c("sing", 'hiking', "pingpong", 'hiking', "sing"))

df3 = merge(df1, df2, by.x=c("CustomerId", "Hobby"), by.y=c("CustomerId", "like"))
