setwd('/mnt/R/proj/RebootStudy')
if (!exists('getWhole',mode='function')) source('funOracle.R')
require(ggplot2)
require(scales)

###################stop rate##################################################
beam_gw<-read.csv("BEAM_GW_LKUP.csv",stringsAsFactor=FALSE);
beam_gw<-do.call(rbind,lapply(split(beam_gw,beam_gw$GW),transform,Color = rank(BEAM,ties.method = "first")))

stoprateQry<-"select COLLECTION_START_TIME_EST as PROCESS_DAY,ABSOLUTE_BEAM_ID as BEAM,sum(TYPE2),sum(TYPE3),sum(TYPE10),sum(TERMCNT_FROM_WAREHOUSE) as Nom,sum(HT1100_CNT) as Denom, sum(TERMCNT_FROM_WAREHOUSE)/sum(HT1100_CNT) as Rate from QA_REBOOT_NOM where COLLECTION_START_TIME_EST>=SYSDATE-31 
group by COLLECTION_START_TIME_EST,ABSOLUTE_BEAM_ID order by COLLECTION_START_TIME_EST,ABSOLUTE_BEAM_ID"
stoprate<-funOracle(stoprateQry)
colnames(stoprate)<-c("Date","BEAM","Type2","Type3","Type10","Nom","Denom","Rate")


###################call rate##################################################
callrateQry<-"select trunc(CASE_OPENED_DATE) as CASE_DATE, HNSBEAMID as BEAM,CATEGORY as CATEGORY,count(*) as CALL_CNT
from PS_CASE c join QA_JUPITER_SITE s on c.HNS_SITE_ID=s.SITE_NAME join QA_CASE_CATEGORY cat on c.TAD_DESCRIPTION_SUMMARY=cat.TAD_DESCRIPTION_SUMMARY where CASE_OPENED_DATE>=SYSDATE-31 group by trunc(CASE_OPENED_DATE), HNSBEAMID, CATEGORY"
call_rate<-funOracle(callrateQry)

call_rate.rollup<-with(call_rate,aggregate(CALL_CNT,by=list(CASE_DATE,BEAM),FUN=sum))
colnames(call_rate.rollup)<-c("CASE_DATE","BEAM","CALL_CNT.Total")
call_rate$CATEGORY<-gsub('customer','Customer',call_rate$CATEGORY)
call_rate$CATEGORY<-gsub('Other Miscellaneous ','No Call',call_rate$CATEGORY)
call_rate$CATEGORY<-gsub('fom HNS','from HNS',call_rate$CATEGORY)
call_rate$Cat <- factor(call_rate$CATEGORY,levels=c("Billing","Communication from HNS","Cost of Service","Customer Service","Service Performance - FAP","Service Performance - Slow","Service Reliability","Retention","No Call"),labels=c("Bil","Com","Cost","Cust","Perf_FAP","Perf_Slow", "Rel","Ret","Oth")) 

library(reshape)
call_rate<-call_rate[,!names(call_rate) %in% c("CATEGORY")] #call_rate<-call_rate[,-3]
call_rate<-reshape(call_rate,idvar=c("BEAM","CASE_DATE"),timevar="Cat",direction="wide")
#sum(call_rate[call_rate$CASE_DATE==as.POSIXct('2014-11-25') & call_rate$Cat=='Perf',]$CALL_CNT)
call_rate<-merge(call_rate.rollup,call_rate,by=c("CASE_DATE","BEAM"))
###################churn rate##################################################

churnrateQry<-"select trunc(RPT_DATE_DT) as CHURN_DATE, HNSBEAMID as BEAM,count(*) as CHURN_CNT from QA_JUPITER_SITE
where RPT_DATE_DT>=SYSDATE-31 group by trunc(RPT_DATE_DT),HNSBEAMID"
churn_rate<-funOracle(churnrateQry)
###################all rate##################################################
#stoprate[,1:8][order(stoprate$Date),]
allrate<-merge(stoprate,call_rate,by.x=c("BEAM","Date"),by.y=c("BEAM","CASE_DATE"),all=TRUE);#nrow(allrate)
#allrate[,1:3][order(allrate$Date),]
allrate<-merge(allrate,churn_rate,by.x=c("BEAM","Date"),by.y=c("BEAM","CHURN_DATE"),all=TRUE);#nrow(allrate)
allrate<-merge(beam_gw,allrate,by="BEAM")
#allrate<-subset(allrate,Denom>=100);#This is wrong allrate<-allrate[allrate$Denom>=100,]
pStop<-ggplot(allrate)+theme(legend.position = "none")+aes(as.Date(Date),Rate,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
p10<-ggplot(allrate)+theme(legend.position = "none")+aes(as.Date(Date),Type10/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
p2<-ggplot(allrate)+theme(legend.position = "none")+aes(as.Date(Date),Type2/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
p3<-ggplot(allrate)+theme(legend.position = "none")+aes(as.Date(Date),Type3/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
pDenom<-ggplot(allrate)+theme(legend.position = "none")+aes(as.Date(Date),Denom,group=BEAM,colour=factor(Color))+facet_wrap(~GW_LOC,ncol=2)+geom_text(aes(label=BEAM),size=3,angle=45)+scale_colour_brewer(palette="Set1")
pdf("/var/www/html/stoprate.pdf",paper="USr",width=11,height=8)
pDenom;pStop;p10;p2;p3;
dev.off()

pCalls<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Total/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)#+ggtitle('Reliability')
pRel<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Rel/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)#+ggtitle('Reliability')
pPerf_fap<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Perf_FAP/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
pPerf_slow<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Perf_Slow/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
pRet<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Ret/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
pCust<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Cust/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
pCom<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Com/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
pCost<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Cost/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
pBil<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Bil/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
#pOth<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CALL_CNT.Oth/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
pdf("/var/www/html/callrate.pdf",paper="USr",width=11,height=8)
pCalls;pRel;pPerf_fap;pPerf_slow;pRet;pCust;pCom;pCost;pBil
dev.off()

pChurn<-ggplot(allrate)+theme(legend.position="none")+aes(as.Date(Date),CHURN_CNT/Denom,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+facet_wrap(~GW_LOC,ncol=1)+geom_text(aes(label=BEAM),size=3)+scale_colour_brewer(palette="Set1")+scale_y_continuous(labels=percent)
pdf("/var/www/html/churnrate.pdf",paper="USr",width=11,height=8)
pChurn
dev.off()


#p1 does not work without converting to as.Date, it will have non-weekly breaks and missing 1 day! 
#ggplot(stoprate)+aes(Date,Rate,group=BEAM,colour=factor(Color))+geom_line(aes(label=as.character(BEAM)))+scale_x_datetime(breaks = date_breaks("1 day"),labels=date_format("%d"))
#p<-p+scale_y_continuous(limits=c(0,0.5))
#p+scale_x_date(labels = date_format("%m/%d"),breaks=pretty_breaks())
#save(stoprate,file="sr.Rdata")
#dput(stoprate,file="stoprate.txt")
#dump("stoprate",file="sr.txt")
#ggsave("stoprate.pdf", p, width=11, height=8.5)

