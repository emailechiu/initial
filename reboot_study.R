library(openxlsx); library(reshape);#install.packages("reshape")
par(mfrow=c(1,2))
rebootStudy<-read.xlsx("/mnt/R/data/reboot_study/StateCodes.xlsx",sheet=1)
rebootStudy<-melt(rebootStudy,id=1)
rebootStudy<-cbind(RANDOM="REBOOT",rebootStudy)
norebootStudy<-read.xlsx("/mnt/R/data/reboot_study/StateCodes.xlsx",sheet=2)
norebootStudy<-melt(norebootStudy,id=1)
norebootStudy<-cbind(RANDOM="NO REBOOT",norebootStudy)
rebootStudy<-rbind(rebootStudy,norebootStudy)
colnames(rebootStudy)[3:4]=c("Time","statecode")
rebootStudy$Time<-convertToDateTime(as.numeric(as.character(rebootStudy[,3])))
#rebootStudy$Time<-as.POSIXlt(as.numeric(as.character(rebootStudy[,2])))
#rebootStudy$Time<-as.POSIXct(as.numeric(as.character(rebootStudy[,2])))
statecodeTable<-table(rebootStudy$statecode)
reboot<-sort(statecodeTable[statecodeTable>quantile(statecodeTable,0.75)])
barplot(sort(statecodeTable[statecodeTable>quantile(statecodeTable,0.75)]),col=rainbow(20))
rebootTable<-table(rebootStudy$SAN, rebootStudy$statecode)
#write.xlsx(rebootStudy,"/mnt/R/data/reboot_study/State.xlsx")


#************Get San Data *****************
library(ROracle) #install.packages("ROracle",type='source')
drv <- dbDriver("Oracle")
conn <- dbConnect(drv, "echiu", "whse4Emily","(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(COMMUNITY=tcp.world)(PROTOCOL=TCP)(Host=139.85.128.102)(Port=1525)))(CONNECT_DATA=(SID=whse)))" )
# Get Site data
qryStr<-'select * from QA_REBOOT_SLC_SAN_1031 a join PS_CASE b on a.SAN=b.SITE_NAME join QA_CASE_CATEGORY'
qryStr<-'select RANDOM,SAN,BEAM_ID from QA_REBOOT_SLC_SAN_1031'
start<-proc.time();sanResult<-dbGetQuery(conn,qryStr);timeElapsed <-proc.time()-start; timeElapsed #<1
dbDisconnect(conn)

#************Get Call Data *****************
library(ROracle) #install.packages("ROracle",type='source')
drv <- dbDriver("Oracle")
conn <- dbConnect(drv, "echiu", "whse4Emily","(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(COMMUNITY=tcp.world)(PROTOCOL=TCP)(Host=139.85.128.102)(Port=1525)))(CONNECT_DATA=(SID=whse)))" )
qryStr<-"select a.RANDOM, a.SAN,a.BEAM_ID,b.CASE_OPENED_DATE,b.TAD_DESCRIPTION_SUMMARY,b.CASE_ID from QA_REBOOT_SLC_SAN_1031 a join PS_CASE b on a.SAN=b.SITE_NAME  and a.RANDOM!='Alpha'
and b.CASE_OPENED_DATE between to_date('2014-12-02','YYYY-MM-DD') and to_date('2014-12-20','YYYY-MM-DD')
join QA_CASE_CATEGORY c on b.TAD_DESCRIPTION_SUMMARY = c.TAD_DESCRIPTION_SUMMARY
where c.POST_BRM_CATEGORY = 'Service Reliability' or c.POST_BRM_CATEGORY='Service Performance' "
start<-proc.time();sanResult<-dbGetQuery(conn,qryStr);timeElapsed <-proc.time()-start; timeElapsed #41
dbDisconnect(conn);
callResult<-cbind(sanResult,statecode="TECHCALL")
colnames(callResult)<-c("RANDOM","SAN","BEAM_ID","Time","TAD_DESCRIPTION_SUMMARY","CASE_ID","statecode")
callTable<-table(callResult$SAN,callResult$statecode)
# 
# **************Union*************************
# rbined<-rbind(callResult[,c(1,2,4,6)],rebootStudy)
# rbinedTable<-table(rbined$SAN,rbined$statecode);head(rbinedTable)
# rbinedDT<-as.data.frame.matrix(rbinedTable)
# rbinedModel<-lm(TECHCALL~.,data=rbinedDT[rbinedDT$TECHCALL>0,]);str(rbinedModel);summary(rbinedModel)
# rbinedTable[rbinedTable[,1]>0]
# 
# ***************Merge************************
# combined<-merge(callResult(,c(1,2,4,6)),rebootStudy)
# sanTable<-table(combined$SAN, combined$statecode);head(sanTable)
# san<-sort(sanTable[statecodeTable>quantile(sanTable,0.75)])
# head(sanTable)

***************data.table rolling join*************
callDT<-as.data.table(callResult)
statecodeDT<-as.data.table(rebootStudy)
system.time(setkey(callDT,SAN,Time))
system.time(setkey(statecodeDT,SAN,Time))
joined<-statecodeDT[callDT[,c(-1,-ncol(callDT)),with=FALSE],roll=TRUE]
write.xlsx(, "/mnt/R/data/reboot_study/call_statecode.xlsx")
table(joined$statecode) #which statecode contributes to call most
stateDF<-as.data.frame.matrix(table(rebootStudy$SAN,rebootStudy$statecode)) #which user is stuck longest
head(stateDF[order(-stateDF$"24.1.1"-stateDF$"13.2.2"-stateDF$"13.1.1"),])

l<-list("correlated"=joined[order(RANDOM,Time) & !is.na(RANDOM)],"SAN"=stateDF[order(-stateDF$"24.1.1"-stateDF$"13.2.2"-stateDF$"13.1.1"),])
write.xlsx(l,"/mnt/R/data/reboot_study/call_statecode.xlsx")
par(mfrow=c(1,4))
plot(cumsum(table(stateDF$"0.0.0")))
plot(table(stateDF$"0.0.0"))
plot(sttable(stateDF$"0.0.0"))
