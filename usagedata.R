# Git!
#************Get outside Data *****************
library(ROracle) #install.packages("ROracle",type='source')
drv <- dbDriver("Oracle")
conn <- dbConnect(drv, "echiu", "whse4Emily","(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(COMMUNITY=tcp.world)(PROTOCOL=TCP)(Host=139.85.128.102)(Port=1525)))(CONNECT_DATA=(SID=whse)))" )
qry1<-paste0("DAY_OF_YEAR=","'","20150101","'")
#ids<-paste0("'",qryResult$SITE_NAME,"'",collapse=",") 
#qry2<-paste0('DEVICE_ID in (', ids,')')
#qryStr<-paste0('select DEVICE_ID, DAY_OF_YEAR, ATIME_BUCKET_SIZE_R,COS4_ATIME_BUCKET_SIZE_R,OFFPEAK_BUCKET_SIZE_R,COS4_OFFPEAK_BUCKET_SIZE_R,TOKEN_BUCKET_SIZE_R,AGG_BYTES_UPLOADED,AGG_BYTES_DOWNLOADED from JUPITER_USAGE_DAILY where ',qry1, ' and ',qry2 ); #Approach 1 with in, list>1000 elements rejected
qry3<-paste0("select '", qryResult$SITE_NAME, "' as DEVICE_ID from dual ", collapse=" union all ")
qryStr<-paste0('select DEVICE_ID, DAY_OF_YEAR, ATIME_BUCKET_SIZE_R,COS4_ATIME_BUCKET_SIZE_R,OFFPEAK_BUCKET_SIZE_R,COS4_OFFPEAK_BUCKET_SIZE_R,TOKEN_BUCKET_SIZE_R,AGG_BYTES_UPLOADED,AGG_BYTES_DOWNLOADED from JUPITER_USAGE_DAILY j join (', qry3, ') r using (DEVICE_ID) where j.',qry1 ); #Approach 2 with union, ORA-03113 end-of-file communication channel

#looping
getChunks<-function(x) {
    qry1<-paste0("DAY_OF_YEAR=","'","20150101","'")
    qry3<-paste0("select '", x, "' as DEVICE_ID from dual ", collapse=" union all ")
    qryStr<-paste0('select DEVICE_ID, DAY_OF_YEAR, ATIME_BUCKET_SIZE_R,COS4_ATIME_BUCKET_SIZE_R,OFFPEAK_BUCKET_SIZE_R,COS4_OFFPEAK_BUCKET_SIZE_R,TOKEN_BUCKET_SIZE_R,AGG_BYTES_UPLOADED,AGG_BYTES_DOWNLOADED from JUPITER_USAGE_DAILY j join (', qry3, ') r using (DEVICE_ID) where j.',qry1 );
    usageResult<-dbGetQuery(conn,qryStr);
}

getWhole<-function(x) {
conn <- dbConnect(drv, "echiu", "whse4Emily","(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(COMMUNITY=tcp.world)(PROTOCOL=TCP)(Host=139.85.128.102)(Port=1525)))(CONNECT_DATA=(SID=whse)))" )
breaks<-length(x)/1000 # maximum 1000 entries to Oracle at a time
id.chunks<-split(x,seq(breaks))
result.list <- lapply(id.chunks, getChunks)
combined.results <- do.call(rbind,result.list) #Approach 3 with split and rbind, fine!
dbDisconnect(conn)
return(combined.results)
}
system.time(usage<-getWhole(qryResult$SITE_NAME)) #2

qryStr<-"select DEVICE_ID, DAY_OF_YEAR, ATIME_BUCKET_SIZE_R,COS4_ATIME_BUCKET_SIZE_R,OFFPEAK_BUCKET_SIZE_R,COS4_OFFPEAK_BUCKET_SIZE_R,TOKEN_BUCKET_SIZE_R,AGG_BYTES_UPLOADED,AGG_BYTES_DOWNLOADED from JUPITER_USAGE_DAILY j
join QA_JUPITER_SITE on DEVICE_ID=SITE_NAME where DAY_OF_YEAR='20150101' and INSTALL2NOW>30*25" #Approach 4 straight
system.time(usageResult<-dbGetQuery(conn,qryStr)) #21
dbDisconnect(conn)

dev.copy(pdf,file="fromscreen.pdf")
dev.off()
