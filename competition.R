library(ROracle) #install.packages("ROracle",type='source')
drv <- dbDriver("Oracle")
conn <- dbConnect(drv, "echiu", "whse4Emily","(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(COMMUNITY=tcp.world)(PROTOCOL=TCP)(Host=139.85.128.102)(Port=1525)))(CONNECT_DATA=(SID=whse)))" )
#looping
getChunks<-function(x) {
    qry1<-paste0("select '", x, "' as AccountNumber from DUAL", collapse=" union all ")
    qryStr<-paste0('select r.AccountNumber, s.DTV_CBL,s.DTV_FBR from SITE_COMPETITIVE_OPTIONS_MAP s right join (', qry1, ') r on r.AccountNumber=s.SAN')
    usageResult<-dbGetQuery(conn,qryStr)
}

getWhole<-function(x) {   
    breaks<-length(x)/1000 # maximum 1000 entries at a time
    id.chunks<-split(x,seq(breaks))
    result.list <- lapply(id.chunks, getChunks)
    combined.results <- do.call(rbind,result.list) #Approach 3 with split and rbind, fine!
}

system.time(competitionResult<-getWhole(qryResult$SITE_NAME)) ;dbDisconnect(conn)#441
save(competitionResult,file='/mnt/R/data/EOC/competitionResult')

table(competitionResult$DTV_FBR)