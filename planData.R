sessionInfo()
options(java.parameters="-Xmx32000m")
library(RJDBC)
JdbcPath=if (.Platform$OS.type=="unix") "/etc/sqljdbc_3.0/sqljdbc4.jar" else "C:\\Program Files\\R\\R-3.1.1\\library\\RJDBC\\java\\sqljdbc4.jar"
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", classPath=JdbcPath)
#looping
getChunks<-function(x) {
    qry1<-paste0("select '", x, "' as AccountNumber ", collapse=" union all ")
    qryStr<-paste0('select r.AccountNumber, da.RiskScore,da.ValueScore,da.CreditScore,da.CreditScoreBand, fsp.ServicePremiumCreationDateKey,fsp.ServicePremiumEndDateKey,dd.DealCategoryNameNew,dd.DealDescription from (select AccountKey, Row_number() over (partition by AccountKey order by ServicePremiumEndDateKey desc) as RowPurchase,DealKey, ServicePremiumCreationDateKey,ServicePremiumEndDateKey from Fact.FactServicePremium) fsp join Dimension.DimAccount da on da.AccountKey = fsp.AccountKey and fsp.RowPurchase=1 join Dimension.DimDeal dd on dd.DealKey = fsp.DealKey right join (', qry1, ') r on r.AccountNumber=da.AccountNumber')
    usageResult<-dbGetQuery(conn,qryStr)
}

getWhole<-function(x) {
    conn <- dbConnect(drv, "jdbc:sqlserver://expbitssql.hughes.com", "BLRptAcct", "ReadM*0nly")
    breaks<-length(x)/1000 # maximum 1000 entries at a time
    id.chunks<-split(x,seq(breaks))
    result.list <- lapply(id.chunks, getChunks)
    combined.results <- do.call(rbind,result.list) #Approach 3 with split and rbind, fine!
    dbDisconnect(conn)
    return(combined.results)
}

system.time(expressRepairResult<-getWhole(qryResult$SITE_NAME)) #441

table(expressRepairResult$DealDescription,expressRepairResult$DealCategoryNameNew)
save(expressRepairResult,file='/mnt/R/data/EOC/expressRepairResult')