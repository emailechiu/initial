funOracle<-function(qryStr,database='ora') {
   if (database=='ora') {
       
    require(ROracle) 
    drv <- dbDriver("Oracle")
    conn <- dbConnect(drv, "echiu", "whse4Emily","(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(COMMUNITY=tcp.world)(PROTOCOL=TCP)(Host=whse)(Port=1525)))(CONNECT_DATA=(SID=whse)))" )
   } else {
       require(RJDBC)
       JdbcPath=if (.Platform$OS.type=="unix") "/etc/sqljdbc_3.0/sqljdbc4.jar" else "C:\\Program Files\\R\\R-3.1.1\\library\\RJDBC\\java\\sqljdbc4.jar"
       drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", classPath=JdbcPath)
       conn <- dbConnect(drv, "jdbc:sqlserver://expbitssql.hughes.com", "BLRptAcct", "ReadM*0nly")
   }
    result<-dbGetQuery(conn,qryStr)
    print(qryStr)
    dbDisconnect(conn)
    return(result)
}

getWhole<-function(x,qryStr,database='ora') {   
    breaks<-length(x)/1000 # maximum 1000 entries at a time
    id.chunks<-split(x,seq(breaks))
    result.list <- lapply(id.chunks, getChunks,qryStr,database)
    combined.results <- do.call(rbind,result.list) #Approach 3 with split and rbind, fine!
}

getChunks<-function(x,baseQryStr,database='ora') {
    if (database=='ora') {
        idStr<-paste0("select '", x, "' as DeviceId from DUAL", collapse=" union all ") }else {
        idStr<-paste0("select '", x, "' as DeviceId", collapse=" union all ")
        }
    qryStr<-paste0("select s.* from (", baseQryStr, ")s join (", idStr, ") r on s.DeviceId=r.DeviceId")
    funOracle(qryStr,database)     
}

whichTab<-function(keyword) {
    qryStr<-paste0("select TABLE_NAME, COLUMN_NAME from ALL_TAB_COLUMNS where COLUMN_NAME like '%",keyword,"%'")
    print(qryStr)
    funOracle(qryStr)
}

whatTab<-function(keyword) {
    qryStr<-paste0("select TABLE_NAME from ALL_TABLES where TABLE_NAME like '%",keyword,"%'")
    print(qryStr)
    funOracle(qryStr)
}

funTrimCsv<-function(infile,outfile,cols) {
    
    system.time(header<-read.csv(infile, header=FALSE, stringsAsFactors=FALSE, nrows=1))
    colNum<-paste0(which(header %in% cols),collapse=',')
    systemCmd<-paste("cut -d, -f",colNum, infile,">",outfile )
    system(systemCmd)
}