#************ROracle Oracle Linux+W64 (W32 needs Rtools and build from source) *****************
library(ROracle) #install.packages("ROracle",type='source') fails!
drv <- dbDriver("Oracle")
conn <- dbConnect(drv, "echiu", "whse4Emily","(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(COMMUNITY=tcp.world)(PROTOCOL=TCP)(Host=whse)(Port=1525)))(CONNECT_DATA=(SID=whse)))" )
stoprate<-dbGetQuery(conn,"select PROCESS_DAY,BEAM_ID as BEAM,TYPE2,TYPE3,TYPE10,TERMCNT_FROM_WAREHOUSE as Nom,HT1100_CNT as Denom, TERMCNT_FROM_WAREHOUSE/HT1100_CNT as Rate from QA_REBOOT_NOM join QA_REBOOT_DENOM on COLLECTION_START_TIME_EST=PROCESS_DAY and BEAM_ID=ABSOLUTE_BEAM_ID
where PROCESS_DAY>=SYSDATE-31 order by PROCESS_DAY,BEAM")
colnames(stoprate)<-c("Date","BEAM","Type2","Type3","Type10","Nom","Denom","Rate")
d <- dbReadTable(conn, "QA_CASE_CATEGORY");d
dbDisconnect(conn)

#************RJDBC Oracle: Linux+W64+W32*****************
library(RJDBC)
JdbcPath=if (.Platform$OS.type=="unix") "/usr/lib/oracle/12.1/client64/lib/ojdbc7.jar" else "C:\\OraHome_1\\ojdbc6.jar"
drv <- JDBC("oracle.jdbc.OracleDriver", classPath=JdbcPath)
conn <- dbConnect(drv, "jdbc:oracle:thin:@139.85.128.102:1525:whse", "echiu", "whse4Emily")
e <- dbReadTable(conn, "QA_CASE_CATEGORY");e
dbDisconnect(conn)

#************RJDBC MSSQL(MykeBass and Mine): Linux+W64+W32*****************
library(RJDBC)
JdbcPath=if (.Platform$OS.type=="unix") "/etc/sqljdbc_3.0/sqljdbc4.jar" else "C:\\Program Files\\R\\R-3.1.1\\library\\RJDBC\\java\\sqljdbc4.jar"
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", classPath=JdbcPath)
conn <- dbConnect(drv, "jdbc:sqlserver://echiult7\\SQLEVAL:1433","BLRptAcct","ReadM*0nly") #databbaseName='etl' does not help, only works from echiult7
#conn <- dbConnect(drv, "jdbc:sqlserver://expbitssql.hughes.com", "BLRptAcct", "ReadM*0nly")
f <- dbReadTable(conn, "information_schema.tables");f
g <- dbGetQuery(conn, "select top 10 * from etl.dbo.PS_SITE");g
dbDisconnect(conn)

#************RODBC Oracle: W32+W64*****************
require(RODBC)
OdbcConn=if (.Platform$r_arch=="i386") "whseprd" else "whseprd64"
conn<-odbcConnect(OdbcConn,uid='echiu',pwd='whse4Emily')
g <- sqlQuery(conn, "select * from QA_CASE_CATEGORY");g
close(conn)

#************RODBC MSSQL: W32+W64 (Linux fails at configure long 77 problem) *****************
require(RODBC)
#OdbcConn<-if (.Platform$r_arch=="i386") "SQLEVAL" else "SQLEVAL64"
#conn<-odbcConnect(OdbcConn,uid='echiu',pwd='P@ssw0rd2') 
#g <- sqlquery(conn, "select top 10 * from etl.dbo.PS_SITE");g
OdbcConn=if (.Platform$r_arch=="i386") "EXPBITSSQL32" else "EXPBITSSQL64"
conn<-odbcConnect(OdbcConn,uid='BLRptAcct',pwd='ReadM*0nly')
g<-sqlFetch(conn,"information_schema.tables");g
close(conn)

#######################csv #####################################
setwd("./data/EOC")
#system("cut -f $(head -1 EOC.csv |sed 's/,/\'$'\n/g' |grep -ni 'Churn_In_Range_IND' |cut -f1 -d:) -d, EOC.csv >premodel.csv")  #for one column
csvfile<-"/mnt/R/input/EOC.csv"
outputfile<-"premodel.csv"
nrows<-197378
colName<-c("AccountNumber", "ActivationDate", "Churn_Week", "Churn_In_Range_IND", "Churn_Before_Range_IND", "Churn_After_Range_IND","Churn_Date", "Express_Repair_PRESENT")
system.time(header<-read.csv(csvfile, header=FALSE, stringsAsFactors=FALSE, nrows=1))
colNum<-paste0(which(header %in% colName),collapse=',')
systemCmd<-paste("cut -d, -f",colNum, csvfile,">",outputfile )
system(systemCmd)
system.time(EOC <-read.csv(outputfile,nrows=197378,stringsAsFactors=FALSE))

########################Excel##########################################
library(openxlsx)
churnOct<-read.xlsx("/mnt/R/data/reboot_study/Sunil.xlsx",sheet=1)
churnNov<-read.xlsx("/mnt/R/data/reboot_study/Sunil.xlsx",sheet=2)
churnDec<-read.xlsx("/mnt/R/data/reboot_study/Sunil.xlsx",sheet=3)
churnwhat<-read.xlsx("/mnt/R/data/reboot_study/morethan1sheet.xlsx",sheet=5)
