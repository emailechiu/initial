#************ROracle Oracle only: Linux+W64 *****************
library(ROracle) #install.packages("ROracle",type='source')
drv <- dbDriver("Oracle")
conn <- dbConnect(drv, "echiu", "whse4Emily","(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(COMMUNITY=tcp.world)(PROTOCOL=TCP)(Host=139.85.128.102)(Port=1525)))(CONNECT_DATA=(SID=whse)))" )
stoprate<-dbGetQuery(conn,"select PROCESS_DAY,BEAM_ID as BEAM,TYPE2,TYPE3,TYPE10,TERMCNT_FROM_WAREHOUSE as Nom,HT1100_CNT as Denom, TERMCNT_FROM_WAREHOUSE/HT1100_CNT as Rate from QA_REBOOT_NOM join QA_REBOOT_DENOM on COLLECTION_START_TIME_EST=PROCESS_DAY and BEAM_ID=ABSOLUTE_BEAM_ID
where PROCESS_DAY>=SYSDATE-31 order by PROCESS_DAY,BEAM")
colnames(stoprate)<-c("Date","BEAM","Type2","Type3","Type10","Nom","Denom","Rate")
d <- dbReadTable(conn, "QA_CASE_CATEGORY")
dbDisconnect(conn)

#************RJDBC Oracle: Linux+W64+W32*****************
library(RJDBC)
if (.Platform$OS.type=="unix") drv <- JDBC("oracle.jdbc.OracleDriver", classPath="/usr/lib/oracle/12.1/client64/lib/ojdbc7.jar", " ") else drv <- JDBC("oracle.jdbc.OracleDriver", classPath="C:\\OraHome_1\\ojdbc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@139.85.128.102:1525:whse", "echiu", "whse4Emily")
e <- dbReadTable(conn, "QA_CASE_CATEGORY")
dbDisconnect(conn)

#************RJDBC MSSQL: Linux+W64+W32*****************
library(RJDBC)
if (.Platform$OS.type=="unix") drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", classPath="/etc/sqljdbc_3.0/sqljdbc4.jar") else drv<-JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", classPath="C:\\Program Files\\R\\R-3.1.1\\library\\RJDBC\\java\\sqljdbc4.jar")
conn <- dbConnect(drv, "jdbc:sqlserver://expbitssql.hughes.com", "BLRptAcct", "ReadM*0nly")
f <- dbReadTable(conn, "information_schema.tables")
dbDisconnect(conn)

#************RODBC Oracle: W32*****************
require(RODBC)
conn<-odbcConnect("whseprd.world",uid='echiu',pwd='whse4Emily')
g <- sqlQuery(conn, "select * from QA_CASE_CATEGORY")
close(conn)

#************RODBC MSSQL: W32*****************
require(RODBC)
conn<-odbcConnect("SQLEVAL",uid='echiu',pwd='P@ssw0rd2')
g <- sqlQuery(conn, "select top 10 * from etl.dbo.PS_SITE");g
close(conn)


#************RODBC Oracle*****************
require(RODBC)#|| install.packages("RODBC")
myconn <-odbcConnect("SQLEVAL")
#odbcGetInfo(myconn) 
#sqlTables(myconn, schema="dbo")    
#sqlQuery(myconn, "select * from sys.databases")  
site<-sqlQuery(myconn, "select datediff(day,Tech_earliest, RPT_DATE_DT) as TECH2CHURN, INSTALL2NOW, INSTALL2CHURN,LAST_IVR_DATE,RPT_DATE_DT from etl.dbo.PS_SITE") 
par(mfrow=c(2,1))
site670<-sqlQuery(myconn, "select datediff(day,Tech_earliest, RPT_DATE_DT) as TECH2CHURN, s.INSTALL2NOW, s.INSTALL2CHURN, TAD_DESCRIPTION_SUMMARY from etl.dbo.PS_SITE s join etl.dbo.PS_CASE c on s.SITE_NAME=c.SITE_NAME and Tech_earliest = Case_OPENED_DATE 
where TAD_DESCRIPTION_SUMMARY!='Account, General Information, RMA' and left(TAD_DESCRIPTION_SUMMARY,6)!='Cancel' --and left(TAD_DESCRIPTION_SUMMARY,4)!='OOSB'
and  (datediff(day,Tech_earliest,RPT_DATE_DT) >=0 or RPT_DATE_DT is NULL)
and Install2Now>=670") 
siteall<-sqlQuery(myconn,"select datediff(day,Tech_earliest, RPT_DATE_DT) as TECH2CHURN, s.INSTALL2NOW, s.INSTALL2CHURN, s.SITE_NAME, s.LAST_IVR_DATE, RPT_DATE_DT, REASON, cast(Tech_earliest as date) as TECH_EARLIEST, TAD_DESCRIPTION_SUMMARY from etl.dbo.PS_SITE s join etl.dbo.PS_CASE c on s.SITE_NAME=c.SITE_NAME and Tech_earliest = Case_OPENED_DATE")
siteEC<-sqlQuery(myconn,"select datediff(day,Tech_earliest, RPT_DATE_DT) as TECH2CHURN,INSTALL2NOW,INSTALL2CHURN,SITE_NAME, RPT_DATE_DT, cast(Tech_earliest as date) as TECH_EARLIEST from etl.dbo.PS_SITE where LAST_IVR_DATE <'2012-10-31' ")
attach(site670)
detach(site670)
par(mfrow=c(2,2));
hist(site$TECH2CHURN);hist(site$INSTALL2CHURN[INSTALL2NOW>=670]);
hist(site670$TECH2CHURN);hist(site670$INSTALL2CHURN)
hist(siteall$RPT_DATE_DT, breaks=30);hist(siteall$LAST_IVR_DATE, breaks=30);
hist(siteall$RPT_DATE_DT[siteall$LAST_IVR_DATE<'2012-10-30'],freq=TRUE,breaks=30)
hist(siteall$RPT_DATE_DT[siteall$LAST_IVR_DATE<'2012-11-30' & siteall$LAST_IVR_DATE>='2012-10-31'],freq=TRUE,breaks=30)
hist(siteall$RPT_DATE_DT[siteall$LAST_IVR_DATE<'2012-12-31' & siteall$LAST_IVR_DATE>='2012-11-30'],freq=TRUE,breaks=30)
library(arules)
library(arulesViz)
apriori(siteall[siteall$LAST_IVR_DATE<'2012-10-31'])
summary(siteall)
barplot(site$INSTALL2CHURN[INSTALL2NOW>=670])
barplot(site670$TAD_DESCRIPTION_SUMMARY)
close(myconn)

#************JDBC connection*****************
library(RJDBC,logical.return=T)||install.packages("RJDBC")
search()
.jinit()
.jaddClassPath("C:\\Program Files\\R\\R-3.1.1\\library\\RJDBC\\java\\sqljdbc4.jar")
.jaddClassPath("C:\\Program Files\\R\\R-3.1.1\\library\\RJDBC\\java\\RJDBC.jar")
.jclassPath()
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver")  
conn <- dbConnect(drv, "jdbc:sqlserver://echiult7\\SQLEX", "echiu", "P@ssw0rd2") #SOCKET time out
#then build a query and run it
sqlText <- paste("select * from db_test.dbo.tb_test")
queryResults <- sqlQuery(myconn, sqlText)  

#************Data Types*****************
myvector <- c(1:9,2,5)
mymatrix <- matrix(1:8, 2)
yourmatrix <-matrix(0:15:2,2)
print(mymatrix+yourmatrix)
mylist <- list(3, "Interesting", 4+5)
named(mylist)
mytable <-read.table("C:\\Users\\echiu\\Downloads\\Serial.csv") #unsuccessful
mytable <-read.csv("C:\\Users\\echiu\\Downloads\\R.csv")
print(mytable)
print(mytable$Serial)
print(mytable[1:3,])
print(mytable[,1:3])
history()
#sqlQuery(myconn,'CREATE TABLE db_test.dbo.Serial  ("CaseID" int, "CreationDate" varchar(255), "ClosedDateTime" varchar(255), "HNSSite_ID" varchar(255), "Serial" int)')
sqlQuery(myconn,'CREATE TABLE db_test.dbo.Serial  (CaseID int, CreationDate varchar(255), ClosedDateTime varchar(255), HNSSite_ID varchar(255), Serial int)')
sqlSave(myconn, mytable,tablename="db_test.dbo.mytable",rownames=FALSE)
sqlTables(myconn)

#*************Add Data to SQL**********************
data(USArrests)
sqlSave(myconn, USArrests, rownames = "state", addPK = TRUE)
sqlDrop(myconn, "USArrests")

#*************Spreadsheet**********************
myexcel <-odbcConnectExcel("C:\\Users\\echiu\\Downloads\\Friday 8_22_14_ Data HT1100 No Browse.csv")
sqlTables(myexcel)
sh1 <- sqlQuery(myexcel, "select * from [Sheet1$]")
sh1 <- sqlQuery(myexcel, "select * from [Case ID and Call creation$]")
sh1 <- sqlFetch(myexcel, "Sheet2")
close(myconn)

#************Useful table manupulation commands**********
library(RODBC)
myconn <-odbcConnect("SQLEVAL")
resetstat<-sqlQuery(myconn,"select * from etl.dbo.JUPITER_RESET_NOMINATOR4")
summary(resetstat)
dim(resetstat)
#eclat not useful here
#apriori not useful here
#inspect only works on itemset and rules?
#tidList only used for transactions
#image useful for tidList
cor
lm
avPlots
qqPlot
influencePlot
plot
#outlierTest used on a model as a result of lm
t.test

#**************Quality testing
attach(resetstat)
type10<-qcc.groups(TYPE10,COLLECTION_START_TIME_EST) #expect every day to be the same
dim(type10) #should be 52 groups with 59 samples
obj<-qcc(type10[1:25,],type="xbar")
summary(obj)
obj<-qcc(type10[1:25,],type="xbar",newdata=type10[26:59,])
plot(obj,id.n=3)


#***********Useful statistical values to check
#P value after t.test






