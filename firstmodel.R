#************Get outside Data *****************
library(ROracle) #install.packages("ROracle",type='source')
drv <- dbDriver("Oracle")
conn <- dbConnect(drv, "echiu", "whse4Emily","(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(COMMUNITY=tcp.world)(PROTOCOL=TCP)(Host=139.85.128.102)(Port=1525)))(CONNECT_DATA=(SID=whse)))" )

# Get Site data ExpressRepair
qryStr<-'select * from QA_JUPITER_SITE where INSTALL2NOW>30*25'
qryStr<-"select * from QA_JUPITER_CASE where LAST_IVR_DATE<to_date('2013-01-19', 'YYYY-MM-DD')"
qryStr<-'select q.*, ap.PURCHASE_START_DATE, (case when ap.PRODUCT_ID is not NULL then 1 else 0 end) as EXPRESS_REPAIR
from ACCOUNT_PRODUCT ap join ACCOUNT a on (ap.PRODUCT_ID=2095989835 or ap.PRODUCT_ID=2095991371)
 and ap.ACCOUNT_ID=a.ACCOUNT_ID
right join QA_JUPITER_SITE q on a.ACCOUNT_NUMBER=q.SITE_NAME where INSTALL2NOW > 30*25'
qryStr<-'select * from ACCOUNT_PROdUCT ap where (ap.PRODUCT_ID=2095989835 or ap.PRODUCT_ID=2095991371) and rownum=1 '
qryStr<-'select * from JUPITER_USAGE_DAILY where rownum=1'; headerUsage<-dbGetQuery(conn,qryStr)
qryStr<-"select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME='JUPITER_USAGE_DAILY'";header<-dbGetQuery(conn,qryStr)

start<-proc.time();qryResult<-dbGetQuery(conn,qryStr);timeElapsed <-proc.time()-start; timeElapsed #3,2,91
# Get Case data
qryResult$CHURNED <-!is.na(qryResult$INSTALL2CHURN)
qryResult$INSTALL2CHURN[is.na(qryResult$INSTALL2CHURN)]=999
qryResult$CHURN_RANGE<-cut(qryResult$INSTALL2CHURN,c(-1,30*24,30*25,999))
trainingSet<-qryResult[qryResult$INSTALL2NOW>30*26,]
verifySet<-qryResult[qryResult$INSTALL2NOW<=30*26,]
table(qryResult$EXPRESS_REPAIR,cut(qryResult$INSTALL2CHURN,breaks=c(-1,30*24,30*25,999)))
boxplot(qryResult$INSTALL2CHURN~qryResult$EXPRESS_REPAIR)

start<-proc.time();qryCase<-dbGetQuery(conn,qryStr);timeElapsed <-proc.time()-start; timeElapsed #102,86,1824
caseMinusDetails<-qryCase[,-12]

#Generate binary tree model
library(rpart)
par(mfrow = c(1,1), xpd = NA)
trainingSet.rpart<-rpart(CHURNED ~ EXPRESS_REPAIR,method="anova",cp=0.001,data=trainingSet)
plot(trainingSet.rpart);
text(trainingSet.rpart, use.n=TRUE, all=TRUE, cex=.8);
verifySet.predict<-predict(trainingSet.rpart,data=verifySet)
print(trainingSet.rpart);post(trainingSet.rpart)

verifySet.rpart<-rpart(CHURNED ~ EXPRESS_REPAIR,method="anova",cp=0.001,data=verifySet)
plot(verifySet.rpart);
text(verifySet.rpart, use.n=TRUE, all=TRUE, cex=.8);

#Generate GLM model and prediction

qryModel<-glm(CHURNED~as.factor(EXPRESS_REPAIR),data=trainingSet,family=binomial);summary(qryModel)
verifySet$prediction<-predict(qryModel,verifySet)

# Generate plot gain chart without getting the original set into the right format
gain<-cumsum(verifySet[order(verifySet$prediction,decreasing=TRUE),]$CHURNED)
plot(1:nrow(verifySet)/nrow(verifySet),gain/max(gain));abline(0,1);abline(0,nrow(verifySet)/max(gain))
ks <- max(gain/max(gain)-1:nrow(verifySet)/nrow(verifySet));ks

# Generate and plot Gain chart;verifySet<-verifySet[,-ncol(verifySet)]
verifySet[order(-verifySet$prediction),ncol(verifySet)+1]<-1:nrow(verifySet);colnames(verifySet)[ncol(verifySet)]="rank"
verifySet<-verifySet[order(verifySet$rank),]
verifySet$target<-cumsum(verifySet$CHURNED)
plot(verifySet$rank, verifySet$target);abline(0,max(verifySet$target)/max(verifySet$rank))
head(verifySet)

#/ Verify with table
verifyTable<-as.data.frame(table(verifySet$prediction,verifySet$CHURNED),stringAsFactors=FALSE)
summary(verifyTable);str(verifyTable);verifyTable

# Save and Load data
save(qryResult,file="/mnt/R/data/1/qryResult")
save(trainingSet,file="/mnt/R/data/1/trainingSet")
save(verifySet,file="/mnt/R/data/1/verifySet")
save(caseMinusDetails,file="/mnt/R/data/1/caseMinusDetails")
load("/mnt/R/data/1/qryResult")
load("/mnt/R/data/1/trainingSet")
load("/mnt/R/data/1/verifySet")
load("/mnt/data/1/caseMinusDetails")

dbDisconnect(conn)