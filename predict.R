library(TraMineR)
data(mvad)
library(survival)
library(zoo)
vignette(zoo)

###########Population Survival#############################
require(RODBC)
myconn <-odbcConnect("SQLEVAL")
site<-sqlQuery(myconn, "select datediff(day,Tech_earliest, RPT_DATE_DT) as TECH2CHURN, INSTALL2NOW, INSTALL2CHURN,LAST_IVR_DATE,RPT_DATE_DT from etl.dbo.PS_SITE")
case<-sqlQuery(myconn, "select * from etl.dbo.SITE_HISTORY")

par(mfrow=c(1,5))
attach(site);
	hwouldbeage<-hist(INSTALL2NOW, breaks=30,freq=TRUE,main="Total Count vs Age",plot=FALSE)
attach(site[is.na(RPT_DATE_DT),]);
	hsurvived<-hist(INSTALL2NOW, breaks=30,freq=TRUE,main="Active Count vs Age",plot=FALSE)
attach(site);
	hchurnage<-hist(INSTALL2CHURN, breaks=30,freq=TRUE,main="Churn Count vs ChurnAge",plot=FALSE)
attach(site[!is.na(RPT_DATE_DT),]);
	hinactive<-hist(INSTALL2NOW, breaks=30,main="Churn Count vs Age",plot=FALSE)
	barplot(rbind(hsurvived$counts,hinactive$counts),col = c("mistyrose", "lightcyan"), legend = c('survived','churned'),xlab="INSTALL2NOW",main="Survived and Churned #")
	barplot(rbind(cumsum(hsurvived$counts),cumsum(hinactive$counts)),col = c("mistyrose", "lightcyan"), legend = c('survived','churned'),xlab="INSTALL2NOW",main="Survived and Churned # RunningTotal")
	barplot(rbind(hsurvived$counts/hwouldbeage$counts,hinactive$counts/hwouldbeage$counts),col = c("mistyrose", "lightcyan"), legend = c('survived','churned'),xlab="INSTALL2NOW",main="Survived and Churned %")
hratio<-hwouldbeage;
hratio$counts<-hinactive$counts/hwouldbeage$counts
	plot(hratio,main="Churn Rate vs Age")
hratio$breaks<-rev(hwouldbeage$breaks)
hratio$mids<-rev(hwouldbeage$mids)
hratio$counts<-cumsum(rev(hwouldbeage$counts))
	#plot(hratio,main="Wouldbe subscribers count > AGE")
hratio$counts<-cumsum(rev(hchurnage$counts))+cumsum(rev(hsurvived$counts))
	#plot(hratio,main="Surbscriber Lasting > AGE")
hratio$counts<-rev(hchurnage$counts)/cumsum(rev(hwouldbeage$counts))
	#plot(hratio,main="Churn Rate vs Age")
hratio$counts<-(cumsum(rev(hchurnage$counts))+cumsum(rev(hsurvived$counts)))/cumsum(rev(hwouldbeage$counts))
	plot(hratio,main="Average Probability Reaching Age",xlab='Churn Age')

###########Time Series##################33
library(zoo)
cdata <- aggregate(data["SITE_HISTORY"], by=data[c("SITE_NAME")], FUN=zoo(Call_Type, CASE_OPENED_DATE))
library(doBy)
cdata <- summaryBy(INSTALL2NOW ~ SITE_NAME, FUN=c(length,mean,sd))
attach(case)
cdata<-summarySE( measurevar="INSTALL2NOW", groupvars=c("SITE_NAME"))
attach(site)
library(plyr)
cdata <- ddply(case, c("SITE_NAME"), summarise,N    = sum(!is.na(INSTALL2CHURN)),mean = mean(INSTALL2CHURN, na.rm=TRUE),sd   = sd(INSTALL2CHURN, na.rm=TRUE),se   = sd / sqrt(N) )c

site_history<-split(cars
split(case$CallType, case$SITE_NAME)
par(mfrow=c(1,3))
attach(case[case$CallType=='T',]);
	hcaseT<-hist(CASE_OPENED_DATE, breaks=30,freq=TRUE,plot=FALSE)
attach(case[case$CallType=='N',]);
	hcaseN<-hist(CASE_OPENED_DATE, breaks=30,freq=TRUE,plot=FALSE)
attach(case[case$CallType=='R',]);
	hcaseR<-hist(CASE_OPENED_DATE, breaks=30,freq=TRUE,plot=FALSE)
attach(case[case$CallType=='I',]);
	hcaseI<-hist(CASE_OPENED_DATE, breaks=30,freq=TRUE,plot=FALSE)
attach(case[case$CallType=='O',]);
	hcaseO<-hist(CASE_OPENED_DATE, breaks=30,freq=TRUE,plot=FALSE)
	barplot(rbind(hcaseI$counts,hcaseO$counts,hcaseT$counts,hcaseN$counts,hcaseR$counts),col = c("red","blue","green","mistyrose", "lightcyan"), legend = c('In','out','Tech','NonT','Retent'),xlab="EventDate",main="Events #")
	














case[1,]
hratio<-hnom
hratio$counts<-cumsum(rev(hnom$counts))/cumsum(rev(hdenom$counts))
plot(hratio,main="Survival Rate vs Installed Age")

(plot(count(is.na(RPT_DATE_DT) and x<INSTALL2NOW) / count(x<INSTALl2Now) ~x )

m<-glm(factor(is.na(RPT_DATE_DT)) ~ TECH_EARLIEST,family=binomial,data=siteEC)
m<-glm(factor(is.na(RPT_DATE_DT)) ~ LAST_IVR_DATE,family=binomial,data=site)
summary(m)

attach(siteEC[is.na(siteEC$RPT_DATE_DT)])
m<-glm(factor(is.na(RPT_DATE_DT)) ~ TECH_EARLIEST,family=binomial,data=siteEC[is.na(siteEC$RPT_DATE_DT),])

m<-glm(factor(is.na(RPT_DATE_DT)) ~ TECH_EARLIEST,family=binomial,data=siteEC[siteEC$RPT_DATE_DT>'2014-10-01',])

siteTN<-read.csv("R\\input_training1.txt")
m<-glm(factor(is.na(RPT_DATE_DT)) ~ as.Date(TECH_EARLIEST),family=binomial,data=rep(siteTN,100))
summary(m)



apriori


