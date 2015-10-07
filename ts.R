###########Population Survival#############################
require(RODBC)
myconn <-odbcConnect("SQLEVAL")
sitets<-sqlQuery(myconn, "select * from etl.dbo.SITE_b4_2012_11");head(sitets)
ggplot(subset(sitets,subset=(INSTALL2CHURN>758)))+aes(CASE_OPENED_DATE,CallType,group=SITE_NAME,shape=NA)+geom_point()+geom_line()+geom_text(aes(label=CallType),size=2)+facet_wrap(~SITE_NAME,ncol=3)

summary(sitets)
sitets_670<-with(sitets,subset(sitets,subset=(INSTALL2CHURN>670 | is.na(INSTALL2CHURN)),select=c(SITE_NAME,CASE_OPENED_DATE,CallType,INSTALL2CHURN)));
sitets_670$CallType<-as.numeric(sitets_670$CallType)
sitets_670_split<-split(sitets_670,is.na(sitets_670$INSTALL2CHURN))
#head(sitets_670$SITE_NAME);
# This: subset(sitets_670,(sitets_670$SITE_NAME=='AMR-001-473A')); #Not THIS! sitets_670[sitets_670$SITE_NAME=="AMR-001-473A"]
library(zoo)
sitets_670.zoo<-zoo(sitets_670_split[[1]],sitets_670_split[[1]]$CASE_OPENED_DATE)
# This does not work: sitets_670.split<-split(sitets_670.zoo,sitets_670$SITE_NAME);head(sitets_670.split,1)
sitets_670.split2<-split(sitets_670.zoo,sitets_670.zoo$SITE_NAME);head(sitets_670.split2,1);str(head(sitets_670.split2,1)); 
str(sitets_670.split2[[1]][,3]);  plot(sitets_670.split2[[1]][,3],ylim=(1:5),type='b')


###########Copied from plot zoo documentation################
x.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-"))
## univariate plotting
x <- zoo(rnorm(5), x.Date)
x2 <- zoo(rnorm(5, sd = 0.2), x.Date)
plot(x)
lines(x2, col = 2)

## multivariate plotting
z <- cbind(x, x2, zoo(rnorm(5, sd = 0.5), x.Date))
colnames(z) <- LETTERS[1:3]
plot(z, plot.type = "single", col = list(B = 2))
plot(z, type = "b", pch = 1:3, col = 1:3)
plot(z, type = "b", pch = list(A = 1:5, B = 3), col = list(C = 4, 2))
plot(z, type = "b", screen = c(1,2,1), col = 1:3)

## plot one zoo series against the other.
plot(x, x2)
plot(x, x2, xy.labels = TRUE)
plot(x, x2, xy.labels = 1:5, xy.lines = FALSE)

## barplot
x <- zoo(cbind(rpois(5, 2), rpois(5, 3)), x.Date)
barplot(x, beside = TRUE)

###########Copied from plot zoo documentation################



	sitets_ordered<-order(sitets$RPT_DATE_DT,sitets$SITE_NAME,sitets$CASE_OPENED_DATE)
	attach(sitets)
	sitetsSorted<-sitets[order(RPT_DATE_DT,SITE_NAME,CASE_OPENED_DATE,na.last=FALSE),]
	sitetsOrdered<-sitets[sitets_ordered,]
attach(sitetsOrdered[1:100,1:3]);sitetsOrdered[1:100,2:3];list(SITE_NAME)
newdata<-sitetsOrdered[1:100,1:3]
attach(subset(sitetsOrdered[1:10,1:3]))

tsdata <- aggregate(sitetsOrdered[1:100,2:3], by=list(SITE_NAME), FUN=unlist(rbind));tsdata;str(tsdata);tsdata$zoo<-zoo(I(tsdata$CallType),I(tsdata$CASE_OPENED_DATE)))
nrow(tsdata);ncol(tsdata)
tsdata

a=1:3
b=list(1:3,2:4)
df<-data.frame(a,b);df
df$b<-list(1:3,2:6,3:8)
df$c<-df$b
rbind(df$b,df$c)
cbind(df$b,df$c)
df$d<-zoo(do.call(unlist,df$b),do.call(unlist,df$c))
df$b
df$d<-paste(df$c,df$b)
paste(list(c(1:3)),list(c(2:6)))

lists <- list(col1=list(7,8,9), col2=list(70,80,90,100), col3=list(700,800,900));lists[1,1];lists[[2]];lists[[3]]
lists.call<-do.call(rbind,lists);lists.call

df$d<-zoo(rbind(df$b),rbind(df$c))
#df$c[1]=I(matrix(c(1:4),2,2))
dfm <- data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)))
dfl <- data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)))
df$d<-list(col1=df$b,col2=df$c)


split.zoo = Split.zoo ## make generic
x = zoo(1:30,1:30)
f = sample(letters[1:5],30, replace=TRUE)
plot(split(x,f))


within(tsdata, obj<-zoo(I(tsdata[1,3]),I(tsdata[1,2])))
tsdata
tsdata.zoo<-zoo(unlist(tsdata[1,3]),unlist(tsdata[1,2]));t(tsdata.zoo)
tsdata.zoo.Index=tsdata[2,2]
tsdata.zoo.Data=tsdata[2,3]
str(tsdata.zoo)

tsdata.zoo1.Index=tsdata[2,2]
tsdata.zoo1.Data=tsdata[2,3]
str(tsdata.zoo1)

within(tsdata,obj <-zoo(unlist(CallType),unlist(CASE_OPENED_DATE)))
as.vector(unlist(tsdata$CallType))

tsdata.zoo<-zoo(unlist(tsdata[2,3]),unlist(tsdata[2,2]));tsdata.zoo
x <- zoo(rnorm(5), c(1, 3, 7, 9, 14))
unlist(tsdata[2,2])
tsdata[2]

df<-data.frame(a=1:2)
df$b<-list(1:3,4:10);df # NOT df<-data.frame(a=1:2,b=list(1:3,4:10))

	
test_zoo<-zoo(as.numeric(factor(sitetsOrdered[1:21,3])),sitetsOrdered[1:21,2])
test_scan<-scan(what=list(c=numeric(0),d=numeric(0)))
test_scan<-scan(what=(list(c=numeric(0),d=numeric(0))))1



plot(test_zoo)
plot(factor(c('I','O','T','T','T','N','R','T','N'))~sitetsOrdered[1:9,2])
plot(zoo(as.numeric(factor(c('I','O','T','T','T','N','R','T','N'))),as.Date(c("1992-01-10", "1992-01-17", "1992-01-24", "1992-01-31", "1992-02-07", "1992-02-14", "1992-02-21", "1992-02-28", "1992-03-06"))))


directions <- c("North", "East", "South", "South") 
directions.factor <- factor(directions)
directions.factor
as.numeric(directions.factor)
ratio1<-function(d,w) sum(d$u*w)/sum(d$x*w)
ratio2<-function(d,w) sum(d$u*w)/sum(d$x)
ratio3<-function(d,w) w
ratio4<-function(d,w) sum(d$u)/sum(d$x)
ratio5<-function(d,w) 
#result<-boot(city,ratio,weights=c(0.5,0.1,0.1,0.1,0.1,0.1,0,0,0,0),R=999,stype='w')
set.seed(111);
result1<-boot(city,ratio1,R=999,stype='w')
set.seed(111);
result2<-boot(city,ratio2,R=999,stype='w')
set.seed(111);
result3<-boot(city,ratio3,R=999,stype='w')
set.seed(111);
result4<-boot(city,ratio4,R=999,stype='w')
result1$t[1:10];result2$t[1:10];result3$t[1:10];result4$t[1:10]
par(mfrow=c(3,1))
plot(result1,main="result1");plot(result2,main='result2');plot(result3,main='result3')

plot(sitetsOrdered[1:33,1:3])
par(mfrow=c(1,5))
attach(site);
	hwouldbeage<-hist(INSTALL2NOW, breaks=30,freq=TRUE,main="Total Count vs Age",plot=FALSE)
