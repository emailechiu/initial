call_sensitivity<-read.csv('Regression.csv')
attach(call_sensitivity)
call_sensitivity$prob<-Nom/Denom #split(call_sensitivity,Category)
period<-data.frame(Track=c(1:6),breaks=c(1,3,6,22,24,26),duration=c(1,2,3,16,2,2))
call_sensitivity<-merge(call_sensitivity,period,by='Track')
call_sensitivity<-transform(call_sensitivity,Nom_nor=Nom/duration, Denom_nor=Denom/duration, prob_nor=Nom/Denom/duration)
call_sensitivity$Cat <- factor(call_sensitivity$Category,levels=c("Billing","Communication fom HNS","Cost of Service","Customer Service", "Service Performance","Service Reliability","Others"),labels=c("Bil","Com","Cost","Cust","Perf","Rel","Oth")) 
attach(call_sensitivity)
chisq.test(data.frame(call_sensitivity[Month==9,]$Denom,call_sensitivity[Month==10,]$Denom))

result9<-tapply(call_sensitivity[Month==9,]$Denom,call_sensitivity[Month==9,]$Track,sum);result10<-tapply(call_sensitivity[Month==10,]$Denom,call_sensitivity[Month==9,]$Track,sum)
chisq.test(result9[1:5],result10[1:5]/sum(result10));chisq.test(result10[1:5],result9[1:5]/sum(result9));chisq.test(data.frame(result9[1:5],result10[1:5]))

chisq.test(data.frame(c(1000,900),c(2000,1600))) #4.17,0.04
chisq.test(data.frame(c(1000,900),c(1000,800))) #3.06,0.08
chisq.test(data.frame(c(1000,900),c(4000,3200))) #5.07,0.02
chisq.test(data.frame(c(500,450),c(2000,1600))) #2.5,0.12
chisq.test(data.frame(c(500,450),c(1000,800))) #2.02,0.15

chisq.test(data.frame(c(1000,999,998),c(2000,1998,1800)))

with(subset(call_sensitivity,Month==9),boxplot(prob~Track,boxwex=0.25, col="yellow",at=1:6-0.2))
with(subset(call_sensitivity,Month==10),boxplot(prob~Track,col="orange",boxwex=0.25,add=TRUE,at=1:6+0.2))
with(subset(call_sensitivity,Month==9),boxplot(prob~Cat,boxwex=0.25, col="yellow",at=1:7-0.2))
with(subset(call_sensitivity,Month==10),boxplot(prob~Cat,col="orange",boxwex=0.25,add=TRUE,at=1:7+0.2))
lm(prob~factor(Track)*Cat)
model_cat<-lm(prob~(Cat=='Bil')+(Cat=='Com')+(Cat=='Cost')+(Cat=='Cust')+(Cat=='Perf')+(Cat=='Rel')+(Cat=='Oth'));summary(model_cat)
model_track<-lm(prob~(Track==1)+(Track==2)+(Track==3)+(Track==4)+(Track==5)+(Track==6)+0);summary(model_track)
model_track<-lm(prob~(Track==1|Track==3 |Track==4|Track==5)+(Track==2)+(Track==6));summary(model_track)
cors<-sapply(as.numeric(factor(call_sensitivity[,1])),cor,y=prob)
plot(model_track);abline(model_track)
tapply(call_sensitivity[,4],call_sensitivity$Track,sum)/tapply(call_sensitivity[,5],call_sensitivity$Track,sum)
tapply(call_sensitivity[,4],call_sensitivity$Cat,sum)/tapply(call_sensitivity[,5],call_sensitivity$Cat,sum)
call_sensitivity<-transform(


test_regr<-data.frame(pred1=1:10,pred2=c(2,3,5,7,11,13,17,19,23,29))

summary(call_sensitivity[call_sensitivity$Month==9,])
summary(call_sensitivity[call_sensitivity$Month==10,])
attach(call_sensitivity)
dim(call_sensitivity[Month==10,])
dim(call_sensitivity[call_sensitivity$Month==10,])

library(reshape)
newdata<-cast(call_sensitivity,Track+Category~Month);newdata
plot(newdata$'9',newdata$'10',pch=c(1:7),col=c(1:7))

library(ggplot2,logical.return=TRUE)||install.packages("ggplot2")
par(mfrow=c(1,1))
qplot(Track, prob,  data=call_sensitivity[Month==9,], geom=c("point",'smooth'), 
   method="loess", formula=y~x, color=Cat, 
   main="September Regression of Churn Rate on Track ", 
   xlab="Track", ylab="Rate/Month")

qplot(Track, prob,  data=call_sensitivity[Month==10,], geom=c("point", "smooth"), 
   method="loess", formula=y~x, color=Cat, 
   main="October Regression of Churn Rate on Track ", 
   xlab="Track", ylab="Rate/Month")

qplot(Track, prob,  data=call_sensitivity, geom=c("point", "smooth"), facets=color~.,
   method="loess", formula=y~x, color=Cat, 
   main="October Regression of Churn Rate on Track ", 
   xlab="Track", ylab="Rate/Month") )

qplot(Track,prob,data=call_sensitivity, facets=.~Month,method="loess", formula=y~x, color=Cat ,scale_colour_brewer(pal = "Set2" )) + geom_smooth()
summary(mtcars)
ggplot(mtcars)+aes(wt,mpg,colour=as.character(cyl))+geom_point()+scale_colour_brewer(pal = "Set2" )

ggplotR(call_sensitivity)+ aes(Track,prob) +geom_point(colour=as.numeric(factor(Cat))) +scale_colour_brewer(pal = "Set1" )


qplot(Cat,prob,data=call_sensitivity, facets=.~Month,method="loess", formula=y~x,col=Track ) + geom_smooth()
g<-ggplot(call_sensitivity,aes(Cat,prob,label=Track)) +geom_point(color=Track)+geom_line(color=Track);g
g9<-ggplot(call_sensitivity[Month==9,],aes(Track,prob,label=Cat)) +geom_point(aes(color=Cat))+geom_line(aes(color=Cat))+scale_colour_manual(name='', values=c('Bil'='red', 'Com'='blue','Cust'='yellow', 'Cost'='green', 'Perf'='purple','Rel'='orange', 'Oth'='pink'))
g10<-ggplot(call_sensitivity[Month==10,],aes(Track,prob,label=Cat)) +geom_point(aes(color=Cat))+geom_line(aes(color=Cat))+scale_colour_manual(name='', values=c('Bil'='red', 'Com'='blue','Cust'='yellow', 'Cost'='green', 'Perf'='purple','Rel'='orange', 'Oth'='pink'))
ggplot(call_sensitivity,aes(Track,prob,label=Cat))+facet_grid(Track~Month)+geom_point(aes(color=Cat))


table(Cat,Track)
summary(table(Track,Cat))









g<-g+ggtitle("Against Categories");g

x <- rnorm(100)
eps <- rnorm(100,0,.2)
qplot(x,3*x+eps)
qplot(x,2*x+eps)
qplot(displ, hwy, data=mpg, facets = . ~ year) + geom_smooth()

ggplot(
ggplot(data) + geom_line(aes(x=Time, y=weight, colour=Chick)) + facet_wrap(~Diet) + guides(col=guide_legend(ncol=3))

ggplot(data=data, aes(x=Time, y=weight, group=Chick, colour=Chick))  geom_line() geom_point()

library(car) 
scatterplot(mpg ~ wt | cyl, data=mtcars, 
  	xlab="Weight of Car", ylab="Miles Per Gallon", 
   main="Enhanced Scatter Plot", 
   labels=row.names(mtcars))

# Add fit lines
abline(lm(mpg~wt), col="red") # regression line (y~x) 
lines(lowess(wt,mpg), col="blue") # lowess line (x,y)
call_sensitivity

newdata
reduce
par()
curve(x^7)

melt()
tapply(call_sensitivity, Category, sd)
call_sensitivity[call_sensitivity$Category=='Others',]
Sys.Date()-as.Date('2012-09-20')
comb <- stack(list(v1=v1, v2=v2, v3=v3)) # Combine 3 vectors
aov(values ~ ind, data=comb)

coplot(Nom/Denom ~ Month | Track | Category)
by()
split()
do.call
I()
aggregate
unique
ggplot

stack(),unstack
WRT
JUDD

