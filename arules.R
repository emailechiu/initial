b<-c("Photo","Empty")
c<-c("Responded","NotResponded")
d<-merge(b,c,by=NULL)
dmale<-merge(rbind(d[1,],d[1,],d[2,],d[3,],d[4,],d[4,]),c("Male"),by=NULL)
dfemale<-merge(rbind(d[1,],d[2,],d[2,],d[3,],d[3,],d[4,]),c("Female"),by=NULL)
d<-rbind(dmale,dfemale)
d<-rbind(d,d,d,d,d,d,d,d)
colnames(d)<-c("exp","res","sex")
#glm(res~sex,data=d)
library(rpart)
d<-transform(d,expf=as.numeric(factor(exp))+runif(nrow(d))/2,sexf=as.numeric(factor(sex))+runif(nrow(d))/10)
#d<-transform(d,expf=as.numeric(factor(exp)),sexf=as.numeric(factor(sex)))
d.rpart<-rpart(res ~ expf,method="class", d[d$sexf>2,],control=rpart.control(minsplit=2, cp=0.08) )
d.rpart
par(mfrow = c(1,2), xpd = NA)
plot(d.rpart);text(d.rpart,use.n=TRUE,cex=.8)

library(stats)
kmeans(d[,4:5],2,iter.max = 10, nstart = 2)->d.kmeans;d.kmeans;str(d.kmeans);

library(useful);library(help=useful);plot(d.kmeans,data=d)

#subset(cbind(d,d.kmeans$cluster),d.kmeans$cluster==1)

z.auto <- rpart(Mileage ~ Country, car.test.frame)
plot(z.auto)
summary(z.auto)

library(arules)

d.trans<-as(rbind(d,d,d,d,d,d,d,d),"transactions")
image(d.trans)
summary(d.trans)
inspect(d.trans)
unique(d.trans)

library(arulesViz)
d.arules<-apriori(d,parameter = list(minlen=1, conf=0.5),appearance = list(rhs=c("res=Responded", "res=NotResponded"),default="lhs"))
d.arules <- sort(d.arules, by="lift")
inspect(d.arules)

subset.matrix <- is.subset(d.arules,d.arules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- d.arules[!redundant]
inspect(rules.pruned)

plot(rules.pruned, measure=c("support", "lift"), shading="confidence", interactive=TRUE)
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules.pruned, method="grouped")




################Useful examples########################3333
load("/mnt/R/titanic.RData") 
rules <- apriori(titanic.raw,
   parameter = list(minlen=2, supp=0.005, conf=0.8),
   appearance = list(rhs=c("Survived=No", "Survived=Yes"),
   default="lhs"),
   control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
plot(rules)
plot(rules, method="graph", control=list(type="items"))
inspect(rules)
x <- 1:9; names(x) <- x
# Multiplication & Power Tables
x %o% x; x%*%x
y <- 2:8; names(y) <- paste(y,")", sep = "")
outer(y, x, "^")
outer(month.abb, 1999:2003, FUN = "paste")
 
## three way multiplication table:
x %o% x %o% y[1:3]

dunif(x, min = 0, max = 1, log = FALSE)
punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
plot(runif(100, 0,100))
n<-10;p=0.5;boxplot(rbinom(100,n,p)/n/p)

u <- runif(20)
 
## The following relations always hold :
punif(u) == u
dunif(u) == 1
 
var(runif(10000))  #- ~ = 1/12 = .08333

##################Decision Tree Example###############3
# Classification Tree with rpart
library(rpart)

# grow tree 
fit <- rpart(Kyphosis ~ Age + Start + Number,
  	method="class", data=kyphosis)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
  	main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "/mnt/R/binary.ps", 
  	title = "Classification Tree for Kyphosis")

#################Matrix Multiplication#################3
e<-matrix(c(1,2,3,4),ncol=2,byrow=T)
f<-matrix(c(0,3,5,3,0,6,5,4,0),ncol=3);f
e%*%f;f%*%e
apply(e,1,"sum",drop=T);e
which.min(f)
c(f,c(11,12))
g <- list("M","F","F","I","I","M","M","F")
str(sapply(c("M","F","I"),function(gender) which(g==gender)))
h<-matrix(c("M","Fin","Fax","I","I","I","Max","Min","F"),nrow=3)
j<-data.frame(a=c("M","Fin","Fax"),b=c("I","I","I"),c=c("Max","Min","F"),stringsAsFactors=FALSE);j;grep("ax",j)#integer(0)
j<-as.data.frame(matrix(c("M","Max","Fax","Fin","Fi","I","Min","Min","F"),nrow=3),stringsAsFactors=FALSE);j;grep("ax",j)#integer(0)
j<-as.data.frame(matrix(c("M","Fin","Fax","Fax","Fax","Max","Max","Min","F"),nrow=3));j;grep("ax",j)#integer(0)
colnames(j)<-c("fat","fax","fin")
rownames(j)<-c("fat","fax","fin")


#################Table#################################
subtable<-function(tbl,subnames){
dcargs<-list(tbl)
for (i in 1:length(subnames)) dcargs[[i+1]]<-subnames[[i]]
 subarray<-do.call("[", dcargs)
 dims <- lapply(subnames,length)
 subtbl <- array(subarray,dims,dimnames=subnames)
 class(subtbl)<-"table"
 return(subtbl)
}
#####
df<-data.frame(Sex=c("Male","Fem","Inf","Fem"),Party=c("Dem","Indep","Rep","Dem"))
tbl<-table(df);tbl[c("Fem","Male"),c("Dem","Rep")];tbl[Sex=c("Fem","Male"),Party=c("Dem","Rep")]
do.call("[",list(tbl,subnames[[1]], subnames[[2]]))
subnames<-list(Sex=c("Fem","Male"),Party=c("Dem","Rep"))
subtable(tbl,list(Party=c("Dem","Rep"),Sex=c("Fem","Male")))
str(dimnames(subtable(tbl,list(Sex=c("Fem","Male"),Party=c("Dem","Rep")))))

d <- c(5,12,13,4,3,28,12,12,9,5,5,13,5,4,12)
dtab <- table(d)
-sort(-dtab)[1:3]
df[c("Sex")];tbl[c("Fem"),c("Dem")] # Ways to address dataframe and table
as.data.frame(dtab)

aggregate(mtcars[1:6,-2],by=list(mtcars[1:6,]$cyl),sum)
by(mtcars[,-2],mtcars$cyl,function(m) sum(m[,1]))
by(mtcars[1:6,-2:-6],mtcars[1:6,]$cyl,sum)
by(mtcars,mtcars$cyl,function(m) lm(m[,2]~m[,3]))
aggregate(mtcars,by=list(mtcars$cyl),function(m) lm(m[,2]~m[,3]))

tblarray <- unclass(tbl)
dcargs <- list(tblarray)
for (j %in% 1:length(subnames))
	for (i %in%  subnames[[j]])
	ifelse(tbl[x,y])==i,


	

j[grep("ax",j),]
h<-list(c(1:4),list(9:7))

l<-list(1,2,"45ab",3:4)
l[2];l[3]
lapply(list(1:3,25:29),median)
list(1:5)
list(1,2,3,4)
c(1,2,3,4);c(1:4)

if (!library(pixmap)) install.packages("pixmap")
library(pixmap)
library(help=pixmap)
help(package=pixmap)
mtrush1 <- read.pnm("mtrush1.pgm")
mtrush1

blurpart <- function(img,rows,cols,q) {
lrows <- length(rows)
lcols <- length(cols)
newimg <- img
randomnoise <- matrix(nrow=lrows, ncol=lcols,runif(lrows*lcols))
newimg@grey[rows,cols] <- (1-q) * img@grey[rows,cols] + q * randomnoise
return(newimg)
}
plot(blurpart(mtrush1,84:163,135:177,0.5))
