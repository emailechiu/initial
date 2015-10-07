xvar <- 1:20 *(rnorm(20,sd=1))
zvar <- c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1)+(rnorm(20,sd=0.5))
evar <- c(1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
fvar <- c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1)
yvar <- -(1:20/4)
df <- data.frame (x=xvar,y=yvar,z=zvar,f=fvar)
fit <- lm(z~f, data=df)
fit
summary(fit)
cor(df)
cor(zvar,evar)
cor(zvar, yvar)
cor(xvar,yvar)
plot(xvar,yvar)

summary(rnorm(20,sd=1))
mean(zvar)
sum(evar)
sd(zvar)
fit

avar <-c('Y','N','Y','N','Y','N','Y','N','Y','N')
bvar <-c('C','D','C','D','C','D','C','D','C','D')
cor(avar,bvar)

reg1 <- lm(prestige ~ education + income + type, data = Prestige)
avPlots(reg1,id.n=2, id.cex=0.7)
outlierTest(reg1)
qqPlot(reg1, id.n=5)
influencePlot(reg1,id.n=2)
help(qqPlot)

require(graphics)

t.test(1:10, y = c(7:20))      # P = .00001855
t.test(1:10, y = c(7:20, 200)) # P = .1245    -- NOT significant anymore

## Classical example: Student's sleep data
plot(extra ~ group, data = sleep)
## Traditional interface
with(sleep, t.test(extra[group == 1], extra[group == 2]))
## Formula interface
t.test(extra ~ group, data = sleep)
c(7:20,200)
help(t.test)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)

trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)

group <- gl(2,10, labels=c("Ctl","Trt"))
weight<-c(ctl,trt)
cor(weight,group)
lm(weight~group-0)
helgroup
load("c:/users/echiu/Downloads/titanic.raw.rdata")
1+2








