####################Categorical predictor variables#################################
test_regr<-data.frame(pred1=1:30,pred2=c(2,3,5,7,11,13,17,19,23,29),resp=pred1*pred2)
test_regr<-data.frame(pred1=1:10,pred2=c(2,3,5,7,11,13,17,19,23,29),resp=pred1*pred2)
test_regr<-data.frame(pred1=1:50,pred2=rep(c(2,3,5,7,11,13,17,19,23,29),5),resp=pred1+pred2)

test_regr<-transform(test_regr,resm=pred1*pred2,resp=5*pred1+3*pred2+40,pred3=as.character(pred2))
attach(test_regr)
model<-lm(resm~pred1*( (pred3=='2')+(pred3=='3') + (pred3=='5') + (pred3=='7') +(pred3=='11')+(pred3=='13')+(pred3=='17') + (pred3=='19') + (pred3=='23') +(pred3=='29') ))
model<-lm(resm~I(pred1*( (pred3=='2')+(pred3=='3') + (pred3=='5') + (pred3=='7') +(pred3=='11')+(pred3=='13')+(pred3=='17') + (pred3=='19') + (pred3=='23') +(pred3=='29') )))

model<-lm(resp~pred1+( (pred3=='2')+(pred3=='3') + (pred3=='5') + (pred3=='7') +(pred3=='11')+(pred3=='13')+(pred3=='17') + (pred3=='19') + (pred3=='23') +(pred3=='29') ))
summary(model);plot(model)
test_regr<-transform(test_regr,pred3=as.character(pred2))

###################Continuous Predictors and response variables, unbalanced coef ####################
df<-data.frame(p1=rep(1:10,10),p2=rep(1:10,each=10));df$r<-10*df$p1+ df$p2-1;
cor(df$p1,df$p2)
df<-rbind(df,df)
summary(lm(r~p1,df));summary(lm(r~p2,df));plot(lm(r~I(p2+p1),df));df.lm<-lm(r~exp(p1+p2),df)
df$f<-5.5*df$p1+5.5*df$p2-1;df$err<-df$r-df$f;df$sd<-df$err/sd(df$err);plot(df$f,df$err);plot(df$f,df$sd);
plot( (df$f-mean(df$f))/sd(df$f), df$sd)
plot( (df$r-mean(df$r))/sd(df$r), df$sd)

#############Logistic Regression################################
lf<-data.frame(p1=rep(1:10,10), p2=rep(1:10,each=10),p3=1:100)
lf$r=lf$p1< lf$p2
lf$p4=log((lf$p2-1)/(11-lf$p2))
lf<-lf[-1:-10,]
lf.glm<-glm(r~p4,gaussian)
tapply(lf$p1,lf$r,mean)
aggregate(lf$)
plot(lf.glm)
attach(lf[-1:-10,])
cor(lf)
plot(p3,r/10)
help(gl)

attach(lf)
glm.out = glm(r ~ p3, family=binomial(logit), data=lf);summary(glm.out)
plot(p3,r)
lines(p1,glm.out$fitted)

lf<-data.frame(counts=1:10,p1=1:10)
summary(glm(counts~p1,gaussian,lf));plot(glm(counts~p1,gaussian,lf))
summary(lm(counts~p1,lf));plot(lm(counts~p1,lf))

lf<-data.frame(p1=rep(1:10,10), p2=rep(1:10,each=10),p3=1:100)
lf$r=lf$p1< lf$p2
lf$p4=log((lf$p2-1)/(11-lf$p2))

lf.glm = glm(r~p3, data=lf,family=binomial)
plot(lf$p3,lf$r, ,xlab="Temperature",ylab="Probability of Response")
curve(predict(lf.glm,data.frame(p3=x),type="resp"),add=TRUE, col="red")
points(lf$p3,fitted(lf.glm),pch=20)
plot(1:100, exp(0.044*(1:100)-2.5)/(1+exp(0.044*(1:100)-2.5)))
plot(1:100, exp(0.044*(1:100)-2.5))

lf.glm = glm(r~p2, data=lf,family=binomial)
plot(lf$p2*10,lf$r, ,xlab="Temperature",ylab="Probability of Response")
curve(predict(lf.glm,data.frame(p2=x/10),type="resp"),add=TRUE, col="green")
points(lf$p3,fitted(lf.glm),pch=20)
plot((1:10)*10, exp(0.5259*(1:10)-3.2)/(1+exp(0.5259*(1:10)-3.2)))

lf.glm = glm(r~p4, data=lf[-1:-10,],family=binomial)
plot(lf$p4,lf$r, ,xlab="Temperature",ylab="Probability of Response")
curve(predict(lf.glm,data.frame(p4=x),type="resp"),add=TRUE, col="magenta")
points(lf[-1:-10,]$p4,fitted(lf.glm),pch=20)
plot(-2:2, exp((-2:2))/(1+exp(-2:2)))
summary(lf.glm)
lm(seq(-2,2,length.out=100)~(1:100))

seq(-2,2,100)


counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
anova(glm.D93)
summary(glm.D93)

clotting <- data.frame(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12))
summary(glm(lot1 ~ log(u), data = clotting, family = Gamma))
summary(glm(lot2 ~ log(u), data = clotting, family = Gamma))

## Model of joint independence of sex from hair and eye color.
fm <- loglin(HairEyeColor, list(c(1, 2), c(1, 3), c(2, 3)))
fm
1 - pchisq(fm$lrt, fm$df)

library(MASS)
sapply(minn38, function(x) length(levels(x)))
minn38a <- xtabs(f ~ ., minn38)
fm <- loglm(~ 1 + 2 + 3 + 4, minn38a)  # numerals as names.
deviance(fm)

file = "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/gorilla.csv"
read.csv(file) -> gorilla
str(gorilla)

data(menarche)
menarche
m.glm<-glm(cbind(Menarche,Total-Menarche)~Age,binomial(logit), menarche )
attach(menarche); plot(Age,Menarche/Total)
lines(Age,m.glm$fitted)


ucb.df = data.frame(gender=rep(c("Male","Female"),c(6,6)),
                                           dept=rep(LETTERS[1:6],2),
                                           yes=c(512,353,120,138,53,22,89,17,202,131,94,24),
                                           no=c(313,207,205,279,138,351,19,8,391,244,299,317))
ucb.df
mod.form = "cbind(yes,no) ~ gender * dept"
glm.out = glm(mod.form, family=binomial(logit), data=ucb.df)

####################Distribution and plots########################
args(ppois)
plot(seq(-0.01, 5, 0.01),ppois( seq(-0.01, 5, 0.01),1))
example(ppois)
curve(sin(seq(0,2*3.14,0.01)))
help(curve);help(dpois)
chippy <- function(x) sin(cos(x)*exp(-x/2))
negexp<-function(x) dpois(q,)

####################Poisson probability density function##############

curve(exp(-1)/factorial(x),0,10,11,col="green");points(0:10,dpois(0:10,1),col="green")
curve(dpois(x,1),0,10,11,col="green");points(0:10,dpois(0:10,1),col="green")
lines(0:10,dpois(0:10,2),col="red");points(0:10,dpois(0:10,2),col="red")
points(0:10,dpois(0:10,3),col="gold");lines(0:10,dpois(0:10,3),col="gold")
lines(0:10,dpois(0:10,4),col="blue");points(0:10,dpois(0:10,4),col="blue")

curve(exp(-1)/factorial(x),0,10,11,col="green");points(0:10,dpois(0:10,1),col="green")
curve(dpois(x,0.5),0,10,11,col="green");points(0:10,dpois(0:10,0.5),col="green")
lines(0:10,dpois(0:10,1.5),col="red");points(0:10,dpois(0:10,15),col="red")
points(0:10,dpois(0:10,2.5),col="gold");lines(0:10,dpois(0:10,2.5),col="gold")
lines(0:10,dpois(0:10,3.5),col="blue");points(0:10,dpois(0:10,3.5),col="blue")

########################Gamma and Pois on 3D##################################
install.packages("Rcmdr");library(Rcmdr);library(arules);install.packages("arules")
alpha_dis<-0:10
lamda_cont<-seq(0,10,0.1)
df<-merge(lamda_cont,alpha_dis,by=NULL);head(df);colnames(df)<-c("lamda_cont","alpha_dis")
df<-transform(df, gamma_pois=(lamda_cont)^(alpha_dis)/exp(lamda_cont)/factorial(alpha_dis))
#attach(df);sapply(df,length);length(complete.cases(df))
scatter3d(df$lamda_cont,df$alpha_dis, df$gamma_pois )
cor(df$lamda_cont,df$alpha_dis)

attach(mtcars)
scatter3d(wt, disp, mpg)
plot(dpois(1:10,2))

dpois(x,1)
-log(dpois(0:7, lambda = 1) * gamma(1+ 0:7))

# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
#####################Logistic regression###################


