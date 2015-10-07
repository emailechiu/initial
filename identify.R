fpe <- read.dataframe("http://data.princeton.edu/wws509/datasets/effort.dat")
attach(fpe)
plot(effort,change,pch=21)
identify(effort, change, row.names(fpe), ps=9)
pairs(fpe)

df <- data.frame(x=c(1.8,2.1,3.1,2.8,3.1,4.9,5.1,3.2,2.2),
    y=c(3.2,2.3,4.1,5.2,3.1,2,1.9,2.1,3),
    name=c('agw452','hhewhdsgwgb','cgahawrhs','gsarhrwhd','ehhrwhrwwrw','fhhrwwrw','ghhWwr','hhHRWRHwr','ihwhrHWRHw'))
plot(df$x,df$y)
identified <- identify(df$x,df$y,labels=df$name,pos=T)
df$pos <- NA
df[identified$ind,]$pos <- identified$pos
ggplot(df,aes(x=x,y=y)) + geom_point() + 
    geom_point(data=subset(df,!is.na(pos)),aes(color='red')) +
    geom_text(data=subset(df,pos == 1),aes(label=name),vjust=1) + 
    geom_text(data=subset(df,pos == 2),aes(label=name),hjust=1) +
    geom_text(data=subset(df,pos == 3),aes(label=name),vjust=-.5) + 
    geom_text(data=subset(df,pos == 4),aes(label=name),hjust=0)