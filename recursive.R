findrun<-function(x,k) {
if (length(x)<k) return(NULL)
#else if (identical(rep(1,k),x[1:k])) 
else if (all(rep(1,k)&&x[1:k])) 
return(c(1,1+findrun(x[-1],k)))
else  print(rep(1,k)==x[1:k])
return(1+findrun(x[-1],k))
}
findrun(a,2)

NULL,logical(0)
NA
FALSE,logical(1),integer(1)
TRUE

==
&&
&
||
|
any
all
!

R>is.vector(NA)
[1] TRUE
R>is.vector(NULL)
[1] FALSE
R>is.vector(integer(0))
[1] TRUE
R>is.vector(logical(0))
[1] TRUE

R>is.list(NA)
[1] FALSE
R>is.list(NULL)
[1] FALSE
R>is.list(logical(0))
[1] FALSE
R>is.list(integer(0))
[1] FALSE

R>is.na(NULL)
logical(0)

R>is.null(NA)
[1] FALSE


R>"&&"(c(1,0,0,1,1))
Error: '&&' operator requires 2 arguments
R>"&"(c(1,0,0,1,1))
[1] FALSE  TRUE  TRUE FALSE FALSE


all: terminated by first FALSE,
any: terminated by first TRUE

x<-c(NA,NULL,logical(0),logical(1),TRUE,FALSE)
outer(x,x)
y<-list(NA,NULL,logical(0),logical(1),TRUE,FALSE)


R>all(NA)
[1] NA
R>all(NULL)
[1] TRUE
R>NULL
NULL
R>NULL==TRUE
logical(0)


