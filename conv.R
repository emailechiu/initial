conv<-function(x,m,l) {
if ((l==0)||length(m)==0||!is.vector(x)||!is.vector(m)) return()
if (length(x)<length(m)) x<-c(rep(0,length(m)-length(x)),x)
i<-as.numeric(Reduce(xor, x[1:length(m)]*m))
return(c(i,conv(c(x[-1],i),m,l-1)))
}

conv(c(1),c(1,0,1,0,0),10)

cadd <- function(x) Reduce("+", x, accumulate = TRUE)
cadd(seq_len(7))


Examples

## A general-purpose adder:
add <- function(x) Reduce("+", x)
add(list(1, 2, 3))
## Like sum(), but can also used for adding matrices etc., as it will
## use the appropriate '+' method in each reduction step.
## More generally, many generics meant to work on arbitrarily many
## arguments can be defined via reduction:
FOO <- function(...) Reduce(FOO2, list(...))
FOO2 <- function(x, y) UseMethod("FOO2")
## FOO() methods can then be provided via FOO2() methods.

## A general-purpose cumulative adder:
cadd <- function(x) Reduce("+", x, accumulate = TRUE)
cadd(seq_len(7))

https://stat.ethz.ch/R-manual/R-devel/library/base/html/funprog.html

## A simple function to compute continued fractions:
cfrac <- function(x) Reduce(function(u, v) u + 1 / v, x, right = TRUE)
## Continued fraction approximation for pi:
cfrac(c(3, 7, 15, 1, 292))
## Continued fraction approximation for Euler's number (e):
cfrac(c(2, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8))

## Iterative function application:
Funcall <- function(f, ...) f(...)
## Compute log(exp(acos(cos(0))
Reduce(Funcall, list(log, exp, acos, cos), 0, right = TRUE)
## n-fold iterate of a function, functional style:
Iterate <- function(f, n = 1)
    function(x) Reduce(Funcall, rep.int(list(f), n), x, right = TRUE)
## Continued fraction approximation to the golden ratio:
Iterate(function(x) 1 + 1 / x, 30)(1)
## which is the same as
cfrac(rep.int(1, 31))
## Computing square root approximations for x as fixed points of the
## function t |-> (t + x / t) / 2, as a function of the initial value:
asqrt <- function(x, n) Iterate(function(t) (t + x / t) / 2, n)
asqrt(2, 30)(10) # Starting from a positive value => +sqrt(2)
asqrt(2, 30)(-1) # Starting from a negative value => -sqrt(2)

## A list of all functions in the base environment:
funs <- Filter(is.function, sapply(ls(baseenv()), get, baseenv()))
## Functions in base with more than 10 arguments:
names(Filter(function(f) length(formals(args(f))) > 10, funs))
## Number of functions in base with a '...' argument:
length(Filter(function(f)
              any(names(formals(args(f))) %in% "..."),
              funs))

## Find all objects in the base environment which are *not* functions:
Filter(Negate(is.function),  sapply(ls(baseenv()), get, baseenv()))


All<-function(fun,x,...) {
for(a in c(...)) {print(a)
	print(x)
	x<-fun(x,a)
	}
as.numeric(x)
}
All("&&",1,0,0,1,1)

addPercent <- function(x, mult = 100, FUN = round, ...){
 percent <- FUN(x * mult, ...)
 paste(percent, "%", sep = "")
}

addemup <- function(x,...) {
  args <- list(...)
  print(args)
  for (a in args) {print(a)
	print(x)
	x <- x + a
  }
  x
}

p<-matrix(as.logical(round(runif(24))), ncol = 3)
apply(p,2,"|")

addemup(1,2,3,4,5)
All(sum,c(1,2,3,4,5))
addPercent(c(1,2,3,4,5), FUN = signif, digits = 3)

markov(c(1,1,0,1,1),c(1,0,0,0,0),5)

x