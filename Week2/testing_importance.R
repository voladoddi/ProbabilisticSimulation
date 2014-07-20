#************************function 1 ******************************
f <- function(x) x^ (-0.5)


##plotting histogram and analyzing variance##
#n <-1000
#a <- 0.01
#b <- 1
#set.seed(5)
#r <- replicate(50, mean(f(runif(n,a,b)))*(b-a))
##hist(r)

#plot(f,type="l",col="red")

#variance of the evaluated integral
#print (var(r))


##########################function 2#########################################
#f <- function(x) exp(-x)


##plotting histogram and analyzing variance##
#n <-1000
#a <- 0.01
#b <- 1
#set.seed(5)
#r <- replicate(50, mean(f(runif(n,a,b)))*(b-a))
##hist(r)
#x11()
#plot(f,type="l",col="green")

#variance of the evaluated integral
#print (var(r))
##############################################################################
f1 <- function(x) 1/(1+sinh(2*x)*log(x))
plot(f1,col="red")

f2 <- function(x) 3*(1/(0.2*sqrt(2*pi)))*exp(-((x-0.5)^2)/0.08)
x11()
plot(f2,col="green")

f22 <- f1(runif(1000,0.01,1))

sample.x <- runif(1000,0.01,1)
accept = c()
set.seed(2)
for ( i in 1: length(sample.x))
{
	U = runif(1, 0 ,1)
	
	if (dunif(sample.x[i],0.01,1)* 5 * U <= dnorm(sample.x[i],0.5,0.2))
	{
		accept[i] = dunif(sample.x[i],0,1)* 5 * U 
	}
	#else
	#{
	#	accept[i] = 'NO'
	#}
}


##############################
#set.seed(1909)
#X <- runif(1000,0.01,1)
#Y <- X^(-0.5)
#c( mean(Y), var(Y) )


#w <- function(x) dunif(x, 0.01, 1)/dexp(x,rate=1.5)
#f <- function(x) x^(-0.5)
#X= rexp(1000,rate=1.5)
#Y=w(X)*f(X)
#c( mean(Y), var(Y) )
###############################








#********** Importance Sampling MC method *****************************

#function 1 
 w <- function(x) dunif(x,0.01,1)/dbeta(x,0.7,1)
 f <- function(x) x^(-0.5)
 X <- rbeta(1000,0.7,1)
 Y <- w(X)*f(X)
 c(mean(Y),var(Y))

#--------------
#function 2
 set.seed(19) #0.3 0.4
 w <- function(x) dunif(x,0.8,3)/dnorm(x,0.8,1.49)
 f <- function(x) (1+sinh(2*x)*log(x))^(-1)
plot(f) 
 X <- rnorm(1000,0.8,1.49)
 X <- X[X > 0.8 & X < 3]
 Y <- w(X)*f(X)
 c(mean(Y),var(Y))
hist(Y)

w <- function(x) 3*(1/(0.25*sqrt(2*pi)))*exp(-((x-0.45)^2)/(2*(0.25^2)))
x11(); plot(w,col="green")
sample.x <- runif(1000,0.8,3)
accept = c()
set.seed(2)
for ( i in 1: length(sample.x))
{
	U = runif(1, 0 ,1)
	
	if (w(sample.x[i])* 3 * U <= dnorm(sample.x[i],0.8,1.49))
	{
		accept[i] = dunif(sample.x[i],0.8,3)* 3 * U 
	}
}
yy <- accept[!is.na(accept)]
yy
sum(yy)/1000
var(yy)
#-----------------
#function 3
 f <- function(x) exp(-(x[1]^4)-(x[2]^4))
 
 w1 <- function(x) (dunif(x,-pi,pi)/dnorm(x,-pi,1))*(dunif(x,-pi,pi)/dnorm(x,pi,1))

 X <- rnorm(1000,0,1)

 X <- X[X > -pi & X < pi]
 Y <- (w1(X)*f(X))+(f(X)*w1(X))
 c(mean(Y),var(Y))












