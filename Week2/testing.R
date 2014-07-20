#############*** monte carlo method for integration***############


#************************ SIMPLE MC INTEGRATION*******************


#******************      function 1     **************************
#*****************************************************************
f <- function(x) x^ (-0.5)

n <- 1000
a <- 0.01
b <- 1


set.seed(5)

x <- runif(n,a,b)
summing <- sum(f(x))*(b-a)/n
print(summing)

r <- replicate(50, mean(f(runif(n,a,b)))*(b-a))
hist(r)
print (var(r))


#*****************************************************************
#****************         function 2      ************************
#*****************************************************************
f <- function(x) 1/(1+sinh(2*x)*log(x))

n <- 1000
a <- 0.8
b <- 3


set.seed(5)
x <-  runif(n,a,b)
summing <- sum(f(x))*(b-a)/n
print(summing)

r <- replicate(50, mean(f(runif(n,a,b)))*(b-a))
x11()
hist(r)
print (var(r))


#*********************************************************************
#***************         function 3          *************************
#*********************************************************************
f <- function(x,y) exp(-(x^4)-(y^4))
	n <- 1000
	a <- -pi
	b <- pi
set.seed(5)
	x <-     runif(n,a,b)
	y <-     runif(n,a,b)

summing <- sum(f(x,y))*(b-a)*(b-a)/n
print(summing)
	
r <- replicate(50, mean(f (runif(n,a,b),runif(n,a,b)) )*(b-a)*(b-a))
	x11()
	hist(r)
	summary(r)

#variance of the evaluated integral
print (var(r))

#************************************************************************
#**********************      function 4      ****************************
f <- function(x,y) ((x^2)+(y^2))
	n <- 1000
	a <- -1
	b <- 1
set.seed(5)
	x <-   runif(n,a,b)
	y <-   runif(n,a,b)
integral_estimate <- (b-a)*(b-a)*sum(f(x,y))/n
print (integral_estimate)

r <- replicate(50, mean(f (runif(n,a,b),runif(n,a,b)) )*(b-a)*(b-a))
x11()
hist(r)
print (var(r))

#***********************************************************************
#***************************    function 5  ****************************
f <- function(x,y) 20 + x^2 + y^2 - 10*(cos(2*pi*x) + cos(2*pi*y))
	n <- 1000
	a <- -5
	b <- 5
set.seed(5)
	x <-     runif(n,a,b)
	y <-     runif(n,a,b)

integral_estimate <- (b-a) * (b-a) * sum(f(x,y)) / n
print(integral_estimate)
x11()

r <- replicate(50, mean(f (runif(n,a,b),runif(n,a,b)) )*(b-a)*(b-a))

hist(r)		     #histogram plot of 'integral estimates'	
print (var(r))         #variance of the evaluated integral


#************************************************************************
#**************************    function 6    ****************************
f <- function(x,y)  (x-1)^2 + 100*(y-(x^2))^2
	n <- 1000
	a <- -2
	b <- 2

set.seed(5)
	x <-     runif(n,a,b)
	y <-     runif(n,a,b)
integral_estimate <- (b-a)*(b-a)*sum(f(x,y))/n
print(integral_estimate)

r <- replicate(50, mean(f (runif(n,a,b),runif(n,a,b)) )*(b-a)*(b-a))
x11()
hist(r)                  #histogram plot of 'integral estimates'
print(var(r))             #variance of estiamtes over 50 runs

***************************************************************************

