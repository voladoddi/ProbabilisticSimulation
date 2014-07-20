#####******** stratified sampling************

#*************************************************************
########################## Function 1 #########################
f <- function(x) x^(-0.5)

set.seed(5)
	n <-900
	a <- 0.01
	b <- 0.5
r1 <- replicate(50, mean(f(runif(n,a,b)))*(b-a))
	n <-100
	a <- 0.5
	b <- 1
r2 <- replicate(50, mean(f(runif(n,a,b)))*(b-a))

#mean estimate on one run
mean_estimate <- (r1+r2)
print (mean_estimate[1])

r <- (r1+r2)
hist(r)

#variance of the evaluated integral on fifty runs
print (var(r1+r2))




#####################################################################
####################### function 2 ##################################

f <- function(x) 1/(1+sinh(2*x)*log(x))
# 100 700 200
set.seed(5)

	n <-100
	a <- 0.8
	b <- 1
r1 <- replicate(50, mean(f(runif(n,a,b)))*(b-a))
	n <-700
	a <- 1
	b <- 2
r2 <- replicate(50, mean(f(runif(n,a,b)))*(b-a))
	n <-200
	a <- 2
	b <- 3
r3 <- replicate(50, mean(f(runif(n,a,b)))*(b-a))

#mean estimate
print (mean(r1)+mean(r2)+mean(r3))

#mean estimate _ 2
print (r1[1]+r2[1]+r3[1])

#variance of the evaluated integral
print (var(r1+r2+r3))

rr <- r1 + r2 + r3
hist(rr)

#####################################################################
####################### function 3 ##################################
f <- function(x,y) exp(-(x^4)-(y^4))
####
n <- 500
a1 <- -pi
b1 <- 0

a2 <- -pi 
b2 <- pi
set.seed(5)
r1 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))

####
n <-500
a1 <- 0
b1 <- pi

a2 <- -pi
b2 <- pi
set.seed(5)
r2 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))

#mean estimate
print (mean(r1)+mean(r2))

#mean estimate _ 3
print (r1[1]+r2[1])

#variance of the evaluated integral
print (var(r1+r2))

##########************************************************###########
##########*********function 4****************************############
set.seed(5)
f <- function(x,y) ((x^2)+(y^2))

	n <- 250
	a1 <- -1
	b1 <- -0.97

	a2 <- -1
	b2 <- 1

r1 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))
	
	n <- 5000
	a1 <- -0.97
	b1 <- 0.97

	a2 <- -1
	b2 <- 1

r2 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))

	n <- 250
	a1 <- 0.97
	b1 <- 1

	a2 <- -1
	b2 <- 1
r3 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))

#mean estimate
print (mean(r1)+mean(r2)+mean(r3))

#mean estimate _ 4
print (r1[1]+r2[1]+r3[1])

#variance of the evaluated integral
print (var(r1+r2+r3))

########************************************************########
#######********************function 5***************************
f <- function(x,y) 20 + x^2 + y^2 - 10*(cos(2*pi*x) + cos(2*pi*y))

set.seed(5)

	n <-200
	a1 <- -5
	b1 <- -3

	a2 <- -5
	b2 <- 5
r1 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b2-a2)*(b1-a1))
	
	n <-300
	a1 <- -3
	b1 <- 0

	a2 <- -5
	b2 <- 5
r2 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b2-a2)*(b1-a1))


	n <-300
	a1 <- 0
	b1 <- 3

	a2 <- -5
	b2 <-  5
r3 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b2-a2)*(b1-a1))

	n <-200
	a1 <- 3
	b1 <- 5

	a2 <- -5
	b2 <-  5
r4 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b2-a2)*(b1-a1))


#mean estimate
print (mean(r1)+mean(r2)+mean(r3)+mean(r4))

#variance of the evaluated integral
print (var(r1+r2+r3+r4))


#######****************************************************######
###################****function 6 *********************##########
f <- function(x,y) (x-1)^2 + 100*(y-(x^2))^2

set.seed(5)
	n <- 100
	a1 <- -2
	b1 <- -1.5
	
	a2 <- -2
	b2 <- 2
r1 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))

	n <- 350
	a1 <- -1.5
	b1 <- -0.5

	a2 <- -2
	b2 <- 2
r2 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))
	
	n <- 100
	a1 <- -0.5
	b1 <- 0.5

	a2 <- -2
	b2 <- 2
r3 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))


	n <- 350
	a1 <- 0.5
	b1 <- 1.5
	
	a2 <- -2
	b2 <- 2
r4 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))

	n <- 100
	a1 <- 1.5
	b1 <- 2
	
	a2 <- -2
	b2 <- 2
r5 <- replicate(50, (mean(f(runif(n,a1,b1),runif(n,a2,b2))))*(b1-a1)*(b2-a2))

#mean estimate
print (mean(r1)+mean(r2)+mean(r3)+mean(r4)+mean(r5))

#variance of the evaluated integral
print (var(r1+r2+r3+r4+r5))

rr <- r1 + r2 + r3 + r4 + r5

