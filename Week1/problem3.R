### Problem 3 - Random Sampling using Beta function - Acceptance/Rejection method ###

#-------------------------------------------------------------------------
#### Beta function with parameters = 8 and 5
sample.x <- runif(1000,0,1)
accept = c()
set.seed(2)
for ( i in 1: length(sample.x))
{
	U = runif(1, 0 ,1)
	
	if (dunif(sample.x[i],0,1)* 5 * U <= dbeta(sample.x[i],8,5))
	{
		accept[i] = 'YES'
	}
	else
	{
		accept[i] = 'NO'
	}
}
	
T = data.frame (sample.x , accept = factor (accept, levels = c('YES','NO')))
library(ggplot2)
print(qplot(sample.x, data = T, geom = 'histogram', fill = accept, binwidth=0.01))	
#-------------------------------------------------------------------------
#### Beta function with parameters = 4 and 7
sample.y <- runif(1000,0,1)
accept = c()
set.seed(2)
for ( i in 1: length(sample.x))
{
	U = runif(1, 0 ,1)
	
	if (dunif(sample.x[i],0,1)* 5 * U <= dbeta(sample.x[i],8,5))
	{
		accept[i] = 'YES'
	}
	else
	{
		accept[i] = 'NO'
	}
}
	
T = data.frame (sample.y , accept = factor (accept, levels = c('YES','NO')))
library(ggplot2)
x11()
print(qplot(sample.y, data = T, geom = 'histogram', fill = accept, binwidth=0.01))	

####################### Testing for independence of X and Y ###################
set.seed(2)
B1 <- rbeta(1000, shape1=8, shape2=5)
B2 <- rbeta(1000, shape1=4, shape2=7)
cov(B1,B2)

plot (B1,B2,pch=10)
CT <- table(cut(B1,4), cut(B2,4))
CT
chisq.test(CT)


