#----------------------- BOOTSTRAP CI for means----------------------
setwd("C:/Users/Voladoddi/Documents/R Files") #change this to wherever the file "NJGAS.dat" is lcoated in your compuer.

#---------included libraries-------------
library(combinat)  #'combinat' library for sampling with replacement.
library(boot)
#----------------------------------------

set.seed(20)

mydata <- read.table("NJGAS.dat")
mydata

gas <- mydata$V1
x11();hist(gas)
summary(gas)
length(gas)

R = 1000
boot.means = numeric(R)
for (i in 1:R){
boot.sample = sample(gas,R,replace=TRUE)
boot.means[i] = mean(boot.sample)
}
sort(boot.means)
result1 <- quantile(boot.means,c(0.25,0.50,0.975))


#-------printing results and histogram-------------
print(result1)
print(mean(boot.means))
hist(boot.means)
#--------------------------------------------------

####
####
####
#----------------95% Confidence Interval for MC estimate------------

######################################################
f <- function(x) x^ (-0.5)
a <- 0.01
b <- 1
######################################################
estimates <- numeric(15)
R_num <- numeric(15)
xxxx <- numeric(15)
R = 12
k <- 15

#################
set.seed(20)
x <- runif(R,a,b)
mydata <- f(x)
mean(mydata)
################
R = 100
for (j in 1:k){

boot.means = numeric(R)
for (i in 1:R){
boot.sample = sample(mydata,R,replace=TRUE)
boot.means[i] = mean(boot.sample)
}
R = R + 100
R_num[j] <- R
estimates[j] = mean(boot.means)

print(quantile(boot.means,c(0.25,0.975)))
xxxx[j] <- (quantile(boot.means,c(0.975))-quantile(boot.means,c(0.25)))
}
##final plots
plot(seq(100,1500,by=100),estimates,xlab="mean integral estimates",ylab="samples - 100 to 1500")
x11()
plot(xxxx,xlab="Runs - 1:15",ylab="width of confidence interval",col="blue")
