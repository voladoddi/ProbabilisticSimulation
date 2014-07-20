# Generating n = 100 samples 

pi_estimates_1 <- NULL 
variance_pi_estimates
set.seed(5)
for (k in 1:50){
n <- 100
count <- 0
samples_x <- runif(n)
samples_y <- runif(n)

for (i in 1:n){
if ((samples_x[i]^2 + samples_y[i]^2)<= (1)){
count = count + 1
}
}
count


##########################
area_estimate <- count/n
pi_estimate_1 <- area_estimate * 4
pi_estimates_1[k] <- pi_estimate_1
#########################
}

hist(pi_estimates_1)


############################################################################
# Generating n = 1000 samples 

pi_estimates_2 <- NULL 
variance_pi_estimates
set.seed(5)
for (k in 1:50){
n <- 1000
count <- 0
samples_x <- runif(n)
samples_y <- runif(n)

for (i in 1:n){
if ((samples_x[i]^2 + samples_y[i]^2)<= (1)){
count = count + 1
}
}
count


##########################
area_estimate <- count/n
pi_estimate_2 <- area_estimate * 4
pi_estimates_2[k] <- pi_estimate_2
#########################
}
x11()
hist(pi_estimates_2)

#########################################################################33
############################################################################
# Generating n = 5000 samples 

pi_estimates_3 <- NULL 
variance_pi_estimates
set.seed(5)
for (k in 1:50){
n <- 5000
count <- 0
samples_x <- runif(n)
samples_y <- runif(n)

for (i in 1:n){
if ((samples_x[i]^2 + samples_y[i]^2)<= (1)){
count = count + 1
}
}
count


##########################
area_estimate <- count/n
pi_estimate_3 <- area_estimate * 4
pi_estimates_3[k] <- pi_estimate_3
#########################
}
x11()
hist(pi_estimates_3)

######################################################################
# Generating n = 10000 samples 

pi_estimates_4 <- NULL 
variance_pi_estimates
set.seed(5)
for (k in 1:50){
n <- 10000
count <- 0
samples_x <- runif(n)
samples_y <- runif(n)

for (i in 1:n){
if ((samples_x[i]^2 + samples_y[i]^2)<= (1)){
count = count + 1
}
}
count


##########################
area_estimate <- count/n
pi_estimate_4 <- area_estimate * 4
pi_estimates_4[k] <- pi_estimate_4
#########################
}
x11()
hist(pi_estimates_4)

####################################################
# Generating n = 50000 samples 

pi_estimates_5 <- NULL 
variance_pi_estimates
set.seed(5)
for (k in 1:50){
n <- 50000
count <- 0
samples_x <- runif(n)
samples_y <- runif(n)

for (i in 1:n){
if ((samples_x[i]^2 + samples_y[i]^2)<= (1)){
count = count + 1
}
}
count


##########################
area_estimate <- count/n
pi_estimate_5 <- area_estimate * 4
pi_estimates_5[k] <- pi_estimate_5
#########################
}
x11()
hist(pi_estimates_5)
########################################################################
##########************variance of pi_estimates**************############
variance_pi <- c(var(pi_estimates_1),var(pi_estimates_2),var(pi_estimates_3),var(pi_estimates_4),var(pi_estimates_5))
plot(variance_pi,xlab='Trial number with increasing [n]',col='red',lwd='10')

