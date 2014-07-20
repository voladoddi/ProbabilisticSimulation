### Problem 1 - DTFS: Markov Chain Stationary Distributions #########

#       -----------------------------------------------------------
#            Discrete Time Finite State MC having transition matrix
#       [(1-a)  a ]
#       [b   (1-b)]
# 		
#	A two-state MC's stationary distribution is given by [b/(a+b),a/(a+b)]
#      -------------------------------------------------------------

statDist <- function (a,b){
	transMat <- matrix(c((1-a),a,b,(1-b)),ncol=2,byrow=TRUE)
	cat("Transition Matrix is:   ","[",transMat[1],transMat[2],"]","\n")
      cat("\t\t\t","[",transMat[3],transMat[4]," ]","\n")
	statDist <- c( (b/(a+b)), (a/(a+b)) )
      cat("Stationary Distribution is: ","[",statDist,"]","\n")
}


statDist(1/10,1/15) #(i)
statDist(1/2,1/2) #(ii)
statDist(1,1) #(iii)
statDist(0,0) #(iv)


####------  Problem 2 - DTFS MC Simulation   -------################


# function to draw STEM PLOT of X vs Y (R doesn't have MATLAB like function)
stem <- function(x,y,pch=5,linecol=1,clinecol=1,...){
if (missing(y)){
    y = x
    x = 1:length(x) }
    plot(x,y,pch=pch,...)
    for (i in 1:length(x)){
       lines(c(x[i],x[i]), c(0,y[i]),col=linecol)
    }
    lines(c(x[1]-2,x[length(x)]+2), c(0,0),col=clinecol)
}



###      ###### Simulating Markov Chains #######
# The probability transition matrix
trans  =  matrix(c(0,1,
		       1,0), ncol=2, byrow=TRUE);

# Markov chain simulation function

    # The state that we're starting in
    state = 2;
    cat("Starting state:", state, "\n");

    plotSample <- rep(1,1)
    # Make twenty steps through the markov chain
    for (i in 1:499)
    {
        cat("> Dist:", paste(round(c(trans[state,]), 2)), "\n");
        newState <- sample(1:ncol(trans), 1, prob=trans[state,])
        cat("*", state, "->", newState, "\n");
        state = newState;
	  plotSample <- append(plotSample, state, after=i)
    }
x <- seq(1,500,by=1)
hist(plotSample,col="lightblue",border="red")
x11();
stem(x,plotSample)

#### --- chi-square test of goodness-of fit
chisq.test(plotSample[441:500])

