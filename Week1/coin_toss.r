#################   SIMULATING A COIN TOSS FOR 20 TRIALS   ##############################

n=20 					#no of trials
y=NULL 					#initializing a vector of NULL values	
for(i in 1:n)
	{
		x=runif(1)		#random uniform
		if(x<0.5)
		{				#if condition for assigning heads / tails
			y[i]=1
		}
		else
		{
			y[i]=0
		}
	}
y						#print the vector storing the heads and tails.

###########################################################################################
#To count number of heads
sum(y==1)

###########################################################################################

#To count run length of heads
max_length = 1
count_heads = 0

for (i in 2:n)
{
		if(y[i]==1)
		{
		if(y[i]==y[i-1])
		{
		count_heads = count_heads + 1
		}
		else
		{
	        if (count_heads > max_length)
		     {
			max_length = count_heads
		     }
		count_heads=1
		
		}
}

}
max_length


#####  End of program  ###### 