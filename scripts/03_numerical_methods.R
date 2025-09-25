####################
# Numerical Method #
####################

###########################################
# Solving Equation using Bisection method #
###########################################

# Solving 1/x = sin(x) using numerical Bisection method
# first plot the two functions
x = seq(0.1,10,0.01)
f1 = 1/x
f2 = sin(x)
matplot(x,cbind(f1,f2),type="l", lty=c(1,2), lwd=c(3,3))

# define the gap function
d = function(x) 
{return(1/x-sin(x))}

# find the root between 2 and 4 (why those two values?)
i = 0; smallx = 6; bigx = 8
while (i<20) {
  root = (smallx + bigx)/2
  if (d(root)*d(bigx)>0) 
  {bigx = root}
  else
  {smallx = root}
  cat("--------------------------", "\n")
  cat("Root is ", root, "\n")
  i = i+1
}	
