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

##################
# optional stuff #
##################

#######################################################
# Newton's method for finding root (solving equation) #
#######################################################

# the goal is to solve x^2 = 2, or equivalently, x^2 - 2 = 0
# first plot the function x^2 - 2
x = seq(0,3,0.01)
f = x^2-2
plot(x,f,type="l", main="x squared -2 = 0")
abline(h=0)

# define the function and its 1st order derivative
f = function(x) 
{return(x^2-2)}

fprime = function(x) 
{return(2*x)}

sr = 0.00001
xn = 1
while (abs(f(xn))>sr) {
  xn = xn - f(xn)/fprime(xn)
  cat("Root is ", xn, "\n")
}
cat("squared root of 2 is ", sqrt(2), "\n")

################
# Optimization #
################

f = function(x) {
  return(-x^2)
}

fprime = function(x) {
  return(-2*x)
}

x0 = 5
tol = 0.001

while (abs(fprime(x0))>tol) {
  step = 1
  x1 = x0 + step*2*((fprime(x0)>0)-0.5)
  while (f(x1)<=f(x0)) {
    step = step/10
    x1 = x0 + step*2*((fprime(x0)>0)-0.5)
  }
  cat("---------------------------", "\n")
  cat("New initial value is ", x1, "\n")
  cat("Gradient is ", fprime(x1), "\n")
  cat("f(x) is ", f(x1), "\n")
  x0 = x1
}

# Newton's Method that requires Hessian
fprime2 = function(x) {
  return(-2)
}

tol = 0.001
x0 = 5
while (abs(fprime(x0))>tol) {
  step = -fprime(x0)/fprime2(x0)
  x1 = x0 + step
  while (f(x1)<=f(x0)) {
    step = step/10
    x1 = x0 + step
  }
  cat("New value is ", x1, "\n")
  cat("Gradient is ", fprime(x1), "\n")
  x0 = x1
}

# Numerical gradient
fprimen = function(x) {
  return((f(x+0.001)-f(x))/0.001)
}

x0 = 5
tol = 0.001

while (abs(fprimen(x0))>tol) {
  step = 1
  x1 = x0 + step*2*((fprimen(x0)>0)-0.5)
  while (f(x1)<f(x0)) {
    step = step/10
    x1 = x0 + step*2*((fprimen(x0)>0)-0.5)
  }
  cat("---------------------------", "\n")
  cat("New initial value is ", x1, "\n")
  cat("Gradient is ", fprimen(x1), "\n")
  cat("f(x) is ", f(x1), "\n")
  x0 = x1
}