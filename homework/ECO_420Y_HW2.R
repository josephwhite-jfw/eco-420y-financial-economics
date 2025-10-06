####################
# Numerical Method #
####################

###########################################
# Solving Equation using Bisection method #
###########################################

# Equation (1) on Page 5:
#   110x^3 + 10x^2 + 10x - 95 = 0
# Use the hint: x_small = 0, x_big = 1

# define the gap function d(x)
d = function(x) { return(110*x^3 + 10*x^2 + 10*x - 95) }

# optional quick check: signs at the bracket ends
# cat("d(0)=", d(0), "  d(1)=", d(1), "\n")

# bisection with class-style notation/prints
i = 0; smallx = 0; bigx = 1
while (i < 30) {
  root = (smallx + bigx)/2
  if (d(root) * d(bigx) >= 0) {   # NOTE: >= (robust if equality hits)
    bigx = root
  } else {
    smallx = root
  }
  cat("--------------------------", "\n")
  cat("Root is ", root, "\n")
  cat("d(root) is ", d(root), "\n")
  i = i + 1
}

cat("=========================================\n")
cat("Final bisection root ≈ ", root, "  d(root) ≈ ", d(root), "\n")

################
# Optimization #
################

# Apply Newton’s method to maximize f(x) = -x^2 + log(x)  (natural log)
# Class style: define f, fprime, fprime2 (Hessian), and backtracking

f = function(x) { return(-x^2 + log(x)) }
fprime = function(x) { return(-2*x + 1/x) }
fprime2 = function(x) { return(-2 - 1/x^2) }

tol = 0.001
x0 = 1  # any positive start is fine (domain x>0)

while (abs(fprime(x0)) > tol) {
  step = - fprime(x0) / fprime2(x0)  # Newton step for stationary point
  x1 = x0 + step
  
  # backtracking (robust), and keep x1 in domain
  while ((x1 <= 0) || (f(x1) <= f(x0))) {  # NOTE: <= (robust on ties)
    step = step/10
    x1 = x0 + step
  }
  
  cat("---------------------------", "\n")
  cat("New value is ", x1, "\n")
  cat("Gradient is ", fprime(x1), "\n")
  cat("f(x) is ", f(x1), "\n")
  x0 = x1
}

cat("=========================================\n")
cat("Maximizer x* ≈ ", x0, "\n")
cat("f(x*) ≈ ", f(x0), "\n")
