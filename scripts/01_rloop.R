#### R Loops ####
# compounding interest #

k = 1
while (k < 1001) {
  cat("--------------------------", "\n")
  cat("k is ", k, "\n")
  cat("Compounded Interest is ", (1+1/k)^k, "\n")
  k = k+1
  }

# estimating pi using a monte carlo simulation

set.seed(12345)
k = 10
while (k < 100000001) {
  point = cbind(runif(k, min = -1, max = 1),runif(k, min = -1, max = 1))
  inside_circle = sum(((point[,1]^2+point[,2]^2)<1))
  cat("--------------------------", "\n")
  cat("Total number of points is ", k, "\n")
  cat("magical number is ", inside_circle/k*4, "\n")
  k = k*10
}