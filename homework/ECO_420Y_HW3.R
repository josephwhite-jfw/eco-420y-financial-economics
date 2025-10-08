# ECO 420Y - Homework 3 

library(quadprog)

Dmat <- matrix(c(2,0,
                 0,8), 2, 2, byrow = T)
dvec <- c(8,16)

Amat <- t(matrix(c(-1,-1,
                   -1, 0,
                   1, 0,
                   0, 1), 4, 2, byrow = T))
bvec <- c(-5,-3,0,0)

sol  <- solve.QP(Dmat, dvec, Amat, bvec)
xopt <- sol$solution
f    <- function(x) -8*x[1] - 16*x[2] + x[1]^2 + 4*x[2]^2

cat("x* =", paste(xopt, collapse = ", "), "\n")
cat("f(x*) =", f(xopt), "\n")
