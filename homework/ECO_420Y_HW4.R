#########################################
# Markowitz Portfolio Theory — Comparison
# Old vs New Correlation Matrix
# (script written to match class style)
#########################################

# --- Packages ----
library(tseries)

# --- Inputs (same means/SDs as notes) ----
miu1 = 5;  s1 = 4
miu2 = 8;  s2 = 5
miu3 = 7;  s3 = 4.5
miuv = c(miu1, miu2, miu3)

# =========================================================
# Part A: OLD efficiency frontier (as in notes)
# rho12 = -0.5; rho13 = -0.3; rho23 = 0.4
# =========================================================
rho12 = -0.5; rho13 = -0.3; rho23 = 0.4
s12 = rho12 * s1 * s2
s13 = rho13 * s1 * s3
s23 = rho23 * s2 * s3
sigma_old = matrix(c(s1^2, s12,  s13,
                     s12,  s2^2, s23,
                     s13,  s23,  s3^2),
                   nrow = 3, ncol = 3, byrow = TRUE)

# grid of target means
targetmiu = seq(5, 8, 0.10)

# loop to get min-variance sigma for each target mean
sigp_old = rep(0, length(targetmiu))
w_old = vector("list", length(targetmiu))
for (i in 1:length(targetmiu)) {
  op = portfolio.optim(t(miuv), pm = targetmiu[i], covmat = sigma_old)
  sigp_old[i] = sqrt(t(op$pw) %*% sigma_old %*% op$pw)
  w_old[[i]] = op$pw
}

# GMVP on the old frontier by scanning the grid
idx_old = which.min(sigp_old)
gmvp_mu_old  = targetmiu[idx_old]
gmvp_sig_old = sigp_old[idx_old]
gmvp_w_old   = w_old[[idx_old]]

cat("=== OLD correlations (rho12=-0.5, rho13=-0.3, rho23=0.4) ===\n")
cat(sprintf("GMVP: mu = %.3f, sigma = %.3f\n", gmvp_mu_old, gmvp_sig_old))
cat(sprintf("weights: w1 = %.3f, w2 = %.3f, w3 = %.3f\n\n",
            gmvp_w_old[1], gmvp_w_old[2], gmvp_w_old[3]))

# =========================================================
# Part B: NEW efficiency frontier
# rho12 = 0.5; rho13 = 0.3; rho23 = 0.4
# =========================================================
rho12 = 0.5; rho13 = 0.3; rho23 = 0.4
s12 = rho12 * s1 * s2
s13 = rho13 * s1 * s3
s23 = rho23 * s2 * s3
sigma_new = matrix(c(s1^2, s12,  s13,
                     s12,  s2^2, s23,
                     s13,  s23,  s3^2),
                   nrow = 3, ncol = 3, byrow = TRUE)

# loop for new frontier
sigp_new = rep(0, length(targetmiu))
w_new = vector("list", length(targetmiu))
for (i in 1:length(targetmiu)) {
  op = portfolio.optim(t(miuv), pm = targetmiu[i], covmat = sigma_new)
  sigp_new[i] = sqrt(t(op$pw) %*% sigma_new %*% op$pw)
  w_new[[i]] = op$pw
}

# GMVP on the new frontier
idx_new = which.min(sigp_new)
gmvp_mu_new  = targetmiu[idx_new]
gmvp_sig_new = sigp_new[idx_new]
gmvp_w_new   = w_new[[idx_new]]

cat("=== NEW correlations (rho12=0.5, rho13=0.3, rho23=0.4) ===\n")
cat(sprintf("GMVP: mu = %.3f, sigma = %.3f\n", gmvp_mu_new, gmvp_sig_new))
cat(sprintf("weights: w1 = %.3f, w2 = %.3f, w3 = %.3f\n\n",
            gmvp_w_new[1], gmvp_w_new[2], gmvp_w_new[3]))

# =========================================================
# Plot: overlay old vs new efficient frontiers
# =========================================================
plot(sigp_old, targetmiu, type = "l", lwd = 2,
     xlab = "sigmap", ylab = "mup",
     main = "Efficient Frontiers (Old vs New Correlations)",
     col = "blue")
lines(sigp_new, targetmiu, lwd = 2, col = "red")

# mark GMVP points
points(gmvp_sig_old, gmvp_mu_old, pch = 19, col = "blue")
points(gmvp_sig_new, gmvp_mu_new, pch = 19, col = "red")

legend("bottomright",
       legend = c("Old frontier", "New frontier", "Old GMVP", "New GMVP"),
       lty = c(1,1,NA,NA), pch = c(NA,NA,19,19),
       col = c("blue","red","blue","red"),
       bty = "n", lwd = c(2,2,NA,NA))
