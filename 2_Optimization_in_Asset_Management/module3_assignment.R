## Module 3: Assignment ##
library(readxl)
daily_rets <- read_excel(
  '2_Optimization_in_Asset_Management/FERM_3_Assignment_1.xlsx',
  sheet=1
)

head(daily_rets)
summary(daily_rets)

# Question 1
mean_rets <- colMeans(daily_rets[-1], na.rm = TRUE)
mean_rets

cov_matrix <- cov(daily_rets[-1], use = 'complete.obs')
cov_matrix

cat('Mean Daily return of GE = ', round(mean_rets['GE'], 4))

# Question 2
cat('Covariance between IBM & GE = ', sprintf("%.6f", cov_matrix['IBM', 'GE']))


# Question 3
library(quadprog)

n_assets <- length(mean_rets)
Dmat <- 2 * cov_matrix
dvec <- rep(0, n_assets)

Amat <- cbind(rep(1, n_assets), mean_rets)
bvec <- c(1, 0.0005)

# Minimizing x^T*V*x, s.t. mu^T*x >= 0.0005, 1^T*x = 1
qp_results <- solve.QP(Dmat=Dmat, dvec=dvec, Amat=Amat, bvec=bvec, meq=1)
qp_results

optimal_variance <- qp_results$value
cat('Optimal value (variance) = ', optimal_variance)


# Question 4
optimal_weights <- qp_results$solution
print(optimal_weights)

expected_return_x1 <- sum(optimal_weights * mean_rets)
cat('Expected Return of portfolio x1 = ', round(expected_return_x1, 4), '\n')


# Question 5
bvec_x2 <- c(1, 0.0008)
qp_results_x2 <- solve.QP(Dmat=Dmat, dvec=dvec, Amat=Amat, bvec=bvec_x2, meq=1)

# Extract the new optimal value (variance)
optimal_variance_x2 <- qp_results_x2$value

cat('New optimal value (variance) for x2 = ', round(optimal_variance_x2, 6))

optimal_weights_x2 <- qp_results_x2$solution

# Question 6
bvec_x3 <- c(1, 0.0010)
qp_results_x3 <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec_x3, meq = 1)
optimal_weights_x3 <- qp_results_x3$solution

phi <- (0.0008 - 0.0010) / (0.0008 - 0.0005)

z <- phi * optimal_weights + (1 - phi) * optimal_weights_x2

is_equal <- all.equal(z, optimal_weights_x3)
cat('Does z equal x3? ', is_equal)

# Question 7
phi_seq <- seq(-2, 2, by=0.0001)

returns_z <- numeric(length(phi_seq))
variances_z <- numeric(length(phi_seq))

# Looping over the phi sequence
for (i in seq_along(phi_seq)) {
  phi_val <- phi_seq[i]
  
  z_weights <- phi_val * optimal_weights + (1 - phi_val) * optimal_weights_x2
  
  returns_z[i] <- sum(z_weights * mean_rets)
  
  variances_z[i] <- as.numeric(t(z_weights) %*% cov_matrix %*% z_weights)
}

plot(
  variances_z, returns_z, type='l', lwd=2, col='blue', main="Efficient Frontier",
  xlab = 'Variance', ylab = 'Return'
)

# Verticial Dashed line at risk tolerance 0.000040
abline(v = 0.000040, col = 'red', lty = 2, lwd = 2)

valid_returns <- returns_z[variances_z <= 0.000040]

# The optimal return is the highest return in that valid set
optimal_constrained_return <- max(valid_returns)

cat('Optimal Return for Variance <= 0.000040 is: ', round(optimal_constrained_return, 4))


# Question 9
rf_daily <- 0.0001

mu_excess <- mean_rets - rf_daily

x_unscaled <- solve(cov_matrix) %*% mu_excess

x_sharpe <- x_unscaled / sum(x_unscaled)

sharpe_port_return <- sum(x_sharpe * mean_rets)
sharpe_port_variance <- as.numeric(t(x_sharpe) %*% cov_matrix %*% x_sharpe)
sharpe_port_volatility <- sqrt(sharpe_port_variance)

sharpe_ratio <- (sharpe_port_return - rf_daily) / sharpe_port_volatility
cat('Sharpe Ratio = ', round(sharpe_ratio, 3))

x_sharpe
