library(readxl)
daily_rets <- read_excel(
  '2_Optimization_in_Asset_Management/FERM_3_Assignment_2.xlsx',
  sheet=1
)

# Question 1
sample_mean <- colMeans(daily_rets[-1], na.rm = TRUE)
sample_mean

alpha <- 0.9

grand_mean <- mean(sample_mean)

mu <- (alpha * sample_mean) + ((1 - alpha) * grand_mean)

ibm_mu <- round(mu['IBM'], 4)
cat("Shrunk return for IBM:", ibm_mu)

# sample covariance matrix V
V <- cov(daily_rets[-1], use = "complete.obs")
n_assets <- ncol(V)

# Avegrage Variance
avg_var <- mean(diag(V))

I <- diag(n_assets)

V_est <- (alpha * V) + ((1 - alpha) * avg_var * I)

# Question 2
library(quadprog)

Dmat <- 2 * V_est
dvec <- rep(0, n_assets)
Amat <- cbind(rep(1, n_assets), mu, diag(n_assets))
bvec <- c(1, 0.0005, rep(0, n_assets))

# Solve
qp_sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)

weights <- qp_sol$solution
names(weights) <- colnames(daily_rets[-1])

ibm_weight <- round(weights['IBM'], 3)
cat("IBM Portfolio Weight:", ibm_weight)

# Question 3
# Building the 2n x 2n block matrix for Dmat
V_block <- rbind(cbind(V_est, -V_est),
                 cbind(-V_est, V_est))
Dmat <- 2 * V_block + 1e-8 * diag(2 * n_assets)
dvec <- rep(0, 2 * n_assets)

Amat <- cbind(
  c(rep(1, n_assets), rep(-1, n_assets)),     # 1^T x^+ - 1^T x^- = 1
  c(mu, -mu),                   # mu^T x^+ - mu^T x^- >= 0.0005
  c(rep(0, n_assets), rep(-1, n_assets)),     # -1^T x^- >= -0.1
  diag(2 * n_assets)                   # z >= 0
)

bvec <- c(1, 0.0005, -0.1, rep(0, 2 * n_assets))

qp_sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)

z <- qp_sol$solution
x_plus <- z[1:n_assets]
x_minus <- z[(n_assets + 1):(2 * n_assets)]
x <- x_plus - x_minus
names(x) <- colnames(daily_rets[-1])

ibm_weight <- round(x['IBM'], 3)
cat("IBM Weight with 10% Shorting Limit:", ibm_weight)

