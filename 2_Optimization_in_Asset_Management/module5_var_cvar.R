library(readxl)
daily_rets_sheet <- read_excel(
  '2_Optimization_in_Asset_Management/FERM_3_Assignment_2.xlsx',
  sheet=1
)

# Question 1
daily_rets_df <- daily_rets_sheet[-1]
daily_rets_df

daily_losses <- -daily_rets_df

variances <- apply(daily_rets_df, 2, var, na.rm=TRUE)
stock_max_var <- names(which.max(variances))

vars_90 <- apply(daily_losses, 2, function(x) quantile(x, 0.90, na.rm = TRUE))
stock_max_var90 <- names(which.max(vars_90))

cvars_90 <- apply(daily_losses, 2, function(loss_vec) {
  loss_sorted <- sort(as.numeric(na.omit(loss_vec)))
  N <- length(loss_sorted)
  p <- 0.90
  Kp <- ceiling(p * N)
  num_samples <- N - Kp + 1
  worst_losses <- tail(loss_sorted, num_samples)
  return(sum(worst_losses) / ((1 - p) * N))
})
stock_max_cvar90 <- names(which.max(cvars_90))

# Greatest for Variance, VaR, and CVaR
cat("Greatest Variance:", stock_max_var, "\n")
cat("Greatest VaR_0.9: ", stock_max_var90, "\n")
cat("Greatest CVaR_0.9:", stock_max_cvar90, "\n")

# Question 2
port_returns <- rowMeans(daily_rets_df, na.rm = TRUE)
port_losses <- -port_returns

p <- 0.90
var_90 <- quantile(port_losses, p, na.rm=TRUE)
cat("90% VaR (Quantile):", round(var_90, 4))

# Question 3
sorted_losses <- sort(na.omit(port_losses))
N <- length(sorted_losses)

# 3. Compute index K_p and total tail samples (N - K_p + 1)
Kp <- ceiling(p * N)
num_samples <- N - Kp + 1

# 4. Sum the largest tail samples and divide by (1 - p) * N
worst_losses <- tail(sorted_losses, num_samples)
cvar_90 <- sum(worst_losses) / ((1 - p) * N)

# Print final result rounded to 4 decimal places
cat("90% Portfolio CVaR:", round(cvar_90, 4), "\n")

# Question 4
# P&L vectors
pnl1 <- c(-0.012, 0.021, 0.0212, 0.0111, -0.0054, 0.0254, -0.0195, -0.003, 0.008, -0.021)
pnl2 <- c(-0.012, 0.021, 0.0212, 0.0111, -0.0054, 0.0254, -0.0195, -0.003, 0.008, -0.030)

loss1 <- -pnl1
loss2 <- -pnl2

N <- 10
p <- 0.90
Kp <- ceiling(p * N)           # 9
num_samples <- N - Kp + 1      # 2

cvar1 <- sum(tail(sort(loss1), num_samples)) / ((1 - p) * N)
cvar2 <- sum(tail(sort(loss2), num_samples)) / ((1 - p) * N)

diff_cvar <- cvar2 - cvar1
cat("CVaR_0.9(Investment_2) - CVaR_0.9(Investment_1):", round(diff_cvar, 4))
