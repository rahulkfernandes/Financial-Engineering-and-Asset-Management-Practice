# This file contains solutions the assignment module 5: Practical Issues
# in Implementing Mean Variance.

# Esitmated Mean returns vector
mu <- c(-0.5186,	4.7057, -0.6986)

mu <- mu / 100 # Converting to decimal

# Covariance matrix
V <- matrix(c( 0.0056, -0.0020,	0.0037,
               -0.0020,	0.0022,	-0.0022,
               0.0037,	-0.0022,	0.0074), 
            nrow = 3, 
            byrow = TRUE)


rf <- 0.01 # 1% Risk Free Rate

# Question 1
mu_excess <- mu - rf

V_inv <- solve(V)
w_unscaled <- V_inv %*% mu_excess

w_sharpe <- w_unscaled / sum(w_unscaled)

mean_return_sharpe <- t(w_sharpe) %*% mu
cat("Mean Estimated Return of Sharpe Optimimal = ", mean_return_sharpe)

var_sharpe <- t(w_sharpe) %*% V %*% w_sharpe
sd_sharpe <- sqrt(as.numeric(var_sharpe))

target_vol <- 0.05 # Target volatility
sharpe_ratio <- (mean_return_sharpe - rf) / sd_sharpe
target_return <- rf + (sharpe_ratio * target_vol)

final_ans <- round(target_return * 100, 2)
cat("Estimated Return at 5% Volatility (%):", final_ans)

# Question 2
cat("Expected Returns (Realized) = ", round(mean_return_sharpe*100, 2))

# Question 3
library(readxl)
value_at_risk_sheet <- read_excel(
  '2_Optimization_in_Asset_Management/Assignment2.xlsx',
  sheet=3
)

var_probs <- 0.90
losses <- as.numeric(unlist(value_at_risk_sheet["Equally weighted loss"]))

# Value-at-Risk at 90% Probablity
var_90 <- quantile(losses, probs=var_probs, na.rm = TRUE)

final_var_ans <- round(var_90, 2)
cat("90% Value-at-Risk (%):", final_var_ans)


# Question 4
N <- length(losses)
Kp <- ceiling(var_probs * N) # Finding the index K_p for the VaR threshold

num_samples <- N - Kp + 1

# Extract the largest (N - Kp + 1) samples
worst_losses <- tail(losses, num_samples)

# Sum them up and divide by (1 - p) * N
cvar_90 <- sum(worst_losses) / ((1 - var_probs) * N)


# cvar_90 <- mean(losses[losses >= var_probs], na.rm = TRUE)
cat("90% Conditional Value-at-Risk (%):", round(cvar_90, 2))

# Question 5
prob <- pbinom(11, size = 15, prob = 0.5, lower.tail = FALSE)
round(prob, 4)

p_1 <- pbinom(13, size = 15, prob = 0.5, lower.tail = FALSE)
prob_best <- 1 - (1 - p_1)^100
round(prob_best, 4)

