# This file contains solutions to Assignments from the Optimization in Asset Management course on Coursera.

#### Module 3: Review #####
# Mean returns vector
mu <- c(0.06, 0.02, 0.04)

# Covariance matrix
V <- matrix(c( 8.0, -2.0,  4.0,
               -2.0,  2.0, -2.0,
               4.0, -2.0,  8.0), 
            nrow = 3, 
            byrow = TRUE) * 1e-3

rf <- 0.01 # 1% Risk Free Rate

# Question 1
x = c(1/3, 1/3, 1/3)

port_mean = t(x) %*% mu

port_mean_pct <- round(port_mean * 100, 2)
print(port_mean_pct)


# Question 2
port_var <- t(x) %*% V %*% x

port_vol <- sqrt(port_var)
port_vol_pct = round(port_vol * 100, 2)

cat("Portfolio Volatility (%) = ", port_vol_pct)


# Question 3
ones <- rep(1, 3)

# to calculate the inverse of the covariance matrix
V_inv <- solve(V)

w_mvp <- (V_inv %*% ones) / as.numeric(t(ones) %*% V_inv %*% ones)

# Let's print the weights just to see them (matches Excel cells B18, C18, D18)
cat("Optimal Minimum Variance Weights:", w_mvp)

mu_mvp <- t(w_mvp) %*% mu

final_ans <- round(mu_mvp * 100, 2)
cat("Mean Portfolio return (%) = ", final_ans)

# Question 4
mu_excess <- mu - rf
# V_inv <- solve(V)
w_unscaled <- V_inv %*% mu_excess

w_sharpe <- w_unscaled / sum(w_unscaled)

mean_return_sharpe <- t(w_sharpe) %*% mu

final_ans <- round(mean_return_sharpe * 100, 2)

print(paste("Sharpe Portfolio Weights:", paste(round(w_sharpe, 4), collapse=", ")))
print(paste("Sharpe Mean Return (%):", final_ans))

# Question 5
var_sharpe <- t(w_sharpe) %*% V %*% w_sharpe
vol_sharpe <- round(sqrt(var_sharpe) * 100, 2)

cat("Volatility of Sharpe Optimal Portfolio (%) = ", vol_sharpe)

# Question 6
cml_slope <- (mean_return_sharpe - rf) / (vol_sharpe / 100)

final_slope_ans <- round(cml_slope, 2)
cat("CML Slope = ", final_slope_ans)

# Question 7
target_vol <- 0.05
target_return <- rf + cml_slope * target_vol
cat("Expected return of inefficient asset (%) = ", round(target_return*100, 2))

    