import numpy as np

def price_american_stock_option(option_type = 'call'):
    """Function to Price American Stock Options (Calls & Puts)"""

    # Build stock price binomial tree
    stock_tree = np.zeros((n_periods+1, n_periods+1))

    for i in range(n_periods + 1):
        for j in range(i + 1):
            stock_tree[i,j] = S0 * (u ** j) * (d ** (i - j))

    # Initialize option value tree
    opt_tree = np.zeros((n_periods+1, n_periods+1))
    earliest_exercise_period = n_periods

    # Terminal Payoffs (Period 15)
    for j in range(n_periods + 1):
        if option_type == 'call':
            opt_tree[n_periods, j] = max(0, stock_tree[n_periods, j] - K)
        else:
            opt_tree[n_periods, j] = max(0, K - stock_tree[n_periods, j])

    # Backward Induction
    for i in range(n_periods - 1, -1, -1):
        for j in range (i + 1):
            continuation = df * (p * opt_tree[i+1, j+1] + (1 - p) * opt_tree[i+1, j])

            if option_type == 'call':
                exercise = max(0, stock_tree[i,j] - K)
            else:
                exercise = max(0, K - stock_tree[i,j])

            opt_tree[i,j] = max(continuation, exercise)

            if exercise > continuation + 1e-8 and exercise > 0:
                if i < earliest_exercise_period:
                    earliest_exercise_period = i

    return opt_tree[0,0], earliest_exercise_period

# 4. Function to Price American Futures Call Option (n = 10 periods)
def price_american_futures_call(n_opt=10):
    # Futures Price Tree up to n_opt periods
    # F_{i,j} = S_{i,j} * exp((r - c) * (15 - i) * dt)
    futures_tree = np.zeros((n_opt + 1, n_opt + 1))
    for i in range(n_opt + 1):
        for j in range(i + 1):
            s_node = S0 * (u ** j) * (d ** (i - j))
            futures_tree[i, j] = s_node * np.exp((r - c) * (15 - i) * dt)
            
    opt_tree = np.zeros((n_opt + 1, n_opt + 1))
    earliest_exercise_period = n_opt
    
    # Terminal Payoffs (Period 10)
    for j in range(n_opt + 1):
        opt_tree[n_opt, j] = max(0, futures_tree[n_opt, j] - K)
        
    # Backward Induction
    for i in range(n_opt - 1, -1, -1):
        for j in range(i + 1):
            continuation = df * (p * opt_tree[i+1, j+1] + (1 - p) * opt_tree[i+1, j])
            exercise = max(0, futures_tree[i, j] - K)
            
            opt_tree[i, j] = max(continuation, exercise)
            
            if exercise > continuation + 1e-8 and exercise > 0:
                if i < earliest_exercise_period:
                    earliest_exercise_period = i
                    
    return opt_tree[0, 0], earliest_exercise_period

if __name__ == '__main__':
    S0 = 100.0
    K = 110
    T = 0.25
    n_periods = 15
    r = 0.02
    sigma = 0.3
    c = 0.01

    # 2. Calculate Binomial Model Constants
    dt = T / n_periods
    u = np.exp(sigma * np.sqrt(dt))
    d = 1.0 / u
    p = (np.exp((r - c) * dt) - d) / (u - d)
    df = np.exp(-r * dt)

    print(f'Calculated u: {u:.6f}')
    print(f'Calculated d: {d:.6f}')
    print(f'Calculated p: {p:.6f}\n')


    # Run Stock Options
    call_price, call_ex_period = price_american_stock_option('call')
    put_price, put_ex_period = price_american_stock_option('put')

    print(f"--- Questions 1 & 3 & 4 (Stock Options) ---")
    print(f"American Call Option Price: {call_price:.2f}")
    print(f"American Put Option Price: {put_price:.2f}")
    print(f"Put Earliest Exercise Period: {put_ex_period if put_ex_period < 15 else 15}\n")

    # Run Futures Option
    fut_call_price, fut_ex_period = price_american_futures_call(10)

    print(f"--- Questions 6 & 7 (Futures Options) ---")
    print(f"American Futures Call Option Price: {fut_call_price:.2f}")
    print(f"Futures Call Earliest Exercise Period: {fut_ex_period}")