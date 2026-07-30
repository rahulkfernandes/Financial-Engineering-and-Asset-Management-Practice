import numpy as np

class BinomialPricer:
    def __init__(self, S0, K, T, n_periods, r, sigma, c):
        """Currently works with American Options only"""
        self.S0 = S0
        self.K = K
        self.T = T
        self.n_periods = n_periods
        self.r = r
        self.sigma = sigma
        self.c = c
        
        # Automatically precompute constants upon initialization
        self.dt = T / n_periods
        self.u = np.exp(sigma * np.sqrt(self.dt))
        self.d = 1.0 / self.u
        self.p = (np.exp((r - c) * self.dt) - self.d) / (self.u - self.d)
        self.df = np.exp(-r * self.dt)

    def price_stock_option(self, option_type='call'):
        stock_tree = np.zeros((self.n_periods + 1, self.n_periods + 1))
        for i in range(self.n_periods + 1):
            for j in range(i + 1):
                stock_tree[i, j] = self.S0 * (self.u ** j) * (self.d ** (i - j))

        opt_tree = np.zeros((self.n_periods + 1, self.n_periods + 1))
        earliest_exercise_period = self.n_periods

        for j in range(self.n_periods + 1):
            if option_type == 'call':
                opt_tree[self.n_periods, j] = max(0, stock_tree[self.n_periods, j] - self.K)
            else:
                opt_tree[self.n_periods, j] = max(0, self.K - stock_tree[self.n_periods, j])

        for i in range(self.n_periods - 1, -1, -1):
            for j in range(i + 1):
                continuation = self.df * (self.p * opt_tree[i+1, j+1] + (1 - self.p) * opt_tree[i+1, j])
                exercise = max(0, stock_tree[i, j] - self.K) if option_type == 'call' else max(0, self.K - stock_tree[i, j])
                
                opt_tree[i, j] = max(continuation, exercise)
                if exercise > continuation + 1e-8 and exercise > 0:
                    if i < earliest_exercise_period:
                        earliest_exercise_period = i

        return opt_tree[0, 0], earliest_exercise_period

    def price_futures_option(self, n_opt=10):
        futures_tree = np.zeros((n_opt + 1, n_opt + 1))
        for i in range(n_opt + 1):
            for j in range(i + 1):
                s_node = self.S0 * (self.u ** j) * (self.d ** (i - j))
                futures_tree[i, j] = s_node * np.exp((self.r - self.c) * (self.n_periods - i) * self.dt)
                
        opt_tree = np.zeros((n_opt + 1, n_opt + 1))
        earliest_exercise_period = n_opt
        
        for j in range(n_opt + 1):
            opt_tree[n_opt, j] = max(0, futures_tree[n_opt, j] - self.K)
            
        for i in range(n_opt - 1, -1, -1):
            for j in range(i + 1):
                continuation = self.df * (self.p * opt_tree[i+1, j+1] + (1 - self.p) * opt_tree[i+1, j])
                exercise = max(0, futures_tree[i, j] - self.K)
                
                opt_tree[i, j] = max(continuation, exercise)
                if exercise > continuation + 1e-8 and exercise > 0:
                    if i < earliest_exercise_period:
                        earliest_exercise_period = i
                        
        return opt_tree[0, 0], earliest_exercise_period

# --- How clean it looks to use: ---
if __name__ == '__main__':
    # Initialize the model with your parameters once
    model = BinomialPricer(S0=100.0, K=110.0, T=0.25, n_periods=15, r=0.02, sigma=0.30, c=0.01)

    call_price, _ = model.price_stock_option('call')
    put_price, put_ex = model.price_stock_option('put')
    fut_price, fut_ex = model.price_futures_option(10)

    print(f"Call Price: {call_price:.2f}")
    print(f"Put Price: {put_price:.2f} (Earliest Exercise: {put_ex})")
    print(f"Futures Call Price: {fut_price:.2f} (Earliest Exercise: {fut_ex})")