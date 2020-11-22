#### Discretized Objective Function Optimization via Stepwise Selection

The purpose of this algorithm is to efficiently weight the allocation of stocks in a portfolio such that the Sharpe Ratio is maximized. There are several methods in which one can do this

1) Grid Search (Discretized Dynamical System)
The standard Grid Search considers every possible combination of weights for the stocks in each portfolio. It iterates over each stock weight, by first finding the optimal value of each stock and then tweaking all other stock values. The algorithm can then be repeated to ensure convergence to a global maxima. 

__Advantages__
- Avoids being stuck in local maxima as the whole parameter space is searched
- Can consider many combinations of portfolio weights and 'tweaks' weights assigned to previous stocks

__Disadvantages__
- Computationally Inefficient- the algorithm searches all areas, even in spatially distant regions from the global maxima
- Grid Search suffers from granularity problems- the smaller the incremental change in weight the more computationally expensive the algorithm becomes

2) Random Search
Stochastic random search works by randomly simulating weights for each stock such that the weights sum to 1. For each simulation the Sharpe Ratio is calculated and the weights associated with the maximum Sharpe Ratio are assigned

__Advantages__
- Avoids Local Maxima
- Weights randomly assigned and then normalized to sum to one, so the weight for each stock is not restricted by the weights given to prior stocks
- Enables one to roughly search the whole area in a far more computationally efficient manner than DDS

__Disadvantages__
- Many simulations required to find a reasonable estimate for the optimal value of all weights

3) Simulated Aneeling
Similar to Random Search, except the value of the randomly generated weights converge over time. This is done by reducing the variance of the distribution used to simulate the weights and then fixing the mean of the distribution for each weight around the weights that give the highest initial Sharpe ratio

__Advantages__
- Computationally far more efficient than DDS or Random Search, as the weights converge over time
- Can be far more granular and accurate, as the weights converge and the variance decreases over time
- Problems of convergence to local and not global maxima can be solved by replicating the process and tuning the speed of convergence of the weights (the speed at which the variance parameter declines)

__Disadvantages__
- Sharpe Ratio can become stuck in local maxima and the chance of this increases as the number of stocks in the portfolio increases
