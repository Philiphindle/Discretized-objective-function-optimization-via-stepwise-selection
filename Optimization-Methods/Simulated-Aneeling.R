
# Simulated Aneeling Function ---------------------------------------------


Simulated_Aneeling <- function(iterations, n_stocks, sd = 0.1, Returns_annualized, 
                               data, risk_free, Sharpe_matrix, tol = 0.01, 
                               cooling_schedule = 0.01, starting_weights = NULL){
  #Simulated Aneeling will require additional parameters, so we will need an option
  #in the main function to enable this
  
  #Inputs:
  # Iterations: Number of iterations to simulate
  # n_stocks: number of stocks in dataframe of daily returns
  # sd: Initial Standard deviation of Distribution used to simulate weights
  # Returns_annualized: vector of annualized returns
  # data: Dataframe of daily returns for each stock
  # risk_free: Risk-free interest rate
  # Sharpe_matrix: Matrix used to store Sharpe Ratio for each set of stock weights
  # tol: Tolerance for Algorithm convergence
  # cooling_schedule: Rate of decline of standard deviation of distribution used to 
  #                   simulate stock weights
  # starting_ weights: optional vector of weights used to start the algorithm 
  
  #Also choose a cooling schedule for which the simulations support- i.e. choose
  #a cooling schedule conducive with good function performance
  
  #Starting weights- use grid search with number of iterations proportional to number
  #of stocks in portfolio
  if (is.null(starting_weights)) {
    Random_search_weights <- Random_Grid_Search(iterations = min(1000, 10000 / n_stocks), 
                              n_stocks, Returns_annualized, data, risk_free, 
                              Sharpe_matrix)$Sharpe_matrix
    starting_weights <- Random_search_weights[which.max(Random_search_weights[, 
                                (n_stocks + 1)]), 1:n_stocks]
  }
  weights <- c(runif(n_stocks))
  normalized_weights <- weights / sum(weights)
  
  #Stopping schedule
  while (delta < delta0) {
    #Propose a new set of weights
    weights <- rnorm(n_stocks, mean = normalized_weights, sd = 1 / (delta ^ 2))
    normalized_weights <- weights / sum(weights)
    
    #Calculate Sharpe ratio for given weights
    #Expected Portfolio Return
    ER_p <- normalized_weights %*% Returns_annualized
    #Expected Portfolio Standard Deviation- annualize by multiplying by number of
    #trading days (251)
    Std_P <- sqrt(t(normalized_weights) %*% (var(data) * 251) %*% normalized_weights)
    #Sharpe Ratio
    Sharpe_matrix[i, 1:n_stocks] <- normalized_weights
    Sharpe_matrix[i, (n_stocks + 1)] <- (ER_p - risk_free) / Std_P
  }
  #Return Sharpe Matrix as a list and identify weights corresponding to greatest 
  #Sharpe Ratio
  OptimizationResults <- list("Sharpe_matrix" = Sharpe_matrix)
  return(OptimizationResults = OptimizationResults)
}