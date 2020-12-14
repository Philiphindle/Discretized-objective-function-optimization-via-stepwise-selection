
# Simulated Aneeling Function ---------------------------------------------


Simulated_Aneeling <- function(iterations, n_stocks, Returns_annualized, data, 
                               risk_free, Sharpe_matrix, delta0 = 0.01, 
                               cooling_schedule = 0.01){
  #Simulated Aneeling will require additional parameters, so we will need an option
  #in the main function to enable this
  
  #Also choose a cooling schedule for which the simulations support- i.e. choose
  #a cooling schedule conducive with good function performance
  
  #Starting weights
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