
# Stochastic Grid Search function -----------------------------------------

Random_Grid_Search <- function(iterations, n_stocks, Returns_annualized, data, 
                               risk_free, Sharpe_matrix){
  for(i in 1:iterations){
    weights <- c(runif(n_stocks))
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


# 1) Boundary Testing -----------------------------------------------------




# 2) Unit Testing ------------------------------------------------------------


