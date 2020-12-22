
# Simulated Aneeling Function ---------------------------------------------


Simulated_Aneeling <- function(iterations, n_stocks, Returns_annualized, 
                               data, risk_free, Sharpe_matrix, tol = 0.01, 
                               cooling_schedule = 1.001, starting_weights = NULL, 
                               delta = 1.01){
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
  
  #Check delta > 1 (Otherwise no convergence- must have delta> 1)
  
  #Check delta appropriate (i.e. delta must be realistically between 1.0 and 1.01, 
  #otherwise algorithm will converge too quickly, potentially getting stuck in local
  #maxima)
  
  #Also exactly the same logic for the cooling schedule
  
  #Make sure starting weights aren't specified as non-null when they only exist in 
  #the current working environment- need to be of the right dimensions (i.e. if 
  #weights are not explicitly expressed as an arguement, we need to set any 
  #starting weights in the global environment to be zero)
  
  #Check starting weights sum to one- have some tolerance if they are not exactly
  if (sum(starting_weights) < 0.99 | sum(starting_weights) > 1.01) {
    stop("Stock weights must sum to one")
  }
  
  #Function to calculate Sharpe Ratio for Optional Weights
  if (is.null(starting_weights) != TRUE) {
      #If weights are close to one then normalize
      if (sum(starting_weights) != 1) {
          starting_weights <- starting_weights / sum(starting_weights)
      }
      #Calculate Sharpe ratio for given weights
      #Expected Portfolio Return
      ER_p <- starting_weights %*% Returns_annualized
      #Expected Portfolio Standard Deviation- annualize by multiplying by number of
      #trading days (251)
      Std_P <- sqrt(t(starting_weights) %*% (var(data) * 251) %*% starting_weights)
      #Sharpe Ratio for 
      Sharpe_matrix[1, 1:n_stocks] <- starting_weights
      Sharpe_matrix[1, (n_stocks + 1)] <- (ER_p - risk_free) / Std_P
  }  
  
  #Starting weights- use grid search with number of iterations proportional to number
  #of stocks in portfolio
  if (is.null(starting_weights)) {
    Random_search_weights <- Random_Grid_Search(iterations = min(1000, 10000 / n_stocks), 
                                                n_stocks, Returns_annualized, data, risk_free, 
                                                Sharpe_matrix)$Sharpe_matrix
    starting_weights <- Random_search_weights[which.max(Random_search_weights[, 
                                                                              (n_stocks + 1)]), 1:n_stocks]
    Sharpe <- Random_search_weights[which.max(Random_search_weights[, (n_stocks + 1)]), (n_stocks + 1)]
    
  }
  
  #Set indexing and Booleans for Convergence of Algorithm and Continuation of loop
  i <- 2
  conv <- FALSE
  loop <- TRUE
  
  #Add row for starting weights and corresponding Sharpe Ratio
  Sharpe_matrix<- rbind(c(starting_weights, Sharpe), Sharpe_matrix)
  
  #Create vector of zeros
  Zero_vector <- c(rep(0, n_stocks))
  
  #Stopping schedule
  while (loop) {
    #Propose a new set of weights
    weights <- pmax(rnorm(n_stocks, mean = Sharpe_matrix[(i - 1), 1:n_stocks], sd = 0.1 / 
                       (delta ^ 2)), Zero_vector)
    normalized_weights <- weights / sum(weights)
    
    #Calculate Sharpe ratio for given weights
    #Expected Portfolio Return
    ER_p <- normalized_weights %*% Returns_annualized
    #Expected Portfolio Standard Deviation- annualize by multiplying by number of
    #trading days (251)
    Std_P <- sqrt(t(normalized_weights) %*% (var(data) * 251) %*% normalized_weights)
    
    Sharpe <- (ER_p - risk_free) / Std_P    
    
    #Add new weights if Sharpe Ratio is greater, otherwise use previous weights
    if (Sharpe > Sharpe_matrix[(i - 1), (n_stocks + 1)]) {
      Sharpe_matrix[i, ] <- c(normalized_weights, Sharpe)
    }else{
      Sharpe_matrix[i, ] <- Sharpe_matrix[(i - 1), ]
    }
    
    #Stop loop once max. iterations reached
    if (i > iterations) {
      loop <- FALSE
      #display whether algorithm has converged
      if (Sharpe_matrix[i, (n_stocks + 1)] == 
          Sharpe_matrix[(max((i - 100), 1)), (n_stocks + 1)]) {
        conv <- TRUE
      }
    }
    i <- i + 1
    delta <- delta ^ cooling_schedule
  }
  #Return Sharpe Matrix as a list and identify weights corresponding to greatest 
  #Sharpe Ratio
  OptimizationResults <- list("Sharpe_matrix" = Sharpe_matrix)
  return(OptimizationResults = OptimizationResults)
}

library(ggplot2)
# Colour Palettes
library(viridis)

#Plot for initial data- Microsoft, Walmart, Delta Airlines
Sharpe_matrix <- as.data.frame(Sharpe_matrix)
ggplot(Sharpe_matrix) +
  geom_point(aes(Sharpe_matrix[, 5], Sharpe_matrix[, 3], colour = Sharpe_matrix[, 6]), 
             size = 3) + 
  geom_path(aes(Sharpe_matrix[, 5], Sharpe_matrix[, 3])) + 
  scale_color_viridis(option = "B") + 
  labs(x = "Walmart Weight", y = "Apple Weight", 
       title ="Simulated Aneeling Path of Weights", colour = "Sharpe Ratio") 

# Plot for alternative data- Microsoft, Amazon, Walmart

# 1) Boundary Testing -----------------------------------------------------



# 2) Unit Root Testing ----------------------------------------------------


