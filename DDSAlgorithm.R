#Can we do simulated aneeling but at each step.

#See Hide and Seek Algorithm
#OR-simulate values from a uniform distribution


# 1) K=1 ------------------------------------------------------------------

#First consider the simplified case where we look at only the first stock/weight in the 
#matrix of weights


# A) Initialize Parameters ------------------------------------------------

#Number of rows (iterations). i.e. if n=10 then we increase the weights by 0.1 by each iteration
n <- 100
#Number of Stocks (columns)
m <- 10
#Risk free interest rate
r_f <- 2
#Vector of Expected Returns
set.seed(1000) #For reproducibility
R <- c(rnorm(m, mean = 10, sd = 2))
#(Empty) Matrix of weights
x <- matrix(NA, nrow = (n+1), ncol = (m))
#(Empty) Covariance Matrix
Cov <- matrix(NA, nrow = m, ncol = m)
#Create empty portfolio variance vector
sigma_p <- c(rep(NA, m))
#Create empty sharpe ratio vector
Sharpe <- c(rep(NA, (n + 1)))


# B) Simulate Known Data --------------------------------------------------

#Iteratively fill in the elements of the matrix
for (i in 1:m) {
  for (j in 1:m) {
    set.seed(j * i) #For reproducibility
    #element [i,j]
    Cov[i, j] <- runif(1, 0, 10)
    #Ensure symmetry property of the covariance matrix
    Cov[j, i] <- Cov[i, j]
  }
}


# C) K=1 Algorithm --------------------------------------------------------

#k = 1 Case- only consider the first stock for which we are changing the weights
for(i in 0:n){
  #Number of columns that excludes the ith column for the weights we are adjusting and exludes 
  #the columns for the previously adjusted stock weights
  m <- ncol(x) - 1
  #Reset the portfolio return
  p <- 0
  for(j in 1:m){
    if(j == 1){
      x[i,j] <- i / n
    }else{
      x[i,j] <- (1 - i/n) / m
    }
    #Calculate expected return on portfolio
    p <- p + x[i,j] * R[j]
    #Calculate variance on portfolio
    sigma_p[j] <- ((x[i,j]^2) * Cov[j, j]) 
  }
  #Calculate sharpe ratio for row i (ith set of weights)
  Sharpe[i] <- (p - r_f) / sum(sigma_p)
}
#Return the row index of the weights matrix that yields the optimal Sharpe Ratio
which.max(Sharpe)
system.time()

# D) Assess Performance of Algorithm --------------------------------------

#Pre-processing function to calculate daily stock returns if stock prices are
#provided instead of returns

# Portfolio Optimization Algorithm ----------------------------------------

DDS_RandomSearch <- function(iterations = 1000, returns = 'yes', data, 
                             risk_free = 1.01, random.seed = 1234, 
                             method = "Grid Search"){
  
  #Check input arguments are of correct structure and value
  # 1. Checking Iterations is a positive integer
  if (iterations <= 0 | iterations != as.integer(iterations)) 
  stop("Number of Iterations must be a positive integer")
  
  # 2. Checking data is a dataframe with number of rows equal to number of
  #    observations and number of columns equal to number of stocks
  
  # 3. Check risk_free rate is an integer greater than or equal to one
  
  #1) Check if data and other variables are correct format
  #2) Check if daily returns or daily stock price has been provided
  #3) Check risk free return is just over 1, etc.
  
  n_stocks <- dim(data)[2]
  observations <- dim(data)[1]
  #If no Expected Returns are set, calculate expected returns as past returns- set
  #other conditions to ensure they have the same numerical value as returns and also
  #have the correct format- i.e. a dataframe of floats.
  
  Returns_annualized <- c(rep(NA, n_stocks))
  #Calculate annualized returns from daily returns
  for (i in 1:n_stocks) {
    Returns_annualized[i] <- prod(1 + data[, i]) ^ (251 / observations)
  }
  
  #Initialize Vector of Weights
  weights <- c(rep(0, n_stocks))
  #Matrix to store weights and Sharpe Ratio
  Sharpe_matrix <- matrix(data = NA, nrow = iterations, ncol = n_stocks + 1)
  
  set.seed(random.seed)
  if (method == "Grid Search") {
      Grid_Search <- Random_Grid_Search(iterations, n_stocks, Returns_annualized, 
                                        data, risk_free, Sharpe_matrix)  
  }

  #Identify weights corresponding to greatest Sharpe Ratio
  return(list("Sharpe_matrix" = Grid_Search$Sharpe_matrix, "Optimal_weights" = 
                Grid_Search$Sharpe_matrix[which.max(Grid_Search$Sharpe_matrix[, (n_stocks + 1)]), 
                              1:n_stocks]))
}


# 1) Random Search Optimization ----------------------------------------------

Random_Grid_Search <- function(iterations, n_stocks, Returns_annualized, data, 
                               risk_free, Sharpe_matrix){
  for(i in 1:iterations){
    weights <- c(runif(n_stocks))
    normalized_weights <- weights / sum(weights)
    #Calculate Sharpe ratio for given weights
    #Expected Portfolio Return
    ER_p <- normalized_weights %*% Returns_annualized
    #Expected Portfolio Standard Deviation
    Std_P <- sqrt(t(normalized_weights) %*% var(data) %*% normalized_weights)
    #Sharpe Ratio
    Sharpe_matrix[i, 1:n_stocks] <- normalized_weights
    Sharpe_matrix[i, (n_stocks + 1)] <- (ER_p - risk_free) / Std_P
  }
  #Return Sharpe Matrix as a list and identify weights corresponding to greatest 
  #Sharpe Ratio
  OptimizationResults <- list("Sharpe_matrix" = Sharpe_matrix)
  return(OptimizationResults = OptimizationResults)
}

# 2) Simulated Aneeling Optimization --------------------------------------

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
  }
  
}

# Testing -----------------------------------------------------------------

#Vector of daily returns for each simulated stock
#1000 trading days in total
n <- 1000
set.seed(1234)
Stock1 <- c(rnorm(n, 0.0002, 0.001))
#calculate annualized returns, assuming 251 annual trading days- 7.1% annualized return
#for stock 1
prod(1 + Stock1)^(251/n)
Stock2 <- c(rnorm(n, 0.0004, 0.01))
prod(1 + Stock2)^(251/n)
Stock3 <- c(rnorm(n, 0.0006, 0.05))
prod(1 + Stock3)^(251/n)
#Plot Returns
plot(Stock1, type = 'l')
#Plot Stock Price
Stock_Price <- c()
Stock_Price[1] <- 100
for (i in 2:n) {
  Stock_Price[i] <- Stock_Price[i - 1] * (1 + Stock3[i])
}
plot(Stock_Price, type = 'l')
#BUT -we want to introduce some positive correlation between the stock returns and 
#simulate this to really think about how the function is working (and not just test
#that it syntatically works)
data <- as.data.frame(matrix(c(Stock1, Stock2, Stock3), ncol = 3))

#Could also use a spatial plot/ heat map for the Sharpe Ratio
library(fields)
quilt.plot(x = Sharpe_matrix[, 1], y = Sharpe_matrix[, 2], z = Sharpe_matrix[, 4], 
           nrow = 50, ncol = 50)
plot(Sharpe_matrix[, 1], Sharpe_matrix[, 4])

DDS_RandomSearch(data = data, risk_free = 1.02)

#Plot with some real test data
#Consider stock in Microsoft, Walmart and Delta Air Lines
MSFT <- read.csv("data/MSFT.csv")["Adj.Close"]
DAL <- read.csv("data/DAL.csv")["Adj.Close"]
WMT <- read.csv("data/WMT.csv")["Adj.Close"]
data <- cbind(MSFT, DAL, WMT)

#Calculate Simple Returns
Returns <- matrix(NA, nrow = (dim(data)[1] - 1), ncol = 3)
for (i in 1:(dim(data)[1] - 1)) {
  Returns[i, ] <- as.matrix(data)[i + 1, ] / as.matrix(data)[i, ]
}
Returns <- Returns - 1
data <- Returns
Test <- DDS_RandomSearch(data = Returns, iterations = 100000)

#Spatial plot
library(fields)
quilt.plot(x = Test$Sharpe_matrix[, 1], y = Test$Sharpe_matrix[, 3], 
           z = Test$Sharpe_matrix[, 4], nrow = 50, ncol = 100)
plot(Sharpe_matrix[, 1], Sharpe_matrix[, 4])

# Boundary Testing --------------------------------------------------------

#Check iterations is appropriately specified
DDS_RandomSearch(data = Returns, iterations = -1)
DDS_RandomSearch(data = Returns, iterations = 27.3)
DDS_RandomSearch(data = Returns, iterations = c(2, 3, 4))

#We could also profile the code and aim for parallelization to improve
#computational speed
