#Import Optimization Functions
source("Optimization-Methods/Stochastic-Grid-Search.R")
source("Optimization-Methods/Bisection-Method.R")
source("Optimization-Methods/Deterministic-Grid-Search.R")
source("Optimization-Methods/Gauss-Newton.R")
source("Optimization-Methods/Simulated-Aneeling.R")
#MIGHT NEED TO PUT THESE INSIDE THE DDS FUNCTION, WHEN CALLED IN THE ARGUMENTS


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
      DDS_optimization <- Random_Grid_Search(iterations, n_stocks, Returns_annualized, 
                                        data, risk_free, Sharpe_matrix)  
  }

  #Identify weights corresponding to greatest Sharpe Ratio
  return(list("Sharpe_matrix" = DDS_optimization$Sharpe_matrix, "Optimal_weights" = 
                DDS_optimization$Sharpe_matrix[which.max(DDS_optimization$Sharpe_matrix[, 
                  (n_stocks + 1)]), 1:n_stocks], "Optimal_sharpe_ratio" = 
                DDS_optimization$Sharpe_matrix[which.max(DDS_optimization$Sharpe_matrix[, 
                  (n_stocks + 1)]), (n_stocks + 1)]))
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


# Computational Speed and Performance -------------------------------------

# Consider the computational speed of the different optimization methods and plot
#a graph showing how this speed is affected both by the number of possible stocks
#and the number of iterations.
