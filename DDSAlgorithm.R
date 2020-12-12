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

#Random Search

DDS_RandomSearch <- function(iterations = 1000, returns = 'yes', data, 
                             risk_free = 1.01, random.seed = 1234){
  
  n_stocks <- dim(data)[2]
  observations <- dim(data)[1]
  #If no Expected Returns are set, calculate expected returns as past returns- set
  #other conditions to ensure they have the same numerical value as returns and also
  #have the correct format- i.e. a dataframe of floats.
  if (returns != 'yes') {
    returns <- c()
  }
  
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
  #Identify weights corresponding to greatest Sharpe Ratio
  return(Sharpe_matrix[which.max(Sharpe_matrix[, (n_stocks + 1)]), 1:n_stocks])
}

#Could also use a spatial plot/ heat map for the Sharpe Ratio


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
