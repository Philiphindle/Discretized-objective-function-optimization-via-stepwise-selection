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

DDS_RandomSearch <- function(n_stocks = 10, iterations = 100, returns, data, 
                             risk_free = 0.5){
  
  #If no Expected Returns are set, calculate expected returns as past returns
  if (is.null(returns)) {
    returns <- c(colmeans(data))
  }
  
  #Initialize Vector of Weights
  weights <- c(rep(0, n_stocks))
  #Matrix to store weights and Sharpe Ratio
  Sharpe_matrix <- matrix(data = NA, nrow = iterations, ncol = n_stocks + 1)
  
  for(i in 1:iterations){
    weights <- c(runif(n_stocks))
    normalized_weights <- weights / sum(weights)
    #Calculate Sharpe ratio for given weights
    #Expected Portfolio Return
    ER_p <- normalized_weights * returns
    #Expected Portfolio Standard Deviation
    Std_P <- t(w) %*% var(data) %*% w
    #Sharpe Ratio
    Sharpe <- (ER_p - risk_free) / Std_P
  }
  #Identify weights corresponding to greatest Sharpe Ratio
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
data <- as.data.frame(matrix())
