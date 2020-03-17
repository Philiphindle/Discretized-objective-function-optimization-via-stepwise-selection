
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
    sigma_p <- sum((x[i,j]^2) * Cov[j, j]) 
  }
  #Calculate sharpe ratio for row i (ith set of weights)
  Sharpe[i] <- (p - r_f) / sigma_p
}
#Return the row index of the weights matrix that yields the optimal Sharpe Ratio
which.max(Sharpe)

system.time()
# D) Assess Performance of Algorithm --------------------------------------


