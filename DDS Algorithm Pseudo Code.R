#Number of rows (iterations). i.e. if n=10 then we increase the weights by 0.1 by each iteration
n <- 100
#Number of Stocks (columns)
m <- 10
#Vector of Expected Returns
R <- c(rnorm(m, mean = 10, sd = 2))
#Matrix of weights
x <- matrix(NA, nrow = (n+1), ncol = (m))
#Covariance Matrix
Cov <- matrix(N)
#k = 1 Case- only consider the first stock for which we are changing the weights
for(i in 0:n){
  #Number of columns that excludes the ith column for the weights we are adjusting and exludes the columns for
  #the previously adjusted stock weights
  m <- ncol(x) - 1
  for(j in 1:m){
    if(j = 1){
      x[i,j] <- i / n
    }else{
      x[i,j] <- (1 - i/n) / m
    }
    #Calculate expected return on portfolio
    p <- p + x[i,j] * R[j]
    sigma_p <- sum((x[i,j]^2))
  }
  Sharpe[i] <- (p - r_f) / sigma_p
  f<- 0
}
which.max(Sharpe)