#Monte Carlo Simulatio of the St Petersburg Paradox Game


# Pseudo Code -------------------------------------------------------------

#start with £2
#toss a coin until we get the first heads- each is Binomially distributed 
#with probability 0.5 of success and stop at the first zero.

#Use object oriented programming by placing functions within functions

#Code for entire Simulation
#for i simulations
  #for j games:
    #function to play game once
      #store winnings of each game
      #take a total winnings and running average

#Code for Single game
#if get a head
  #tosses <- tosses + 1
  #winnings <- 2 ^ tosses
  #play game again
#else
  #return winnings


# Algorithm ---------------------------------------------------------------

Coin_Toss <- function(winnings = 2, tosses = 1){
  #Function to play the St Petersburg Paradox Game once
  #inputs:
    #winnings: starting amount of winnings
    #tosses: starting amount of tosses
  #ouput: 
    #total winnings
  
  #simulate coin toss- if heads continue game, else end game
  if (rbinom(1, 1, 0.5) < 1) {
    tosses <- tosses + 1
    winnings <- 2 ^ tosses
    #repeat coin toss until a tail
    Coin_Toss(winnings = winnings, tosses = tosses)
  }else{
    return(winnings)
  }
}

# Simulation --------------------------------------------------------------

#Single Simulation
Coin_Game <- function(n_games = 1000){
  winnings <- c(rep(0, n_games))
  average_win <- c(rep(NA, n_games))  
  for (i in 1:n_games) {
    winnings[i] <- Coin_Toss()
    average_win[i] <- sum(winnings) / i
  }
  return(average_win)
}

Coin_Sim <- function(n_games = 1000, n_sim = 1){
  #Function to simulate games of a given length
  #Inputs:
    #n_games: number of games to carry out for each simulation
    #n_sim: number of simulations
  #Ouput:
    #matrix of average winnings for each simulation
  
  #Matrix to store average winnings
  mat <- matrix(NA, nrow = n_games, ncol = n_sim)
  for (i in 1:n_sim) {
    mat[,i] <- Coin_Game(n_games)
  }
  return(mat)
}


# Simulation Graphs -------------------------------------------------------

#Plotting Function
set.seed(160001695)
Plot_Function <- function(matrix){
  plot(matrix[,1], type = "l", ylim = c(0, 40), xlab = "Number of Games", 
       ylab = "Rolling Average")
  for (i in 2:ncol(matrix)) {
    lines(matrix[, i], col = i)
  }
}

#10 simulations of 1000 games
set.seed(160001695)
z <- Coin_Sim(n_games = 100000, n_sim = 5)
Plot_Function(z)
