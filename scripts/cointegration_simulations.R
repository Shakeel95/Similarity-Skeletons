library(docstring)
library(pracma)
library(mvtnorm)
library(pbapply)


cointegrated.series <- function(tt,g,C,burn = 1000){
  #'Simulate two cointegrated time series
  #'
  #'@param tt int, time points to simulate
  #'@param g float in [-1,1], strength of cointegrating relationship
  #'@param C innovation covariance structure
  #'@param burn
  
  tot.t <- tt+burn
  X <- matrix(0,1+tot.t,2)
  innov <- rmvnorm(tot.t,rep(0,2),C)
  for (i in 1:tot.t){
    X[i+1,1] <- X[i,1] + innov[i,1]
    X[i+1,2] <- g*X[i+1,1] + innov[i,2]
  }
  X[-c(1:(burn+1)),]
}


cointegrated.vector <- function(tt,n,h,C,burn = 1000){
  #'Simulate n cointegrated time series
  #'
  #'@param tt int, time points to simulate
  #'@param n int, number of time series
  #'@param h int, number of cointegrating retaions
  #'@param C innovation covariance structure
  #'@param burn
  
  tot.t <- tt+burn
  innov <- rmvnorm(tot.t,rep(0,n),C)
  X <- matrix(0,1+tot.t,n)
  
  A <- randortho(n)[1:h,]
  if (h == 1) Phi <- diag(n) - outer(A,A)
  else Phi <- diag(n) - t(A)%*%A
  
  for (i in 1:tot.t){
    X[i+1,] <- Phi%*%X[i,] + innov[i,]
  }
  X[-c(1:(burn+1)),]
}


dependant.noise.vec <- function(tt,n, prob){
  #'Vector process with mutually independent serially correlated entries
  #'
  #'@param tt int, time points to simulate#
  #'@param n int, number of time series
  #'@param probs float, probability of AR processes
  
  X <- matrix(,tt,n)
  
  dependant.noise <- function(tt,prob){
    p <- which(rmultinom(1,1,prob) == 1)
    if (p == 1){
      arima.sim(model = list(ar = runif(1)), tt)
    } else if (p == 2) {
      arima.sim(model = list(ma = runif(1)), tt)
    } else {
      rnorm(tt)
    }
  }
  
  for (i in 1:n) X[,i] <- dependant.noise(tt,prob)
  X
}


nonstationary.noise.vec <- function(tt,n,prob){
  #'Vector process with mutually independent unit root processes 
  #'
  #'@param tt int, time points to simulate#
  #'@param n int, number of time series
  #'@param probs float, probability of drift in random walk 
  
  X <- matrix(,tt,n)
  
  non.stationary.noise <- function(tt,prob){
    drift <- rbinom(1,1,prob)*runif(1,-1,1)
    X <- matrix(0,tt,1)
    X[1,] <- drift
    for (i in 1:(tt-1)){
      X[i+1,] <- X[i,] + drift + rnorm(1)
    }
    X
  }
  
  for (i in 1:n) X[,i] <- non.stationary.noise(tt,prob)
  X
}

