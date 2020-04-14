library(doParallel)
library(parallel)

eval.noise <- function(trials,n,tt,C,g,n.workers,path.dist,path.coint){
  #'Evaluate 
  #'
  #'@param trials int, number of simulations to run 
  #'@param n int, dimension of noise matrix
  #'@param tt int, number of time points to simulate
  #'@param C 2 x 2 matrix, covariance of errors in cointegrated system
  #'@param g int, strength of cointegrating relationship 
  #'@param n.workers int, number of parallel workers to create 
  #'@param path.dist path to distance functions
  #'@param path.coint path to functions for simulating cointegrated systems
  
  cl <- makeCluster(n.workers)
  registerDoParallel(cl)
  
  res <- foreach(i = 1:trials, .combine = "c") %dopar% {
    source(path.dist)
    source(path.coint)
    
    x <- cointegrated.series(tt,g,C)
    X <- matrix(rnorm(n*tt), ncol = n)
    
    sim.noise <- min(skeleton.similarity.comp(x[,1],X,"pcwsLinMean", parallel.comp = FALSE))
    sim.coint <- skeleton.similarity(x,"pcwsLinMean", parallel.comp = FALSE)
    ifelse((sim.noise>sim.coint),TRUE,FALSE)
  }
  stopCluster(cl)
  mean(res)
}


eval.dependant.noise <- function(trials,n,tt,C,g,prob,n.workers,path.dist,path.coint){
  #'
  #'
  #'@param trials int, number of simulations to run 
  #'@param n int, dimension of noise matrix
  #'@param tt int, number of time points to simulate
  #'@param C 2 x 2 matrix, covariance of errors in cointegrated system
  #'@param g int, strength of cointegrating relationship 
  #'@param prob float, probability of AR processes in noise vector 
  #'@param n.workers int, number of parallel workers to create 
  #'@param path.dist path to distance functions
  #'@param path.coint path to functions for simulating cointegrated systems
  
  cl <- makeCluster(n.workers)
  registerDoParallel(cl)
  
  res <- foreach(i = 1:trials, .combine = "c") %dopar% {
    source(path.dist)
    source(path.coint)
    
    x <- cointegrated.series(tt,g,C)
    X <- dependant.noise.vec(tt,n,prob)
    
    sim.noise <- min(skeleton.similarity.comp(x[,1],X,"pcwsLinMean", parallel.comp = FALSE))
    sim.coint <- skeleton.similarity(x,"pcwsLinMean", parallel.comp = FALSE)
    ifelse((sim.noise>sim.coint),TRUE,FALSE)
  }
  stopCluster(cl)
  mean(res)
}


eval.dependant.noise <- function(trials,n,tt,C,g,prob,n.workers,path.dist,path.coint){
  #'
  #'
  #'@param trials int, number of simulations to run 
  #'@param n int, dimension of noise matrix
  #'@param tt int, number of time points to simulate
  #'@param C 2 x 2 matrix, covariance of errors in cointegrated system
  #'@param g int, strength of cointegrating relationship 
  #'@param float, probability of AR processes in noise vector 
  #'@param n.workers int, number of parallel workers to create 
  #'@param path.dist path to distance functions
  #'@param path.coint path to functions for simulating cointegrated systems
  
  cl <- makeCluster(n.workers)
  registerDoParallel(cl)
  
  res <- foreach(i = 1:trials, .combine = "c") %dopar% {
    source(path.dist)
    source(path.coint)
    
    x <- cointegrated.series(tt,g,C)
    X <- dependant.noise.vec(tt,n,prob)
    
    sim.noise <- min(skeleton.similarity.comp(x[,1],X,"pcwsLinMean", parallel.comp = FALSE))
    sim.coint <- skeleton.similarity(x,"pcwsLinMean", parallel.comp = FALSE)
    ifelse((sim.noise>sim.coint),TRUE,FALSE)
  }
  stopCluster(cl)
  mean(res)
}

eval.nonstationary.noise <- function(trials,n,tt,C,g,prob,n.workers,path.dist,path.coint){
  #'
  #'
  #'@param trials int, number of simulations to run 
  #'@param n int, dimension of noise matrix
  #'@param tt int, number of time points to simulate
  #'@param C 2 x 2 matrix, covariance of errors in cointegrated system
  #'@param g int, strength of cointegrating relationship 
  #'@param float, probability of AR processes in noise vector 
  #'@param n.workers int, number of parallel workers to create 
  #'@param path.dist path to distance functions
  #'@param path.coint path to functions for simulating cointegrated systems
  
  cl <- makeCluster(n.workers)
  registerDoParallel(cl)
  
  res <- foreach(i = 1:trials, .combine = "c") %dopar% {
    source(path.dist)
    source(path.coint)
    
    x <- cointegrated.series(tt,g,C)
    X <- nonstationary.noise.vec(tt,n,prob)
    
    sim.noise <- min(skeleton.similarity.comp(x[,1],X,"pcwsLinMean", parallel.comp = FALSE))
    sim.coint <- skeleton.similarity(x,"pcwsLinMean", parallel.comp = FALSE)
    ifelse((sim.noise>sim.coint),TRUE,FALSE)
  }
  stopCluster(cl)
  mean(res)
}


eval.cointegrated.noise <- function(trials,n,h,tt,C.x,C.X,g,n.workers,path.dist,path.coint){
  #'
  #'
  #'@param trials int, number of simulations to run 
  #'@param n int, dimension of noise matrix
  #'@param h int, number of cointegrating relations 
  #'@param tt int, number of time points to simulate
  #'@param C.x 2 x 2 matrix, covariance of errors in cointegrated system
  #'@param C.X n x n matrix, covariance of errors in cointegrated noise vector 
  #'@param g int, strength of cointegrating relationship 
  #'@param n.workers int, number of parallel workers to create 
  #'@param path.dist path to distance functions
  #'@param path.coint path to functions for simulating cointegrated systems
  
  cl <- makeCluster(n.workers)
  registerDoParallel(cl)
  
  res <- foreach(i = 1:trials, .combine = "c") %dopar% {
    source(path.dist)
    source(path.coint)
    
    x <- cointegrated.series(tt,g,C.x)
    X <- cointegrated.vector(tt,n,h,C.X)

    sim.noise <- min(skeleton.similarity.comp(x[,1],X,"pcwsLinMean", parallel.comp = FALSE))
    sim.coint <- skeleton.similarity(x,"pcwsLinMean", parallel.comp = FALSE)
    ifelse((sim.noise>sim.coint),TRUE,FALSE)
  }
  stopCluster(cl)
  sum(res)
}
