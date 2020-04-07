library(docstring) # EZ documentation
library(snow); library(parallel) # parallel processing
library(pbapply) # lovely progress bar
library(not) # fit linear skeleton

skeleton.similarity <- function (X, set.contrast, parallel.comp = TRUE) {
  #'Skeleton similarity
  #'
  #'Computes similarity measure for a panel of time series by fitting a changepoint based "skeleton" 
  #'(piecewise constant or piecewise linear) to each series computing the Frechet distance between skeletons. 
  #'
  #'@param X matrix or dataframe of dimension (T x p)
  #'@param set.contrast contrast function used in the NOT algorithm. Choice of "pcwsConstMean", "pcwsLinMean"
  #'@param parallel.comp if TRUE similarity dmatrix is computed in parallel
  
  
  frechet <- function (idxs, skeletons) {
    # Frechet distance between two time series skeletons
    
    if (idxs[1] == idxs[2]) return(0.0)
    P <- skeletons[[idxs[1]]]
    Q <- skeletons[[idxs[2]]]
    max.cpt.dist <- max(sapply(c(P$cpt,Q$cpt),
                               function(i) abs(P$skeleton[i] - Q$skeleton[i])))
    if (anyNA(max.cpt.dist)) return(1)
    else return(max.cpt.dist)
  }
  
  
  not.skeleton <- function(i, df, set.contrast) {
    # Construct skeleton using NOT algorithm
    
    require(not)
    w <- not(df[,i], contrast = set.contrast)
    x.fit <- predict(w)
    return(list(skeleton = (x.fit - min(x.fit))/(max(x.fit - min(x.fit))),
                cpt = features(w)$cpt))
  }
  
  if (parallel.comp){
    print("fitting time series skeletons (in parallel)")
    cl <- makeSOCKcluster(rep("localhost",detectCores()-1))
    skeletons <- parLapply(cl,1:ncol(X),not.skeleton, df = X, set.contrast = set.contrast)
    stopCluster(cl)
  } else {
    print("fitting time series skeletons")
    skeletons <- pblapply(1:ncol(X),not.skeleton, df = X, set.contrast = set.contrast)
  }
  
  size <- ncol(X)
  print("calculating distance matrix...")
  d <- pbapply(combn(size,2),2,frechet, skeletons = skeletons)
  
  attr(d, "Size") <- size
  xnames <- colnames(X)
  if (!is.null(xnames)) {
    attr(d, "Labels") <- xnames
  }
  attr(d, "Diag") <- FALSE
  attr(d, "Upper") <- FALSE
  class(d) <- "dist"
  
  return(d)
}