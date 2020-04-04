library(oro.nifti)
library(docstring)
library(unbalhaar)


nii.to.tseries <- function(nii.obj){
  #' nii object to time series
  #' 
  #' Converts an object of class nii.obj to a 2-dimenaional array
  #' representing a standard panel of time series
  #' 
  #' @param nii.obj
  
  d <- dim(nii.obj)
  dd <- d[1]*d[2]*d[3]
  t.series <- matrix(,dd,d[4])
  
  for (u in 1:d[1]) for (v in 1:d[2]) for (w in 1:d[3]){
    cur.ind <- (u-1)*d[2]*d[3] + (v-1)*d[3] + w
    t.series[cur.ind,] <- nii.obj[u,v,w,]
  }
  return(t(t.series))
}


cusum.locs <- function(t.series, threshold = FALSE){
  #' Extract cusum statistic
  #' 
  #' @param t.series a p x T array
  
  exceeds.thresh.ind <- NA
  cusum.max.locs <- NA 
  cusums <- apply(t.series,
                  MARGIN = 2, 
                  FUN = function(x){abs(inner.prod.iter(x/sqrt(var(x))))})
  cusums.maxima <- apply(cusums, 2, max)
  
  if (threshold){
    exceeds.thresh.ind <- which(cusums.maxima > threshold) 
    cusum.max.locs <- apply(cusums[,exceeds.thresh.ind],2,which.max) 
  }
  
  list(cusums=cusums,
       cusums.maxima=cusums.maxima,
       exceeds.thresh.ind=exceeds.thresh.ind,
       cusum.max.locs=cusum.max.locs)
  
}
