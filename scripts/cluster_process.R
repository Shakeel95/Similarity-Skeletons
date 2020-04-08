library("stringdist")

country.cluster.info <- function(hc,k = NULL,h = NULL,countries.info){
  #'Get info from country clusters
  #'
  #'@param hc
  #'@param k 
  #'@param h 
  #'@param countries.info
  
  if (is.null(k) && is.null(h)) stop("either k or h must be specified")
  
  country.matchings <- sapply(hc$labels, 
                              function(i) countries.info[which.min(stringdist(i,countries.info$Country)),])
  clusters <- cutree(hc, k = k, h = h)
  df <- data.frame(t(country.matchings),clusters)
  
  cluster.counts <- sapply(unique(df$clusters), function(i) c(i,sum(df$clusters == i)))
  cluster.counts <- data.frame(t(cluster.counts))
  names(cluster.counts) <- c("cluster","count")
  
  return(list(cluster.info = df, cluster.counts = cluster.counts))
}

country.cluster.boxplot <- function(cluster){
  #'Makes barplots for .... 
  #'
  #'@param cluster data frame
  
  par(mfrow = c(2,1))
  
  reg.counts <- sapply(unique(cluster$Region), function(i) c(i,sum(cluster$Region == i)))
  dev.counts <- sapply(unique(cluster$Development), function(i) c(i,sum(cluster$Development == i)))
  
  barplot(as.numeric(reg.counts[2,]), names.arg = reg.counts[1,], main = "Regions")
  barplot(as.numeric(dev.counts[2,]), names.arg = dev.counts[1,], main = "Development classifications")
}

ticker.cluster.info <- function(hc, k = NULL, h = NULL, SnP500.info){
  #'Get info from ticker clusters
  #'
  #'@param hc
  #'@param k 
  #'@param h 
  #'@param countries.info
  
  if (is.null(k) && is.null(h)) stop("either k or h must be specified")
  
  ticker.matchings <- sapply(hc$labels,
                             function(i) SnP500.info[which(SnP500.info$Symbol == i),])
  clusters <- cutree(hc, k = k, h = h)
  df <- data.frame(t(ticker.matchings),clusters)
  
  cluster.counts <- sapply(unique(df$clusters), function(i) c(i,sum(df$clusters == i)))
  cluster.counts <- data.frame(t(cluster.counts))
  names(cluster.counts) <- c("cluster","count")
  
  return(list(cluster.info = df, cluster.counts = cluster.counts))
}

ticker.cluster.boxplot <- function(cluster){
  #'
  #'
  #'@param cluster
  
  par(mfrow = c(2,1))
  
  sector.counts <- sapply(unique(cluster$GICS.Sector), function(i) c(i, sum(cluster$GICS.Sector == i)))
  sub.industry.counts <- sapply(unique(cluster$GICS.Sub.Industry), function(i) c(i, sum(cluster$GICS.Sub.Industry == i)))
  
  barplot(as.numeric(sector.counts[2,]), names.arg = sector.counts[1,], main = "Sectors")
  barplot(as.numeric(sub.industry.counts[2,]), names.arg = sub.industry.counts[1,], main = "Sub-Industries")
}
