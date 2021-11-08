## ============================================================================
## Filename          : $RCSfile: analyse_latent_clusters.r,v $
## Version           : $Revision: 1.2 $
## Release           : $Name:  $
##
## Original author   : Peter Dreisiger, MOD, DSTO Stirling
## Original date     : 3-Mar-2009
## Last modified by  : $Author: prd $
## Last modified on  : $Date: 2009/04/01 06:44:31 $
##
## Security class.   : UNCLASSIFIED
## Copyright         : DSTO
##
## Purpose           : This function accepts the output of batch_analyse_terms,
##                     calculates the overall and category average errors-
##                     squared and delta-Rs, and plots them 2-up.
##
## Parameters        : [stats]  an object containing the terms' statistics
##
## Revision history  :
## ============================================================================
analyse_latent_clusters <- function(filename, categories, first=1, last=0, regexpPrefix="^") {
  # First, we load the data,
  data <- read.table(filename, header = FALSE, comment.char="")
  if (last == 0) last = dim(data)[1]
  drrange <- 2:(dim(data)[2]-1)


  # Next, 
  arraySize <- c(length(categories), dim(data)[2]-2)
  cluster <- list(mean=array(dim=arraySize), sd=array(dim=arraySize),
                  radius.mean=array(dim=length(categories)),
                  radius.sd=array(dim=length(categories)),
                  size=array(dim=length(categories)),
                  intrusions=array(dim=length(categories)))


  for (category in 1:length(categories)) {
    hits <- grep(paste(regexpPrefix, categories[category], sep=""), data[, 1])

    cluster$mean[category, ] <- as.vector(mean(data[hits, drrange]))
    cluster$sd[category, ]   <- as.vector(sd(data[hits, drrange]))
    
    distance <- array(dim=length(hits))
    i <- 1
    for (term in hits) {
      distance[i] <- sqrt(sum((data[term, drrange] - cluster$mean[category, ])^2))
      i <- i + 1
    }
    cluster$radius.mean[category] <- mean(distance)
    cluster$radius.sd[category]   <-   sd(distance)
  }

  # Next, we concatenate the representations with their category means and
  # work out the distance matrix;  this will then allow us to work out the
  # number of cluster intrusions
  numTerms <- dim(data)[1]
  combined <- array(dim=c(numTerms + length(categories), dim(data)[2]-2))
  combined[1:numTerms, ]                    <- as.matrix(data[, drrange])
  combined[(numTerms+1):dim(combined)[1], ] <- as.matrix(cluster$mean)
  distance <- as.matrix(dist(combined, method="euclidean"))
  for (i in 1:length(categories)) {
    hits <- grep(paste(regexpPrefix, categories[i], sep=""), data[, 1])
    cluster$size[i] <- length(hits)
    maxDist <- max(distance[numTerms+i, hits])
    cluster$intrusions[i] <- -length(hits)
    for (j in 1:numTerms)
      if (distance[numTerms+i, j] <= maxDist)
        cluster$intrusions[i] <- cluster$intrusions[i] + 1
  }

  return(list(file=paste(getwd(), filename, sep="/"), categories=categories,
              mean=cluster$mean, sd=cluster$sd,
              radius.mean=cluster$radius.mean, radius.sd=cluster$radius.sd,
              separation=dist(cluster$mean),
              separation.normalised=dist(cluster$mean)/sqrt(dim(data)[2]-2),
              intrusions=cluster$intrusions,
              size=cluster$size))

  # $RCSfile: analyse_latent_clusters.r,v $    $Revision: 1.2 $
}
