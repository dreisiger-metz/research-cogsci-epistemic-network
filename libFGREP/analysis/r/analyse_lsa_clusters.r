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
analyse_lsa_clusters <- function(directory, categories, first=1, last=0, regexpPrefix="^") {
  # Preliminaries --- ensure that the MASS library has been loaded as that's
  # where parcoord is kept
  require(MASS)
  require(lsa)

  # First, we read the file and extract the terms' labels,
  T   <- textmatrix(directory, stemming=FALSE, stopwords=NULL)
  T   <- lw_logtf(T) * gw_idf(T)
  LSA <- lsa(T, dims=dimcalc_share())
  label <- rownames(T)


  # and then we perform a hierarchical cluster analysis.
  data <- matrix(as.matrix(LSA$tk), nrow=dim(LSA$tk)[1], ncol=dim(LSA$tk)[2])


  # Next, 
  arraySize <- c(length(categories), dim(data)[2])
  cluster <- list(mean=array(dim=arraySize), sd=array(dim=arraySize),
                  radius.mean=array(dim=length(categories)),
                  radius.sd=array(dim=length(categories)),
                  size=array(dim=length(categories)),
                  intrusions=array(dim=length(categories)))


  for (category in 1:length(categories)) {
    hits <- grep(paste(regexpPrefix, categories[category], sep=""), label)

    for (i in 1:dim(data)[2]) {
      cluster$mean[category, i] <- mean(data[hits, i])
      cluster$sd[category, i]   <- sd(data[hits, i])
    }
    
    distance <- array(dim=length(hits))
    i <- 1
    for (term in hits) {
      distance[i] <- sqrt(sum((data[term, ] - cluster$mean[category, ])^2))
      i <- i + 1
    }
    cluster$radius.mean[category] <- mean(distance)
    cluster$radius.sd[category]   <-   sd(distance)
  }

  # Next, we concatenate the representations with their category means and
  # work out the distance matrix;  this will then allow us to work out the
  # number of cluster intrusions
  numTerms <- dim(data)[1]
  combined <- array(dim=c(numTerms + length(categories), dim(data)[2]))
  combined[1:numTerms, ]                    <- as.matrix(data[, ])
  combined[(numTerms+1):dim(combined)[1], ] <- as.matrix(cluster$mean)
  distance <- as.matrix(dist(combined, method="euclidean"))
  for (i in 1:length(categories)) {
#write(paste("Intruders for category '", categories[i], "' are:", sep=""), file="")
    hits <- grep(paste(regexpPrefix, categories[i], sep=""), label)
    cluster$size[i] <- length(hits)
    maxDist <- max(distance[numTerms+i, hits])
    cluster$intrusions[i] <- -length(hits)
    for (j in 1:numTerms)
      if (distance[numTerms+i, j] <= maxDist) {
#if (length(grep(categories[i], label[j])) == 0)
#  write(paste("    ", label[j], sep=""), file="")
        cluster$intrusions[i] <- cluster$intrusions[i] + 1
      }
  }

  return(list(directory=paste(getwd(), directory, sep="/"), categories=categories,
              mean=cluster$mean, sd=cluster$sd,
              radius.mean=cluster$radius.mean, radius.sd=cluster$radius.sd,
              separation=dist(cluster$mean),
              separation.normalised=dist(cluster$mean)/sqrt(dim(data)[2]),
              intrusions=cluster$intrusions,
              size=cluster$size))

  # $RCSfile: analyse_latent_clusters.r,v $    $Revision: 1.2 $
}
