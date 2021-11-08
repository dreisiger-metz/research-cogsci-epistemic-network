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
analyse_trial <- function(filename, categories = c('.'), relativeCutHeight = 0.01, regexpPrefix = "^", sortResults = TRUE) {
  # Read the file, extract the desired terms and set the index and name variables
  rawdata <- read.table(filename, header = FALSE, comment.char = "")
  categoryIndex <- vector('list', length(categories))
  for (i in 1:length(categories))
    categoryIndex[[i]] <- grep(paste(regexpPrefix, categories[i], sep=""), as.character(rawdata[,1]))

  data     <- rawdata[c(categoryIndex, recursive=TRUE),]
  terms    <- as.character(data[,1])
  drRange  <- 2:(dim(data)[2] - 1)
  errIndex <- dim(data)[2]
  colnames(data) <- c('term', paste('DR[', c(1:length(drRange)), ']', sep=''), 'error')


  # Perform the hierarchical cluster analysis, cut the tree, and for each resulting
  # cluster, determine the members, the standard deviations and the average errors
  distances <- dist(data[, drRange], method="euclidean")
  hc <- hclust(distances, "average")
  cuts    <- cutree(hc, h=max(hc$height) * relativeCutHeight)
  cluster <- vector("list", max(cuts))
  
  global.centroid <- colMeans(data[drRange])
  representations <- array(dim=c(dim(data)[1]+1, length(drRange)))
  representations[1:(dim(data)[1]),] <- as.matrix(data[, drRange])
  representations[dim(data)[1]+1,]   <- as.matrix(global.centroid)
  global.radius   <- max((as.matrix(dist(representations, method='euclidean')))[dim(data)[1]+1,])
  global.sd       <- sqrt(diag(var(data[, drRange])))

  for (i in 1:length(cluster)) {
    members <- NULL
    for (j in 1:length(cuts))
      if (cuts[j] == i)
        members <- c(members, j)

    centroid <- colMeans(data[members, drRange])
    representations <- array(dim=c(length(members)+1, length(drRange)))
    representations[1:length(members),] <- as.matrix(data[members, drRange])
    representations[length(members)+1,] <- as.matrix(centroid)
    radius <- max((as.matrix(dist(representations, method='euclidean')))[length(members)+1,])
    sd <- sqrt(diag(var(data[members, drRange])))
    error.mean <- mean(data[members, errIndex])

    cluster[[i]] <- list(members=members, centroid=centroid, radius=radius, radius.scaled=radius/global.radius, sd=sd, sd.mean=mean(sd), sd.scaled=sd/global.sd, error.mean=error.mean)
  }


  # Calculate the global characteristics of the latent clusters --- i.e. their
  # average radii, variances and separation, normalised wrt the radius of the
  # complete set of representations
  representations <- array(dim=c(length(cluster), length(drRange)))
  radii <-  array(dim=c(length(cluster)))
  errors.mean <- array(dim=c(length(cluster)))
  for (i in 1:length(cluster)) {
    representations[i,] <- as.matrix(cluster[[i]]$centroid)
    radii[i] <- cluster[[i]]$radius
    errors.mean[i] <- cluster[[i]]$error.mean
  }
  clusterSeparation.mean.scaled <- mean(dist(representations)) / global.radius
  clusterRadius.mean.scaled     <- mean(radii) / global.radius
  clusterError.mean             <- mean(errors.mean)


  # Finally, go back through the data to determine the number of cluster intrusions


  # Return the results
  return(list(numberOfClusters=length(cluster), global.radius=global.radius, global.sd=global.sd,
              clusterSeparation.mean.scaled=clusterSeparation.mean.scaled,
              clusterRadius.mean.scaled=clusterRadius.mean.scaled, clusterError.mean=clusterError.mean,
              cluster=cluster, data=data))
  
  # $RCSfile: analyse_latent_clusters.r,v $    $Revision: 1.2 $
}
