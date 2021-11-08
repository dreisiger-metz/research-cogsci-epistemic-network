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
analyse_trial_terms<- function(filename, categories=c('.'), relativeCutHeight=0.01, regexpPrefix="^") {


  # ===========================================================================
  # Read the file, extract the desired terms and return a structure containing
  # the data, ranges and membership details
  # ===========================================================================
  read_file <- function(filename, categories=c("."), regexpPrefix="^") {
    rawdata <- read.table(filename, header=FALSE, comment.char="")
    members <- vector("list", length(categories))
    for (i in 1:length(categories))
      members[[i]] <- grep(paste(regexpPrefix, categories[i], sep=""), as.character(rawdata[, 1]))

    data <- rawdata[c(members, recursive=TRUE), ]
    rownames(data) <- NULL
    colnames(data) <- c("term", paste("DR[", c(1:(dim(data)[2] - 2)), "]", sep=""), "error")

    # re-grep (data) to correct the member indices
    for (i in 1:length(categories))
      members[[i]] <- grep(paste(regexpPrefix, categories[i], sep=""), as.character(data[, 1]))
    
    return(list(data=data, terms=as.character(data[, 1]), categories=categories,
                members=members, filename=filename, index.dr=2:(dim(data)[2] - 1),
                index.error=dim(data)[2]))
  }
      


  
  # ===========================================================================
  # Go through (data) and calculate the global mean, sd, error and radius;
  # do the same for each of the clusters defined by (membership);  finally,
  # compute some statistics from the clusters' data
  # ===========================================================================
  analyse_clusters <- function(data, membership) {
    # Calculate the global statistics,
    global.mean <- colMeans(data$data[, data$index.dr])
    global.sd   <- sd(data$data[, data$index.dr])
    global.error.mean <- mean(data$data[, data$index.error])
    global.error.sd   <- sd(data$data[, data$index.error])
    representations <- array(dim=c(dim(data$data)[1] + 1, length(data$index.dr)))
    representations[1:(dim(data$data)[1]), ] <- as.matrix(data$data[, data$index.dr])
    representations[dim(data$data)[1] + 1, ] <- as.matrix(global.mean)
    global.radius <- max((as.matrix(dist(representations, method="euclidean")))[dim(data$data)[1] + 1, ])

    # calculate the per-cluster statistics,
    cluster <- vector("list", length(membership))
    for (i in 1:length(cluster)) {
      cluster.mean <- colMeans(data$data[membership[[i]], data$index.dr])
      cluster.sd   <- sd(data$data[membership[[i]], data$index.dr])
      cluster.error.mean <- mean(data$data[membership[[i]], data$index.error])
      cluster.error.sd   <- sd(data$data[membership[[i]], data$index.error])
      representations <- array(dim=c(length(membership[[i]]) + 1, length(data$index.dr)))
      representations[1:length(membership[[i]]), ]   <- as.matrix(data$data[membership[[i]], data$index.dr])
      representations[length(membership[[i]]) + 1, ] <- as.matrix(cluster.mean)
      cluster.radius <- max((as.matrix(dist(representations, method="euclidean")))[length(membership[[i]]) + 1, ])

      # determine the number of intrusions --- i.e. the number of terms in
      # (data$data) that are not in (membership[[i]]), but lie within
      # (cluster.radius) of (cluster.mean)
      representations <- array(dim=c(dim(data$data)[1] + 1, length(data$index.dr)))
      representations[1:(dim(data$data)[1]), ] <- as.matrix(data$data[, data$index.dr])
      representations[dim(data$data)[1] + 1, ] <- as.matrix(cluster.mean)
      distances <- (as.matrix(dist(representations, method="euclidean")))[dim(data$data)[1] + 1, ]
      cluster.intrusions <- sum(distances <= cluster.radius) - (length(membership[[i]]) + 1)    # take centroid out
      
      cluster[[i]] <- list(members=membership[[i]], mean=cluster.mean, sd=cluster.sd,
                           error.mean=cluster.error.mean, error.sd=cluster.error.sd,
                           radius=cluster.radius, intrusions=cluster.intrusions)
    }

    # and finally, calculate the inter-cluster statistics
    representations <- array(dim=c(length(membership), length(data$index.dr)))
    radii <- array(dim=length(cluster))
    intrusions <- array(dim=length(cluster))
    for (i in 1:length(cluster)) {
      representations[i, ] <- as.matrix(cluster[[i]]$mean)
      radii[i] <- cluster[[i]]$radius
      intrusions[i] <- cluster[[i]]$intrusions
    }
    
    cluster.radius.mean <- mean(radii) / global.radius
    cluster.radius.sd   <- sd(radii) / global.radius
    cluster.intrusions.mean <- mean(intrusions)
    cluster.intrusions.sd   <- sd(intrusions)
    cluster.separation.mean <- mean(dist(representations)) / global.radius
    cluster.separation.sd   <- sd(dist(representations)) / global.radius
    cluster.density <- sum((radii / global.radius)^length(data$index.dr))
    

    return(list(mean=global.mean, sd=global.sd, error.mean=global.error.mean, error.sd=global.error.sd, radius=global.radius,
                cluster.radius.mean=cluster.radius.mean, cluster.radius.sd=cluster.radius.sd,
                cluster.intrusions.mean=cluster.intrusions.mean, cluster.intrusions.sd=cluster.intrusions.sd,
                cluster.separation.mean=cluster.separation.mean, cluster.separation.sd=cluster.separation.sd,
                cluster.density=cluster.density,
                cluster=cluster))
  }



  analyse_latent_clusters <- function(data, relativeCutHeight=0.01) {
    # Create the hierarchical clusters,
    distances <- dist(data$data[, data$index.dr], method="euclidean")
    hc      <- hclust(distances, "average")
    cuts    <- cutree(hc, h=max(hc$height) * relativeCutHeight)

    # and determine their members
    membership <- vector("list", max(cuts))
    for (i in 1:length(membership)) {
      members <- NULL
      for (j in 1:length(cuts))
        if (cuts[j] == i)
          members <- c(members, j)

      membership[[i]] <- members
    }

    return(analyse_clusters(data, membership))
  }



  analyse_reference_clusters <- function(data) {
    return(analyse_clusters(data, data$members))
  }



  # Do it!  Read the file, and analyse the latent and reference clusters
  data <- read_file(filename, categories)
  analysis_latent    <- analyse_latent_clusters(data, relativeCutHeight)
  analysis_reference <- analyse_clusters(data, data$members)

  return(list(data=data, latent=analysis_latent, reference=analysis_reference))
}
