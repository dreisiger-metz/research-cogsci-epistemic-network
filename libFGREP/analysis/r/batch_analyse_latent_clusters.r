batch_analyse_latent_clusters <- function(drsize, categories, path=".", filename="iteration--100000.term", numberOfTrials=25, verbose=TRUE, regexpPrefix="^") {
  filename <- paste(path, "/", drsize, "--", 0:(numberOfTrials-1), "/", filename, sep="")

  results <- vector(mode="list", length=numberOfTrials)
  if (verbose) cat("Analysing trials", file="")
  for (i in 1:length(filename)) {
    if (verbose) cat(".")
    results[[i]] <- analyse_latent_clusters(filename[i], categories, regexpPrefix=regexpPrefix)
  }
  cat("  Done!\n")


  if (verbose) cat("Computing cluster averages.")
  sd                    <- results[[1]]$sd
  radius.mean           <- results[[1]]$radius.mean
  radius.sd             <- results[[1]]$radius.sd
  separation            <- results[[1]]$separation
  separation.normalised <- results[[1]]$separation.normalised
  intrusions.mean       <- results[[1]]$intrusions
  intrusions.sd         <- results[[1]]$intrusions
  size                  <- results[[1]]$size

  intrusions     <- array(dim=c(numberOfTrials, length(results[[1]]$intrusions)))
  intrusions[1,] <- as.vector(results[[1]]$intrusions)
  
  for (i in 2:length(results)) {
    if (verbose) cat(".")
    sd                    <- sd + results[[i]]$sd
    radius.mean           <- radius.mean + results[[i]]$radius.mean
    radius.sd             <- radius.sd + results[[i]]$radius.sd
    separation            <- separation + results[[i]]$separation
    separation.normalised <- separation.normalised + results[[i]]$separation.normalised
    intrusions[i,]        <- as.vector(results[[i]]$intrusions)
  }
  sd                    <- sd / length(results)
  radius.mean           <- radius.mean / length(results)
  radius.sd             <- radius.sd / length(results)
  separation            <- separation / length(results)
  separation.normalised <- separation.normalised / length(results)
  intrusions.mean       <- colMeans(intrusions)
  intrusions.sd         <- sd(intrusions)
  cat("  Done!\n")

  return(list(means=list(file=paste(getwd(), "/", drsize, "--*/iteration--100000.term", sep=""),
                         categories=categories, sd=sd, radius.mean=radius.mean, radius.sd=radius.sd, 
                         separation=separation, separation.normalised=separation.normalised,
                         intrusions.mean=intrusions.mean, intrusions.sd=intrusions.sd, size=size),
              results=results))

  # $RCSfile: batch_analyse_latent_clusters.r,v $    $Revision: 1.2 $
}
