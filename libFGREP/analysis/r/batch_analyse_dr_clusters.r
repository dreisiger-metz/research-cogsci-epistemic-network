## ============================================================================
## Filename          : $RCSfile: batch_analyse_dr_clusters.r,v $
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
## Purpose           : This R function iterates through a set of subdirectories,
##                     calls dr_cluster(...) for each 'iteration--100000.term'
##                     file, and writes the trial index and its corresponding
##                     number of latent clusters to the 'drsize.cluster' file.
##
## Parameters        : [drsize]  the size of the distributed representations
##                         to plot; the subdirectories' prefix
##                     [first]  the first of the trials to be plotted
##                     [last]   the last of the trials to be plotted
##                     [cutheight]  the height at which the hierarchical
##                         cluster trees should be cut
## Returns           : a vector consisting of the number of clusters inherent
##                     in the data
##
## Revision history  :
## ============================================================================
batch_analyse_dr_clusters <- function(drsize, first=0, last=24, cutheight=0.01) {
  filenames <- paste(drsize, "--", first:last, "/iteration--100000.term", sep="")
  clustercount <- vector(mode="integer", length=last-first+1)

  i <- 0
  write("", file=paste(drsize, ".cluster", sep=""))
  for (file in filenames) {
    clusters <- dr_cluster(file, cutheight=cutheight)
    write(paste(length(clusters), file, sep="\t"), file=paste(drsize, ".cluster", sep=""), append=TRUE)

    write(paste(1, clusters[1][[1]], sep="\t"), file=paste(file, ".cluster", sep=""))
    for (j in 2:length(clusters)) {
      write(paste(j, clusters[j][[1]], sep="\t"), file=paste(file, ".cluster", sep=""), append=TRUE)
    }

    clustercount[i + 1] <- length(clusters)
    i <- i + 1
  }

  return(clustercount)

  # $RCSfile: batch_analyse_dr_clusters.r,v $    $Revision: 1.2 $
}
