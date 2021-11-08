## ============================================================================
## Filename          : $RCSfile: batch_dr_cluster.r,v $
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
## Purpose           : This R function iterates through a set of subdirectories
##                     and calls dr_cluster(...) for each of the 'iteration--
##                     100000.term' files.
##
## Parameters        : [drsize]  the size of the distributed representations
##                         to plot; the subdirectories' prefix
##                     [first]  the first of the trials to be plotted
##                     [last]   the last of the trials to be plotted
##                     [cutheight]  the height at which the hierarchical
##                         cluster trees should be cut
##                     [fixedterms]  the number of clusters to ignore;  this
##                         should correspond to the number of fixed terms
## Returns           : an array consisting of the trial's index and the number
##                     of non-fixed-term clusters inherent in the data
##
## Revision history  :
## ============================================================================
batch_dr_cluster <- function(drsize, first=0, last=24, cutheight=0.01, fixedterms=3) {
  filenames <- paste(as.character(drsize), "--", first:last, "/iteration--100000.term", sep = "")
  clusters <- array(0, dim=c(last - first + 1, 2))

  i <- 0
  for (file in filenames) {
    clusters[i + 1, 1] <- first + i
    clusters[i + 1, 2] <- length(dr_cluster(file, cutheight=cutheight)) - fixedterms
    i <- i + 1
  }

  return(clusters)

  # $RCSfile: batch_dr_cluster.r,v $    $Revision: 1.2 $
}
