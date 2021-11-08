## ============================================================================
## Filename          : $RCSfile: percepts_analyse_and_plot_clusters.r,v $
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
## Purpose           : This R function calls batch_analyse_dr_clusters for the
##                     specified DR size, and then calls dr_multiple for each
##                     trial with between [minclusters] and [maxclusters]
##                     clusters (inclusively).
##
## Parameters        : [drsize]  the size of the distributed representations
##                         to plot; the subdirectories' prefix
##                     [first]  the first of the trials to be plotted
##                     [last]   the last of the trials to be plotted
##                     [minclusters]  the minimum number of clusters for which
##                         dr_multiplot(...) should be called
##                     [maxclusters]  the maximum number of clusters for which
##                         dr_multiplot(...) should be called
##
## Revision history  :
## ============================================================================
percepts_analyse_and_plot_clusters <- function(drsize, first=0, last=24, minclusters=7, maxclusters=11) {
  # Preliminaries --- ensure that the DAAG library has been loaded as that's
  # where pause is kept
  require(DAAG)

  prefix <- c("colour", "height", "motion", "speed", "texture", "width")
  colour <- c("red",    "blue",   "green",  "grey",  "purple",  "lightblue")

  clustercount <- batch_analyse_dr_clusters(drsize, first=first, last=last)

  for (i in 1:length(clustercount)) {
    if ((clustercount[i] >= minclusters) && (clustercount[i] <= maxclusters)) {
      write(paste("Trial ", i - 1, " has ", clustercount[i] - 3, " non-trivial clusters", sep=""), file="")
      file <- paste(drsize, "--", i - 1, "/iteration--100000.term", sep="")
      dr_multiplot(file, prefix=prefix, colour=colour, defaultcolour="white")
      pause()
    }
  }

  # $RCSfile: percepts_analyse_and_plot_clusters.r,v $    $Revision: 1.2 $
}
