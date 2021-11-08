## ============================================================================
## Filename          : $RCSfile: dr_cluster.r,v $
## Version           : $Revision: 1.3 $
## Release           : $Name:  $
##
## Original author   : Peter Dreisiger, MOD, DSTO Stirling
## Original date     : 3-Mar-2009
## Last modified by  : $Author: prd $
## Last modified on  : $Date: 2009/04/01 09:26:58 $
##
## Security class.   : UNCLASSIFIED
## Copyright         : DSTO
##
## Purpose           : This R function loads an FGREP console output file,
##                     performs a hierarchical cluster analysis on the terms'
##                     distributed representations, and plots the resulting
##                     dendrogram.
##
## Parameters        : [filename]  the name of the FGREP console output file
##                     [first]  the index of the first term to be analysed
##                     [last]   the index of the last term to be analysed
##                     [cutheight]  the height at which the hierarchical 
##                         cluster tree should be cut
## Returns           : the list of clusters, as determined by [cutheight]
##                     together with the names of each clusters' members
##
## Revision history  :
## ============================================================================
dr_cluster <- function(filename, first=1, last=0, cutheight=0.01) {
  # First, we load the data,
  data <- read.table(filename, header = FALSE, comment.char="")
  if (last == 0) last = dim(data)[1]

  # then we perform the hierarchical cluster analysis.
  distances <- dist(data[first:last, 2:(dim(data)[2]-1)], method="euclidean")
  hc <- hclust(distances, "average")

  # Finally, we cut the tree at the specified height and construct the list
  # of the clusters and their members.
  cuts <- cutree(hc, h=cutheight)
  cluster <- vector("list", max(cuts))
  for (i in 1:length(cuts)) {
    cluster[[ cuts[i] ]] = c(cluster[[ cuts[i] ]], as.character(data[i,1]))
  }

  return(cluster)

  # $RCSfile: dr_cluster.r,v $    $Revision: 1.3 $
}
