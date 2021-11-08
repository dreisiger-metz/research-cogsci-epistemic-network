## ============================================================================
## Filename          : $RCSfile: dr_dendrogram.r,v $
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
## Returns           : the output of hclust(...)
##
## Revision history  :
## ============================================================================
dr_dendrogram <- function(filename, first=1, last=0, power=1.0, cex=0.75, title=paste("Hierarchy of DRs from", filename), hang=TRUE) {
  # First, we load the data and extract the terms' labels,
  data <- read.table(filename, header = FALSE, comment.char="")
  if (last == 0) last = dim(data)[1]
  label <- data[,1]

  # then we perform the hierarchical cluster analysis,
  distances <- dist(data[first:last, 2:(dim(data)[2]-1)], method="euclidean")
  hc <- hclust(distances^power, "average")

  # and finally, we plot the dendrogram.
  if (hang == TRUE)
    plot(hc, labels=label[first:last], cex=cex, xlab="", main=title, sub="", hang=-0.25)
  else
    plot(hc, labels=label[first:last], cex=cex, xlab="", main=title, sub="")

  # Note that, to get a horizonal dendrogram, we need to do something like this:
  #   > for (i in 1:length(hc$labels)) hc$labels[i] = as.character(data[i, 1])
  #   > plot(as.dendrogram(hc), horiz=TRUE)
  # (though we do need to do something about the labels' placement)

  return(hc)

  # $RCSfile: dr_dendrogram.r,v $    $Revision: 1.3 $
}
