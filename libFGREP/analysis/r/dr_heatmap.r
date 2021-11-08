## ============================================================================
## Filename          : $RCSfile: dr_heatmap.r,v $
## Version           : $Revision: 1.5 $ 
## Release           : $Name:  $  
##
## Original author   : Peter Dreisiger, MOD, DSTO Stirling
## Original date     : 3-Mar-2009
## Last modified by  : $Author: prd $ 
## Last modified on  : $Date: 2009/04/01 10:00:01 $ 
##
## Security class.   : UNCLASSIFIED
## Copyright         : DSTO
##
## Purpose           : This R function loads an FGREP console output file
##                     and plots its corresponding heatmap.
##
## Parameters        : [filename]  the name of the FGREP console output file
##                     [first]  the index of the first term to be analysed 
##                     [last]   the index of the last term to be analysed
##                     [power]  the power to which the distances will be raised
##                     [numColours]  the number of colours in the colour map
##                     [filter]  a vector of the term label prefixes that will
##                         be used to further filter the list of terms to be
##                         plotted
##                     [cex]  the factor by which to scale the terms' labels
##
## Revision history  :
##
## Notes             : Inspired by http://www2.warwick.ac.uk/fac/sci/moac/
##                         currentstudents/peter_cock/r/heatmap/;  for an
##                     alternative, see ../matrix_contour/
## ============================================================================
dr_heatmap <- function(filename, first=1, last=0, power=1, numColours=256, filter=c("."), cex=1.0) {
  # Preliminaries --- ensure that the gplots library has been loaded as that's
  # where colorpanel lives
  require(gplots)

  # First, we load the data and extract the terms' labels,
  data <- read.table(filename, header = FALSE, comment.char="")
  if (last == 0) last = dim(data)[1]
  label <- as.character(data[,1])

  hits <- grep(paste("^", filter[1], sep=""), label[first:last])
  if (length(filter) > 1) {
    for (i in 2:length(filter))
      hits <- c(hits, grep(paste("^", filter[i], sep=""), label[first:last]))
    hits <- sort(hits)
  }

  # then we construct the distance matrix
  distances <- as.matrix(dist(data[hits, 2:(dim(data)[2]-1)], method="euclidean"))^power
  rownames(distances) <- label[hits]
  colnames(distances) <- label[hits]

  heatmap(distances, scale="none", col=c("red", colorpanel(numColours-1, "white", "grey10")),
          symm=TRUE, revC=TRUE, cexRow=cex, cexCol=cex)

  # $RCSfile: dr_heatmap.r,v $    $Revision: 1.5 $
}
