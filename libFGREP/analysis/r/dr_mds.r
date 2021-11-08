## ============================================================================
## Filename          : $RCSfile: dr_mds.r,v $
## Version           : $Revision: 1.6 $
## Release           : $Name:  $
##
## Original author   : Peter Dreisiger, MOD, DSTO Stirling
## Original date     : 3-Mar-2009
## Last modified by  : $Author: prd $
## Last modified on  : $Date: 2009/04/01 11:44:32 $
##
## Security class.   : UNCLASSIFIED
## Copyright         : DSTO
##
## Purpose           : This R function loads an FGREP console output file,
##                     performs a classical multi-dimensional scaling the
##                     terms' distributed representations, and plots their
##                     2-D reduction.
##
## Parameters        : [filename]  the name of the FGREP console output file
##                     [first]  the index of the first term to be scaled
##                     [last]   the index of the last term to be scaled
##                     [filters]  a vector of the term label prefixes that will
##                         be used to further filter the list of terms
##                     [colour]  a list of their corresponding colours
##                     [defaultcolour]  the colour to use for all other terms
##                     [cex]  the factor by which to scale the terms' labels
##
## Revision history  :
## ============================================================================
dr_mds <- function(filename, first=1, last=0, filters=c("."), colours=NULL, defaultColour="black", cex=0.6, includesErrors=TRUE) {
  # First, we load the data and extract the terms' labels
  data <- read.table(filename, header = FALSE, comment.char="")
  if (last == 0) last = dim(data)[1]
  label  <- as.character(data[, 1])
  colour <- array(defaultColour, dim=dim(data)[1])

  # Next, we generate an index vector based upon our bounds and filter expression
  index <- grep(paste("^", filters[1], sep=""), label[first:last])
  if (length(colours) > 0) colour <- array(colours[1], dim=length(index))
  if (length(filters) > 1) {
    for (i in 2:length(filters)) {
      hits  <- grep(paste("^", filters[i], sep=""), label[first:last])
      index <- c(index, hits)
      if (length(colours) > 0) colour <- c(colour, array(colours[i], dim=length(hits)))
    }
  }

  # Now, we calculate the inter-term distances and their scaled coordinates
  if (includesErrors == TRUE)
    distances <- dist(data[index, 2:(dim(data)[2]-1)], method="euclidean")
  else 
    distances <- dist(data[index, 2:dim(data)[2]], method="euclidean")
  scaled <- cmdscale(distances)

  # And finally, we generate the MDS plot using the specified colours, or the
  # default if none were given.
  offset <- (max(scaled[, 2]) - min(scaled[, 2])) / 10
  plot(scaled, type="p", pch=".", cex=3, xlab="", ylab="", main=paste("MDS Plot of", filename), col=colour)
  text(scaled[, 1], scaled[, 2], label[index], cex=cex, pos=1, offset=offset, col=colour)

  # $RCSfile: dr_mds.r,v $    $Revision: 1.6 $
}
