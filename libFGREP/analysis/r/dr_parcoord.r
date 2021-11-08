## ============================================================================
## Filename          : $RCSfile: dr_parcoord.r,v $
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
## Purpose           : This R function loads an FGREP console output file,
##                     and displays a parallel-coordinates plot of the raw
##                     distributed representations.
##
## Parameters        : [filename]  the name of the FGREP console output file
##                     [first]  the index of the first term to be plotted
##                     [last]   the index of the last term to be plotted
##                     [prefix]  a list of the term prefixes to be plotted
##                     [colour]  a list of their corresponding colours
##                     [defaultcolour]  the colour to use for all other terms
##                     [mode]  if set to 'mds' then a multidimensional scaling
##                         will be performed before the the plotting
##
## Revision history  :
## ============================================================================
dr_parcoord <- function(filename, first=1, last=0, prefix="", colour=NULL, defaultcolour="red", mode="raw") {
  # Preliminaries --- ensure that the MASS library has been loaded as that's
  # where parcoord is kept
  require(MASS)

  # First, we read the file and extract the terms' labels.
  data <- read.table(filename, header = FALSE, comment.char="");
  if (last == 0) last = dim(data)[1];
  label <- data[,1]

  # If [mode] is "mds", then perform a multi-dimensional scaling.
  if (mode == "mds") {
    distances <- dist(data[, 2:(dim(data)[2]-1)], method="euclidean");
    data <- cmdscale(distances, k=(dim(data)[2] - 2));
  }

  # Next, we go through the label, prefix and colour vectors and work out each
  # term's line colour,
  colourlist <- NULL
  for (i in 1:length(label)) {
    choice <- defaultcolour
    for (j in 1:length(prefix)) {
      if (strsplit(as.character(label[i]), "-")[[1]][1] == prefix[j]) {
        choice <- colour[j]
      }
    }
    colourlist = c(colourlist, choice)
  }

  # and finally, we plot the distributed representations.
  if (mode == "mds")
    title <- paste("Multi-dimensional scaled parallel coordinates plot of", filename)
  else
    title <- paste("Parallel coordinates plot of", filename)
  parcoord(data[first:last, 2:(dim(data)[2]-1)], colourlist[first:last], , TRUE, main=title)
  legend("bottomright", c(prefix, ""), lty=1, col=c(colour, "white"), bty='n', cex=0.6)

  # $RCSfile: dr_parcoord.r,v $    $Revision: 1.2 $
}
