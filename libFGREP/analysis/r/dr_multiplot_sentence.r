## ============================================================================
## Filename          : $RCSfile: dr_multiplot.r,v $
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
##                     distributed representations, and plots the raw repre-
##                     sentations and the resulting dendrogram.
##
## Parameters        : [filename]  the name of the FGREP console output file
##                     [first]  the index of the first term to be plotted
##                     [last]   the index of the last term to be plotted
##                     [prefix]  a list of the term prefixes to be plotted
##                     [colour]  a list of their corresponding colours
##                     [defaultcolour]  the colour to use for all other terms
##
## Revision history  :
## ============================================================================
dr_multiplot <- function(filename, first=1, last=0, prefix="", colour=NULL, defaultcolour="red") {
  # Preliminaries --- ensure that the MASS library has been loaded as that's
  # where parcoord is kept
  require(MASS)

  # First, we read the file and extract the terms' labels,
  data <- read.table(filename, header = FALSE, comment.char="");
  if (last == 0) last = dim(data)[1];
  label <- data[,1]

  # and then we perform a hierarchical cluster analysis.
  distances <- dist(data[first:last, 2:(dim(data)[2]-1)], method="euclidean")
  hc <- hclust(distances, "average")

  # Next, we go through the label, prefix and colour vectors and work out each
  # term's line colour.  Note that we can probably simplify this just by over-
  # plotting the indices returned by grep in the colour used to find them.
  colourlist <- NULL
  for (i in 1:length(label)) {
    choice <- defaultcolour
    for (j in 1:length(prefix)) {
      if (length(grep(paste("^", prefix[j], sep=""), as.character(label[i]))) > 0) {
        choice <- colour[j]
      }
    }
    colourlist = c(colourlist, choice)
  }

  # Finally, we plot the distributed representations and the dendrogram.
  split.screen(c(2,1))
  screen(1)
  parcoord(data[first:last, 2:(dim(data)[2]-1)], colourlist[first:last], , TRUE, main=paste("Parallel coordinates plot of", filename));
  legend("bottomright", c(prefix, ""), lty=1, col=c(colour, "white"), bty='n', cex=0.6)
  screen(2)
  plot(hc, labels=label[first:last], cex=0.6, xlab="", main=paste("Dendrogram of", filename), sub="", hang=-0.25)
  close.screen(all = TRUE)

  # $RCSfile: dr_multiplot.r,v $    $Revision: 1.3 $
}
