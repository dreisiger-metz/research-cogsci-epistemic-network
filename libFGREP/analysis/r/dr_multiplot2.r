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
dr_multiplot2 <- function(filename, filter=c(""), colour=NULL, colour.default="red", regexpPrefix="^", cex=0.6) {
  # Preliminaries --- ensure that the MASS library has been loaded as that's
  # where parcoord is kept
  require(MASS)

  # First, we read the file and set the various data variables
  rawdata <- read.table(filename, header = FALSE, comment.char="");
  indices <- vector("list", length(filter))
  for (i in 1:length(filter))
    indices[[i]] <- grep(paste(regexpPrefix, filter[i], sep=""), as.character(rawdata[, 1]))

  data     <- rawdata[c(indices, recursive=TRUE), ]
  label    <- as.character(data[,1])
  dr.range <- 2:(dim(data)[2] - 1)
  colnames(data) <- c("term", paste("DR[", c(1:length(dr.range)), "]", sep=""), "error")

  
  # Then we perform a hierarchical cluster analysis and MDS
  distances <- dist(data[, dr.range], method="euclidean")
  hc     <- hclust(distances, "average")
  scaled <- cmdscale(distances)

  # Next, we go through the label, prefix and colour vectors and work out each
  # term's line colour.  Note that we can probably simplify this just by over-
  # plotting the indices returned by grep in the colour used to find them.
  colour.list <- NULL
  for (i in 1:length(label)) {
    choice <- colour.default
    for (j in 1:length(filter)) {
      if (length(grep(paste(regexpPrefix, filter[j], sep=""), as.character(label[i]))) > 0) {
        choice <- colour[j]
      }
    }
    colour.list = c(colour.list, choice)
  }

  # Finally, we plot the distributed representations and the dendrogram.
  split.screen(c(2,1))
  split.screen(c(1,2), 1)   # creates screen 1 (tl), 2 (b) and 4 (tr)
  screen(1)
  title(paste("Plots of", filename))
  
  screen(3)
  parcoord(data[, dr.range], colour.list, , TRUE, main="")
  legend("bottomright", c(filter, ""), lty=1, col=colour, bty='n', cex=cex)
  screen(2)
  plot(hc, labels=label, cex=cex, xlab="", sub="", main="", hang=-0.25)
  screen(4)
  plot(scaled, type="p", pch=".", cex=3, xlab="", ylab="", col=colour.list)
  text(scaled[, 1], scaled[, 2], label, cex=cex, pos=1, offset=(max(scaled[, 2]) - min(scaled[, 2]))/10, col=colour.list)
  
  close.screen(all = TRUE)

  # $RCSfile: dr_multiplot.r,v $    $Revision: 1.3 $
}
