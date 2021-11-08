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
lsa_multiplot <- function(directory, prefix="", filters=c('.'), colours=NULL, defaultcolour="red", main=paste("Dendrogram of", directory)) {
  # Preliminaries --- ensure that the MASS library has been loaded as that's
  # where parcoord is kept
  require(MASS)
  require(lsa)

  # First, we read the file and extract the terms' labels,
  T   <- textmatrix(directory, stemming=FALSE, stopwords=NULL)
  T   <- lw_logtf(T) * gw_idf(T)
  LSA <- lsa(T, dims=dimcalc_share())
  label <- rownames(T)
  colour <- array(defaultcolour, dim=length(label))


  # and then we perform a hierarchical cluster analysis.
  distances <- dist(LSA$tk, method="euclidean")
  #distances <- dist(as.textmatrix(LSA$tk), method="euclidean")
  #distances <- cosine(t(as.textmatrix(LSA)))
  hc <- hclust(as.dist(distances), "average")

  # Next, we go through the label, prefix and colour vectors and work out each
  # term's line colour.
  hits <- grep(paste("^", filters[1], sep=""), label)
  if (length(colours) > 0) colour[hits] <- colours[1]
  if (length(filters) > 1) {
    for (i in 2:length(filters)) {
      hits <- grep(paste("^", filters[i], sep=""), label)
      if (length(colours) > 0) colour[hits] <- colours[i]
    }
  }

  # Finally, we plot the distributed representations and the dendrogram.
#  split.screen(c(2,1))
#  screen(1)
#  parcoord(LSA$tk, colour, , TRUE, main=paste("Parallel coordinates plot of", directory));
#  legend("bottomright", c(prefix, ""), lty=1, col=c(colour, "white"), bty='n', cex=0.6)
#  screen(2)
##  plot(as.dendrogram(hc), cex=0.6, xlab="", main=paste("Dendrogram of", directory), sub="", horiz=TRUE)
##  plot(hc, cex=0.6, xlab="", main=paste("Dendrogram of", directory), sub="")
  plot(hc, xlab="", main=main, sub="")
#, hang=-0.25)
#  close.screen(all = TRUE)

  # $RCSfile: dr_multiplot.r,v $    $Revision: 1.3 $
}
