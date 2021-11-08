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
mds_latent_clusters <- function(results, categories=c("."), colours=NULL, defaultcolour="black", cex=0.6) {
  #distance <- as.matrix(results$separation.normalised)
  #heatmap(separation0806, scale="non", col=c("red", colorpanel(255, "white", "grey10")),symm=TRUE, revC=TRUE)

  scaled <- cmdscale(results$separation.normalised)
  colour <- array(defaultcolour, dim=length(results$categories))

  # Next, we go through the label, prefix and colour vectors and work out each
  # term's line colour.
  hits <- grep(paste("^", categories[1], sep=""), results$categories)
  if (length(colours) > 0) colour[hits] <- colours[1]
  if (length(categories) > 1) {
    for (i in 2:length(categories)) {
      hits <- grep(paste("^", categories[i], sep=""), results$categories)
      if (length(colours) > 0) colour[hits] <- colours[i]
    }
  }


  # And finally, we generate the MDS plot using the specified colours, or the
  # default if none were given.
  plot(scaled, type="p", pch=".", cex=3, xlab=" ", ylab=" ", col=colour, main=paste("MDS Plot of", results$file),
       cex.main = 0.75,
       panel.first=symbols(scaled[,1], scaled[,2], circles=results$radius.mean, fg=colour))
  text(scaled[,1], scaled[,2], results$categories, cex=cex, pos=1, offset=(max(scaled[,2]) - min(scaled[,2])) / 10, col=colour)

  # $RCSfile: dr_mds.r,v $    $Revision: 1.6 $
}
