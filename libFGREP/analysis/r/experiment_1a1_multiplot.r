## ============================================================================
## Filename          : $RCSfile: experiment_1a1_multiplot.r,v $
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
## Purpose           : This R function calls dr_multiplot with the prefix
##                     and colour vectors corresponding to the percept
##                     trials.
##
## Parameters        : [filename]  the name of the FGREP console output file
##                     [first]  the index of the first term to be plotted
##                     [last]   the index of the last term to be plotted
##
## Revision history  :
## ============================================================================
experiment_1a1_multiplot <- function(filename, first=1, last=0) {
  # Preliminaries --- ensure that the DAAG library has been loaded as that's
  # where pause is kept
  require(DAAG)

  prefix_1 <- c("#",   "action", "part",  "sound",  "colour", "height", "motion", "speed", "texture", "width")
  colour_1 <- c("red", "blue",   "green", "purple", "grey",   "grey",   "grey",   "grey",  "grey",    "grey")
  prefix_2 <- c("#dog", "#cat", 
                "#duck", "#magpie", "#twenty-eight", "#emu",
		"#shark", "#salmon", "#clown-fish",
		"#grass",
		"#everlasting", "#kangaroo-paw",
		"#geraldton-wax", "#bottle-brush", "#grass-tree",
		"#ghost-gum", "#jarrah", "#wattle")
  colour_2 <- c("cyan", "cyan", 
                "blue", "blue", "blue", "lightblue",
		"darkgrey", "darkgrey", "grey",
		"lightgreen",
		"green", "green",
		"darkgreen", "darkgreen", "darkgreen",
		"brown", "brown", "brown")

  dr_multiplot(filename, first, last, prefix_1, colour_1, "white")
  # pause()
  # dr_multiplot(filename, 1, 18, prefix_2, colour_2, "white")

  # $RCSfile: experiment_1a1_multiplot.r,v $    $Revision: 1.2 $
}
