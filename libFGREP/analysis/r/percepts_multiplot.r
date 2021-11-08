## ============================================================================
## Filename          : $RCSfile: percepts_multiplot.r,v $
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
percepts_multiplot <- function(filename, first=1, last=0) {
  prefix <- c("colour", "height", "motion", "speed", "texture", "width")
  colour <- c("red",    "blue",   "green",  "grey",  "purple",  "lightblue")

  dr_multiplot(filename, first, last, prefix, colour, "white")

  # $RCSfile: percepts_multiplot.r,v $    $Revision: 1.2 $
}
