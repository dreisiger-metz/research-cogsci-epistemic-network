## ============================================================================
## Filename          : $RCSfile: batch_dr_parcoord.r,v $
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
## Purpose           : This R function iterates through a set of subdirectories
##                     and calls dr_parcoord(...) for each of the 'iteration--
##                     100000.term' files.
##
## Parameters        : [drsize]  the size of the distributed representations
##                         to plot; the subdirectories' prefix
##                     [first]  the first of the trials to be plotted
##                     [last]   the last of the trials to be plotted
##
## Revision history  :
## ============================================================================
batch_dr_parcoord <- function(drsize, first=0, last=24) {
  # Preliminaries --- ensure that the DAAG library has been loaded as that's
  # where pause is kept
  require(DAAG)

  filenames <- paste(as.character(drsize), "--", first:last, "/iteration--100000.term", sep = "")

  for (file in filenames) {
    dr_parcoord(file);
    pause();
  }

  # $RCSfile: batch_dr_parcoord.r,v $    $Revision: 1.2 $
}
