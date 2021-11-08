## ============================================================================
## Filename          : $RCSfile: batch_read_terms.r,v $
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
## Purpose           : This R function iterates over all of the FGREP console
##                     output files associated with a given DR size, and returns
##                     a list that contains the term and epoch labels, the rep-
##                     resentations themselves, and the errors-squared.
##
## Parameters        : [drsize]  the DR size whose results are to be loaded
##                     [first]  the index of the first term to be plotted
##                     [last]   the index of the last term to be plotted
##                     [numberOfTrials]   the number of trials associated with
##                         the DR size
##                     [numberOfSteps]   the number of samplings (or output
##                         files per trial
##                     [epochsPerStep]   the number of epochs between samplings
## Returns           : a list consisting of $drsize, $terms, $epochs, $repre-
##                     sentations and $errors
##
## Revision history  :
## ============================================================================
batch_read_terms <- function(drsize, first=1, last=0, numberOfTrials=25, epochs=500*0:200) {
  datum <- read.table(paste(drsize, "--0/iteration--0.term", sep=""), header=FALSE, comment.char="")
  datumSize <- dim(datum)
  if (last == 0) last <- datumSize[1]

  terms  <- datum[first:last, 1]
  representations           <- array(dim=c(numberOfTrials, length(epochs), last-first+1, datumSize[2]-2))
  representations[1, 1, , ] <- as.matrix(datum[first:last, 2:(datumSize[2]-1)])
  errors         <- array(dim=c(numberOfTrials, length(epochs), last-first+1))
  errors[1, 1, ] <- as.matrix(datum[first:last, datumSize[2]])

  # Load the remaining epochs' data and place them in [results]
  for (trial in 1:numberOfTrials) {
    write(paste("About to process trial", trial, "for DR size", drsize), file="")
    for (step in 1:length(epochs)) {
      filename <- paste(drsize, "--", trial - 1, "/iteration--", as.integer(epochs[step]), ".term", sep="")
      datum <- read.table(filename, header=FALSE, comment.char="")
      representations[trial, step, , ] <- as.matrix(datum[first:last, 2:(datumSize[2]-1)])
      errors[trial, step, ] <- as.matrix(datum[first:last, datumSize[2]])
    }
  }

  return(list(drsize=drsize, terms=terms, epochs=epochs, representations=representations, errors=errors))

  # $RCSfile: batch_read_terms.r,v $    $Revision: 1.2 $
}
