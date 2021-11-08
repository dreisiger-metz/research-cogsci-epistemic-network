## ============================================================================
## Filename          : $RCSfile: percepts_plot_statistics.r,v $
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
## Purpose           : This function accepts the output of batch_analyse_terms,
##                     calculates the overall and category average errors-
##                     squared and delta-Rs, and plots them 2-up.
##
## Parameters        : [stats]  an object containing the terms' statistics
##
## Revision history  :
## ============================================================================
percepts_plot_statistics <- function(stats) {
  #  return(list(terms, epochs, errors.mean=errors.mean, errors.sd=errors.sd, deltaRs.mean=deltaRs.mean, deltaRs.sd=deltaRs.sd))
  prefix <- c("colour", "height", "motion", "speed", "texture", "width")
  colour <- c("red",    "blue",   "green",  "grey",  "purple",  "lightblue")
  exclusion <- c("is-approx")


  termError.mean <- array(dim=c(dim(stats$errors.mean)[2]))
  for (step in 1:dim(stats$errors.mean)[2])
    termError.mean[step] <- mean(stats$errors.mean[,step])

  termDeltaR.mean <- array(dim=c(dim(stats$deltaRs.mean)[2]))
  for (step in 1:dim(stats$deltaRs.mean)[2])
    termDeltaR.mean[step] <- mean(stats$deltaRs.mean[,step])

  categoryError.mean <- array(dim=c(length(prefix), dim(stats$errors.mean)[2]))
  for (category in 1:length(prefix)) {
    hits <- grep(paste("^", prefix[category], sep=""), stats$terms)
    for (step in 1:dim(stats$errors.mean)[2])
      categoryError.mean[category, step] <- mean(stats$errors.mean[hits, step])
  }

  categoryDeltaR.mean <- array(dim=c(length(prefix), dim(stats$deltaRs.mean)[2]))
  for (category in 1:length(prefix)) {
    hits <- grep(paste("^", prefix[category], sep=""), stats$terms)
    for (step in 1:dim(stats$deltaRs.mean)[2])
      categoryDeltaR.mean[category, step] <- mean(stats$deltaRs.mean[hits, step])
  }


  op <- options()
  split.screen(c(2,1))
  screen(1)
  options(scipen=6)
  #axis(1, at=c(500, 1000, 5000, 10000, 50000, 100000))
  options(op)
  #axis(2, at=c(1e-7, 1e-5, 1e-3, 0.1))
  plot(x=stats$epochs, y=stats$errors.mean[1, ], type="l", log="xy", col="grey", xlim=c(500,100000), ylim=c(1e-7, 0.1), main=paste("Average errors-squared for a DR size of", stats$drsize), xlab="epochs", ylab="average errors-squared")
  for (term in 2:dim(stats$errors.mean)[1])
    if (length(grep(stats$terms[term], exclusion)) == 0)
      points(x=stats$epochs, y=stats$errors.mean[term,], type="l", col="grey")
  for (category in 1:dim(categoryError.mean)[1])
    points(x=stats$epochs, y=categoryError.mean[category, ], type="l", col=colour[category])
  points(x=stats$epochs, y=termError.mean, type="l", col="black", lwd=2)
  legend("bottomleft", c(prefix, ""), lty=1, col=c(colour, "white"), bty='n', cex=0.6)
  abline(v=c(seq(1000,10000,1000), seq(20000,100000,10000)), lty=3, col="lightgrey")
  abline(h=c(0.1, 5e-2, 1e-2, 5e-3, 1e-3, 5e-4, 1e-4, 5e-5, 1e-5, 5e-6, 1e-6, 5e-6), lty=3, col="lightgrey")

  screen(2)

  plot(x=stats$epochs[2:length(stats$epochs)], y=stats$deltaRs.mean[1, ], type="l", log="xy", col="grey", xlim=c(500, 100000), ylim=c(1e-4, 1), main=paste("Average delta-Rs for a DR of size", stats$drsize), xlab="epochs", ylab="average delta-R")
  for (term in 2:dim(stats$deltaRs.mean)[1])
    if (length(grep(stats$terms[term], exclusion)) == 0)
      points(x=stats$epochs[2:length(stats$epochs)], y=stats$deltaRs.mean[term, ], type="l", col="grey")
  for (category in 1:dim(categoryDeltaR.mean)[1])
    points(x=stats$epochs[2:length(stats$epochs)], y=categoryDeltaR.mean[category, ], type="l", col=colour[category])
  points(x=stats$epochs[2:length(stats$epochs)], y=termDeltaR.mean, type="l", col="black", lwd=2)
  legend("bottomleft", c(prefix, ""), lty=1, col=c(colour, "white"), bty='n', cex=0.6)
  abline(v=c(seq(1000,10000,1000), seq(20000,100000,10000)), lty=3, col="lightgrey")
  abline(h=c(1.0, 0.5, 0.1, 5e-2, 1e-2, 5e-3, 1e-3, 5e-4, 1e-4), lty=3, col="lightgrey")

  close.screen(all = TRUE)
  options(op)

  # $RCSfile: percepts_plot_statistics.r,v $    $Revision: 1.2 $
}
