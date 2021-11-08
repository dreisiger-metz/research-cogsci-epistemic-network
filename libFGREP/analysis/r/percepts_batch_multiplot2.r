percepts_batch_multiplot2 <- function(drs, trials, epochs=100000) {
  require(DAAG)  # for pause()

  filter=c("colour", "height", "motion", "speed", "texture", "width")
  colour=c("red",    "blue",   "green",  "grey",  "purple",  "lightblue")

  for (trial in trials) {
    dr_multiplot2(paste(drs, "--", trial, "/iteration--100000.term", sep=""), filter=filter, colour=colour)
    pause()
  }
}
