dr_trajectory <- function(path=".", epochs=c(0, 100, 500, 1000, 5000, 10000, 50000, 100000), terms=1:8, cex=0.6) {
  numEpochs <- length(epochs)
  rawdata <- vector(mode="list", length=numEpochs)
  for (e in 1:numEpochs)
    rawdata[[e]] <- read.table(paste(path, "/iteration--", as.integer(epochs[e]), ".term", sep=""), header=FALSE, comment.char="")
  label <- as.character(rawdata[[1]][,1])


  drcolumns <- 2:(dim(rawdata[[1]])[2]-1)
  scaled <- array(dim=c(numEpochs, length(terms), 2))
  for (e in 1:numEpochs) {
    scaled[e, , ] <- cmdscale(dist(rawdata[[e]][terms, drcolumns], method="euclidean"))
    for (i in 1:length(terms))
      for (j in 1:2)
        scaled[e, i, j] <- (scaled[e, i, j] - min(scaled[e, ,])) / (max(scaled[e, , ]) - min(scaled[e, , ]))
  }


  plot(scaled[, 1, 1], scaled[, 1, 2], type="o", pch=".", cex=3, xlim=c(0, 1.0), ylim=c(0, 1.0))
  points(scaled[numEpochs, 1, 1], scaled[numEpochs, 1, 2], type="p", pch=21)
  text(scaled[numEpochs, 1, 1], scaled[numEpochs, 1, 2], label[1], cex=cex, pos=1, offset=0.01)
  for (t in 2:length(terms)) {
    points(scaled[, t, 1], scaled[, t, 2], type="o", pch=".", cex=3)
    points(scaled[numEpochs, t, 1], scaled[numEpochs, t, 2], type="p", pch=21)
    text(scaled[numEpochs, t, 1], scaled[numEpochs, t, 2], label[t], cex=cex, pos=1, offset=0.01)
  }
}
