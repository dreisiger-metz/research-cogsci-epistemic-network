rawdata <- vector(mode="list", length=9)
rawdata[[1]] <- read.table('iteration--0.term',      header=FALSE, comment.char="")
rawdata[[2]] <- read.table('iteration--100.term',    header=FALSE, comment.char="")
rawdata[[3]] <- read.table('iteration--200.term',    header=FALSE, comment.char="")
rawdata[[4]] <- read.table('iteration--500.term',    header=FALSE, comment.char="")
rawdata[[5]] <- read.table('iteration--1000.term',   header=FALSE, comment.char="")
rawdata[[6]] <- read.table('iteration--5000.term',   header=FALSE, comment.char="")
rawdata[[7]] <- read.table('iteration--10000.term',  header=FALSE, comment.char="")
rawdata[[8]] <- read.table('iteration--50000.term',  header=FALSE, comment.char="")
rawdata[[9]] <- read.table('iteration--100000.term', header=FALSE, comment.char="")
label <- as.character(rawdata[[1]][, 1])
colour <- c("red", "orange", "yellow", "green", "cyan", "blue", "purple", "brown", "black")

index <- 2:(dim(rawdata[[1]])[2]-1)
data <- array(dim=c(8*length(rawdata), length(index)))
for (term in 1:8)
  for (trial in 1:length(rawdata))
    data[(term-1)*length(rawdata) + trial, ] <- as.matrix(rawdata[[trial]][term, index])

distances <- dist(data, method="euclidean")
scaled <- cmdscale(distances)

# Plot the trajectories
plot(scaled[1:length(rawdata), 1], scaled[1:length(rawdata), 2], type="o", pch=".", cex=3, xlim=range(scaled[, 1]), ylim=range(scaled[, 2]), col=colour[1])
points(scaled[length(rawdata), 1], scaled[length(rawdata), 2], type="p", pch=21, col=colour[1])

for (term in 2:8) {
  points(scaled[(1+(term-1)*length(rawdata)):(length(rawdata)*term), 1], scaled[(1+(term-1)*length(rawdata)):(length(rawdata)*term), 2], type="o", pch=".", cex=3, col=colour[term])
  points(scaled[length(rawdata)*term, 1], scaled[length(rawdata)*term, 2], type="p", pch=21, col=colour[term])
}
