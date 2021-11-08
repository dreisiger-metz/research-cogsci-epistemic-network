percepts_analyse_trials <- function(drSize) {
  results <- vector(mode="list", 25)
  
  for (i in 0:24)
    results[[i+1]] <- analyse_trial(paste(drSize, '--', i, '/iteration--100000.term', sep=''),  c('colour', 'height', 'motion', 'speed', 'texture', 'width'))

  summary <- array(dim=c(length(results), 5))
  colnames(summary) <- c('trial number', 'cluster errors', 'separation', 'radius', 'avg error')

  good <- 0
  for (i in 1:length(results)) {
    summary[i, 1] <- i-1
    summary[i, 2] <- (results[[i]]$numberOfClusters - 6)^2
    if (summary[i, 2] < 2)
      good = good + 1
    summary[i, 3] <- results[[i]]$clusterSeparation.mean.scaled
    summary[i, 4] <- results[[i]]$clusterRadius.mean.scaled
    summary[i, 5] <- results[[i]]$clusterError.mean
  }

  summary[, 3] <- summary[, 3] / max(summary[, 3])
  summary[, 4] <- summary[, 4] / max(summary[, 4])
  summary[, 5] <- summary[, 5] / max(summary[, 5])

  recommendation <- array(dim=c(good, 5))
  colnames(recommendation) <- c('trial number', 'cluster errors', 'separation', 'radius', 'avg error')

  j <- 1
  for (i in 1:length(results))
    if (summary[i, 2] < 2) {
      recommendation[j, ] <- as.matrix(summary[i, ])
      j = j + 1
    }
  
  return(recommendation)
}
