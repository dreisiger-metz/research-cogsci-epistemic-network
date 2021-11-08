lsa_heatmap <- function(directory, power=1.0, cex=0.75, numColours=256) {
  # Preliminaries --- ensure that the lsa and gplots libraries have been loaded
  require(lsa)
  require(gplots)

  T   <- textmatrix(directory, stemming=FALSE, stopwords=NULL)
  T   <- lw_logtf(T) * gw_idf(T)
  LSA <- lsa(T, dims=dimcalc_share())

  disttk <- dist(LSA$tk)
  heatmap(as.matrix(disttk)^power, scale="none", symm=TRUE, revC=TRUE, 
          col=c("red", colorpanel(numColours-1, "white", "grey10")), cexRow=cex, cexCol=cex)
}
