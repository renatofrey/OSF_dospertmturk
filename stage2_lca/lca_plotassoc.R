# Determine & plot associations of classes across subscales

load("../output/stage2/lca_output.Rdata")

cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  #print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

pdf(file="../output/stage2/lca_associations.pdf")
par(mfrow=c(3,1), mar=c(6,4,4,4))

# loop through all three possible combinations
for (p in 1:3) {

  if (p == 1) {v1 <- "R"; v2 <- "P"}
  if (p == 2) {v1 <- "R"; v2 <- "B"}
  if (p == 3) {v1 <- "P"; v2 <- "B"}
  
  # get Cramer's phi
  V <- cv.test(pred_class[,v1], pred_class[,v2])
  
  # simulate distribution to be expected from chance
  if (T) {
    Vs <- NULL
    for (i in 1:10000) {
      if (i %% 100 == 0) print(paste(v1, v2, i))
      Vs <- c(Vs, cv.test(pred_class[,v1], sample(pred_class[,v2])))
    }  
  }
  
  hist(Vs, breaks=20, col="red", border="white", las=1, xlim=c(0, 1), main=paste("Association of classes between", v1, "and", v2), xlab="", ylab="", cex.main=2, cex.axis=1.5, yaxt="n")
  abline(v=V, col="blue", lwd=3)
  mtext("Cramer's V", side=1, line=3, cex=1.1)
}
dev.off()

