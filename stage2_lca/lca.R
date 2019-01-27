# find classes (or load from pre-computed files)

# read data
subsampleA <- read.csv("../data/subsampleA_blinded.csv", row.names=1)

# split into three subscales
R <- subsampleA[,paste("R", 1:30, sep="")]
P <- subsampleA[,paste("P", 1:30, sep="")]
B <- subsampleA[,paste("B", 1:30, sep="")]

# make ordinal
Rord <- R
Pord <- P
Bord <- B
for (i in 1:30) {
  Rord[,i] <- as.ordered(Rord[,i])
  Pord[,i] <- as.ordered(Pord[,i])
  Bord[,i] <- as.ordered(Bord[,i])
}


bics_all <- NULL
pred_class <- data.frame(id=subsampleA$unique_ID)
probabilities <- list()

for (sel in c("R", "P", "B")) {
  
  # create model formula
  f <- as.formula(paste("cbind(", paste(colnames(get(sel)), collapse=", "), ") ~ 1", sep=""))
  
  # loop through different solutions
  library(poLCA)
  
  # choose whether models should be run or loaded
  if (F) {
    clusters <- list()
    for (i in 1:10) {
      print(i)
      clusters[i] <- list(poLCA(formula=f, data=get(paste(sel, "ord", sep="")), nclass=i, nrep=50))
    }
    save(clusters, file=paste("../objects/stage2/lca_clusters_", sel, ".Rdata", sep=""))
  } else load(paste("../objects/stage2/lca_clusters_", sel, ".Rdata", sep=""))
  
  # get BICs for all solutions and get best solution
  bics <- rbind(lapply(clusters, function(x) {x$bic}))
  rownames(bics) <- sel
  bics_all <- rbind(bics_all, bics)
  best <- which.min(bics)
  cl <- clusters[[best]]
  ## BIC: log(cl$Nobs)*cl$npar - 2*(cl$llik)
  ## => cl$bic should get small!
  
  # plot cluster profiles
  pdf(file=paste("../output/stage2/lca_profiles_", sel, ".pdf", sep=""), width=14, height=10)
  plot(cl)
  dev.off()
  
  # predicted cluster membership
  pred_class[,sel] <- cl$predclass
  
  # rating probabilites per class
  p <- cl$probs
  probabilities[sel] <- list(p)
  
  EVs <- lapply(p, function(x) {rowSums(t(apply(x, 1, function(y) {y * as.numeric(colnames(p[[1]]))})))})
  EVs <- as.data.frame(EVs)
  
  EVs <- EVs[,order(apply(EVs, 2, sd))]
  #EVs <- EVs[,paste("R", 1:30, sep="")]
  
  pdf(file=paste("../output/stage2/lca_EVs_", sel, ".pdf", sep=""), height=5)
  matplot(t(EVs), type="l", las=1, ylim=c(1,7), ylab="Expected value (rating)", xaxt="n", col=1:nrow(EVs), lty=1:nrow(EVs), lwd=2)
  axis(1, colnames(EVs), at=1:ncol(EVs), las=3)
  
  legend("topright", horiz=F, lwd=2, lty=1:nrow(EVs), col=1:nrow(EVs), paste("Class", 1:nrow(EVs)), box.lty=0, bg=NA)
  dev.off()
  
}

pdf("../output/stage2/lca_BICs.pdf")
matplot(t(bics_all), type="b", pch=rownames(bics_all), las=1, xlab="Number of classes", ylab="BIC", xaxt="n")
axis(1, at=1:ncol(bics_all), mgp=c(5,1,0))
for (i in 1:nrow(bics_all)) {
  tmp <- unlist(bics_all[i,])
  points(which.min(tmp), tmp[which.min(tmp)], pch=2, col=i, cex=5)
}
dev.off()

save(bics_all, probabilities, EVs, pred_class,
     file="../objects/stage2/lca.Rdata")