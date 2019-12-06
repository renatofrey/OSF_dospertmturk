library(data.table)

obj <- "m5"

files <- list.files(paste("../objects/stage3/lpa/py_gmm_screen/", obj, "/", sep=""), pattern=".csv", full.names=T)

lls <- NULL
bics <- NULL
for (file in files) {
  print(file)
  infocrit <- read.csv(file)[1:30,]
  lls <- rbind(lls, infocrit$lls)
  bics <- rbind(bics, infocrit$bics)
}


lls_p <- apply(lls, 2, mean)
lls_sds <- apply(lls, 2, sd)

bics_p <- apply(bics, 2, mean)
bics_sds <- apply(bics, 2, sd)

best_ll <- which.max(lls_p)
best_bic <- which.min(bics_p)

bics1 <- bics[,best_bic]
for (i in best_bic:2) {
  bics2 <- bics[,(i-1)]
  t <- t.test(bics1, bics2)
  if (t$p.value < .01) break
}
ok_bic <- i

pdf(paste("../output/stage3/lpa/", obj, "_infocrit.pdf", sep=""), height=4, width=8)
if (T) {
  par(mfrow=c(1,2), mar=c(4,5,2,2), mgp=c(3,0.75,0))
  
  plot(lls_p, type="l", xlab="Number of clusters", ylab="Likelihood", las=1, pch=16, cex=.5)
  for (i in 1:ncol(lls)) {
    lines(x=c(i,i), y=c(lls_p[i]-lls_sds[i], lls_p[i]+lls_sds[i]), lwd=2, lend=2)
  }
  points(best_bic, lls_p[best_bic], pch=16, col="blue", cex=1.25)
}

plot(bics_p, type="l", xlab="Number of clusters", ylab="BIC", las=1, pch=16, cex=.5)
for (i in 1:ncol(bics)) {
  lines(x=c(i,i), y=c(bics_p[i]-bics_sds[i], bics_p[i]+bics_sds[i]), lwd=3, lend=0)
}
points(best_bic, bics_p[best_bic], pch=16, col="blue", cex=1.25)
#points(ok_bic, bics_p[ok_bic], pch=16, col="orange", cex=1)
dev.off()
