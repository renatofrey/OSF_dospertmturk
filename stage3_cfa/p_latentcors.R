fullsample <- read.csv("../data/fullsample.csv", row.names=1)

library(lavaan)
library(psych)

sel_ms <- c(1,2,3,4,5,6)
#sel_ms <- c(2,4)

load("../objects/stage3/cfas_full.Rdata")

preds <- NULL
for (i in sel_ms) {
  pred <- predict(get(paste("m", i, "_fit", sep="")))
  colnames(pred) <- paste("M", i, tolower(colnames(pred)), sep="")
  
  preds <- as.data.frame(cbind(preds, pred))
  
  write.csv(pred, file=paste("../objects/stage3/cfa/m", i, "_pred.csv", sep=""))
  if (ncol(pred) > 1) {
    pdf(file=paste("../output/stage3/cfa_full/cors", i, ".pdf", sep=""))
    pairs.panels(pred, lm=T, ci=T, las=1, density=F, cex=.2, cex.cor=.5, breaks=50, rug=F)
    dev.off()
  }
  
}
ind_m5 <- grepl("M5", colnames(preds))

ms <- substr(colnames(preds), 2, 2)
grps <- sapply(sel_ms, function(x) {which(ms == x)})
names(grps) <- paste("Model", sel_ms)

# Add risk-return indices
load("../objects/stage3/lm.Rdata")
c_m1 <- coef(m1)[[1]]
c_m2 <- coef(m2)[[1]]
c_rstan <- coef(m2_rstan)[[1]]
preds$I <- c_m1$`(Intercept)`
#preds$I <- c_m2$`(Intercept)`


# Add SOEP items
soep_labels <- colnames(fullsample)[grepl("SOEP", colnames(fullsample))]
preds <- cbind(preds, fullsample[,soep_labels])

cormat <- cor(preds, use="pairwise.complete.obs")
grps$`Risk-return framework` <- which(colnames(cormat) == "I")
grps$`SOEP risk items` <- which(is.element(colnames(cormat), soep_labels))

ind <- c(which(grepl("M2", colnames(cormat))),
         which(grepl("M5", colnames(cormat))),
         which(grepl("I", colnames(cormat))),
         which(is.element(colnames(cormat), soep_labels)))
cormat2 <- cormat[ind,ind]


# use short labels for network plot?
colnames(cormat) <- gsub("M1", "", colnames(cormat))
colnames(cormat) <- gsub("M2", "", colnames(cormat))
colnames(cormat) <- gsub("M3", "", colnames(cormat))
colnames(cormat) <- gsub("M4", "", colnames(cormat))
colnames(cormat) <- gsub("M5", "", colnames(cormat))
colnames(cormat) <- gsub("M6", "", colnames(cormat))
colnames(cormat) <- gsub("SOEP", "", colnames(cormat))
colnames(cormat) <- toupper(colnames(cormat))


pdf(paste("../output/stage3/latent_network.pdf", sep=""), width=10, height=7)

library(viridis)
source("p_qgraph.R")
dev.off()





# plot the factor scores for M5
pdf("../output/stage3/latent_predM5.pdf", height=3)
par(mfrow=c(2,4), mgp=c(2.5,1,0), mar=c(4,2,3,2))
dat <- preds[,ind_m5]
colnames(dat) <- gsub("M5", "", colnames(dat))
colnames(dat) <- toupper(colnames(dat))

p_cols <- c("red", plasma(6, begin=.10, end=.95))

for (i in 1:ncol(dat)) {
  
  c_ind <- match(colnames(dat)[i], c("R", "SOC", "REC", "GAM", "INV", "ETH", "HEA"))
  hist(dat[,i], las=1, border="white", col=p_cols[c_ind], breaks=25, main=colnames(dat)[i], xlim=c(-4,4), yaxt="n", xlab="Factor score", ylab="")
}
dev.off()


ind_R <- which(colnames(cormat) == "R")
cormat[ind_R, ind_R]

colnames(cormat2) <- gsub("M5r\\b", "M5_R", colnames(cormat2))
rownames(cormat2) <- gsub("M5r\\b", "M5_R", rownames(cormat2))
colnames(cormat2) <- gsub("I", "RRF_I", colnames(cormat2))
rownames(cormat2) <- gsub("I", "RRF_I", rownames(cormat2))

library(corrplot)
pdf(file="../output/stage3/latent_cormat.pdf")
cs <- colorRampPalette(c("darkorange", "white", "deepskyblue4"))(20)
corrplot(cormat2, method="color", type="lower", diag=T, is.cor=T, addCoef.col = "black", number.font=1, number.cex=.7, tl.cex=.8, tl.col="darkgrey", cl.cex=.8, cl.ratio=0.1, mar=c(0,0,1,2), col=cs)
dev.off()
