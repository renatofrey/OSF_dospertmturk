# run exploratory factor analyses

# read data
fullsample <- read.csv("../data/fullsample.csv", row.names=1)

labels <- colnames(fullsample)
labels[grepl("R_", labels, fixed="T")] <- paste("R", 1:30, sep="")
labels[grepl("P_", labels, fixed="T")] <- paste("P", 1:30, sep="")
labels[grepl("B_", labels, fixed="T")] <- paste("B", 1:30, sep="")
colnames(fullsample) <- labels

# split into three subscales
R <- fullsample[,paste("R", 1:30, sep="")]
P <- fullsample[,paste("P", 1:30, sep="")]
B <- fullsample[,paste("B", 1:30, sep="")]

# make ordinal
Rord <- R
Pord <- P
Bord <- B
for (i in 1:30) {
  Rord[,i] <- as.ordered(Rord[,i])
  Pord[,i] <- as.ordered(Pord[,i])
  Bord[,i] <- as.ordered(Bord[,i])
}

# create and compare correlation matrices for R-data
cR <- cor(R)
if (F) {
  # library(polycor)
  # cRord <- hetcor(Rord)$correlations
  # cRdiffs <- cRord[lower.tri(cRord)] - cR[lower.tri(cR)]
  # round(summary(cRdiffs), 2)
  # #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # # -0.09    0.01    0.04    0.04    0.08    0.17 
  # difference small on average, continue with Pearson's correlations
}

### select subscale
sel <- "R"

d <- get(sel)
cormat <- get(paste("c", sel, sep=""))


# create network plot (using m3 for grouping)


load("../objects/stage3/cfas_full.Rdata")



factors <- apply(m3$loadings, 1, function(x) {which.max(x)})

grps <- sapply(1:nfact, function(x) {as.numeric(which(x == factors))})
if (is.null(dim(grps))) names(grps) <- paste("F", 1:nfact, sep="") else {
  grps <- data.frame(grps)
  colnames(grps) <- paste("F", 1:nfact, sep="")
}

pdf(paste("../output/stage3/network_", sel, "m3.pdf", sep=""), width=12, height=9)
source("p_qgraph.R")
dev.off()