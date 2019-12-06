# read data
fullsample <- read.csv("../data/fullsample.csv", row.names=1)

labels <- colnames(fullsample)

# split into three subscales
R <- fullsample[,grepl("R_", labels)]
P <- fullsample[,grepl("P_", labels)]
B <- fullsample[,grepl("B_", labels)]

cormat <- cor(P, B)

library(corrplot)
corrplot(cormat, method="color", type="lower", diag=T, is.cor=T, mar=c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=.5, tl.col="red", cl.cex=1, cl.ratio=0.05)
