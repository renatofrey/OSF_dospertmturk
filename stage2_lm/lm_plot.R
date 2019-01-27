# compare and plot LM results

load("../objects/stage2/lm.Rdata")

summary(m)
round(summary(m)$coef, 1)
c <- coef(m)[[1]]
colnames(c) <- paste("lme4_", colnames(c), sep="")
# c <- cbind(c, rowSums(R), rowSums(P), rowSums(B))
cor(c)

c_rstan <- coef(mod_rstan)$unique_ID
colnames(c_rstan) <- c("Intercept", "b(P)", "b(B)")
summary(c_rstan)

comb <- cbind(c, c_rstan)
plot(comb, cex=.2)
cor(comb)

library(psych)
pdf("../output/stage2/riskreturn_coefs.pdf", width=6, height=6)
pairs.panels(c_rstan, hist.col="green", lm=T, ci=T, las=1, density=F, cex=.2, cex.cor=.5, breaks=50, rug=F)
dev.off()