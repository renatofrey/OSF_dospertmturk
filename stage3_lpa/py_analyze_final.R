sel_m <- "m5"
sel_i <- 7

# get estimated factor values
Fs <- read.csv(paste("../objects/stage3/cfa/", sel_m, "_pred.csv", sep=""), row.names=1)

# get cluster means (from GMMs)
c_means <- read.csv(paste("../objects/stage3/lpa/py_gmm_final/full/", sel_m, "_", sel_i, "_c_means.csv", sep=""))
names(c_means) <- names(Fs)

c_means_A <- read.csv(paste("../objects/stage3/lpa/py_gmm_final/A/", sel_m, "_", sel_i, "_c_means.csv", sep=""))
names(c_means_A) <- names(Fs)

c_means_B <- read.csv(paste("../objects/stage3/lpa/py_gmm_final/B/", sel_m, "_", sel_i, "_c_means.csv", sep=""))
names(c_means_B) <- names(Fs)

# get some fit indices
c_info <- read.csv(paste("../objects/stage3/lpa/py_gmm_final/full/", sel_m, "_", sel_i, "_c_info.csv", sep=""))

# only retain clusters that exceed certain thresholds
enrich_ratio <- 1.25
c_info$enrich >= enrich_ratio
results <- cbind(c_info$pvals <= 0.01, c_info$enrich >= enrich_ratio)
results <- cbind(results, (results[,1] == T) & (results[,2] == T))
c_ok <- which(results[,3] == T)
c_del <- which(results[,3] != T)

# get participants' classifications (from GMMs)
part_class <- read.csv(paste("../objects/stage3/lpa/py_gmm_final/full/", sel_m, "_", sel_i, "_part_class.csv", sep=""), header=F) + 1
names(part_class) <- "cluster"
table(part_class)
#plot(as.numeric(table(part_class)), c_info$weights, las=1)

# get classification probabilities
part_p <- read.csv(paste("../objects/stage3/lpa/py_gmm_final/full/", sel_m, "_", sel_i, "_part_p.csv", sep=""), header=T)
part_unc <- part_p[cbind(1:nrow(part_p),as.numeric(unlist(part_class)))]


# plot enrichment analysis
pdf(file=paste("../output/stage3/lpa/", sel_m, "_enrichment.pdf", sep=""), height=5)

p_enrich <- c_info$enrich

p_pvals <- c_info$pvals
p_pvals[which(c_info$pvals == 0)] <- 0.000000001
p_pvals <- log10(p_pvals)

plot(p_enrich, p_pvals, las=1, pch=16, col="cyan", cex=2.5,
     xlab="Enrichment", ylab="log10(p-value)", xlim=c(0, 12), ylim=c(-10, 0))
rect(enrich_ratio, -20, 20, log10(0.01), density=5, border=0, col="lightgrey")
text(p_enrich, p_pvals, paste("C", 1:nrow(results), sep=""), cex=.6)
abline(v=1, lty=3, col="darkgrey")
abline(v=enrich_ratio, lty=3, col="darkgrey")
abline(h=log10(0.01), lty=3, col="darkgrey")

text(x=1, y=-5, "Enrichement = 1", srt=90, cex=.8, font=2)
text(x=enrich_ratio, y=-5, paste("Enrichement =", enrich_ratio), srt=90, cex=.8, font=2)
text(x=6, y=log10(0.01), "p-value = 0.01", cex=.8, font=2)
dev.off()


# prepare data for plotting the identified profiles
pdat <- t(c_means[c_ok,])
colnames(pdat) <- paste("C", c_ok, sep="")
pdat <- pdat[,order(colSums(pdat))]

pdat_A <- t(c_means_A[c(2,5,4,7),])
colnames(pdat_A) <- paste("C", c_ok, sep="")
pdat_A <- pdat_A[,order(colSums(pdat_A))]

pdat_B <- t(c_means_B[c(2,1,7,6),])
colnames(pdat_B) <- paste("C", c_ok, sep="")
pdat_B <- pdat_B[,order(colSums(pdat_B))]



pdat_pos <- pdat
pdat_pos[which(pdat_pos < 0)] <- 0
pdat_neg <- pdat
pdat_neg[which(pdat_neg > 0)] <- 0

prof_lab <- paste("Profile ", c("I","II","III","IV"), " (", colnames(pdat), ")", sep="")

fullsample <- read.csv("../data/fullsample.csv", row.names=1)

fullsample$class <- as.factor(part_class[,1])
levels(fullsample$class)[is.element(levels(fullsample$class), c_del)] <- NA

print(table(fullsample$class))


# add original dospert domain means (for separate plot)
domains <- colnames(fullsample)[grepl("R_1", colnames(fullsample))]
domains <- substr(domains, 1, 4)
fullsample <- cbind(fullsample,
  t(apply(fullsample, 1, function(x) {
    sapply(domains, function(d) {
      as.numeric(mean(as.numeric(x[grepl(d, names(x))])))
    })
  }))
)
dospert_cmeans <- NULL
for (d in domains) {
  tmp <- rbind(tapply(fullsample[,d], list(fullsample$class), mean))
  rownames(tmp) <- d
  dospert_cmeans <- rbind(dospert_cmeans, tmp)
}
dospert_cmeans <- dospert_cmeans[,c("1", "3", "5", "2")]
dospert_cmeans <- dospert_cmeans[c("socR", "recR", "finR", "ethR", "heaR"),]


# Choose whether to rescale Fs to DOSPERT scale using raw domain means
rescale <- F
if (rescale == T) {
  pdat["M5r",] <- pdat["M5r",] + mean(fullsample$heaR) - mean(Fs$M5r)
  pdat["M5soc",] <- pdat["M5soc",] + mean(fullsample$socR) - mean(Fs$M5soc)
  pdat["M5rec",] <- pdat["M5rec",] + mean(fullsample$recR) - mean(Fs$M5rec)
  pdat["M5gam",] <- pdat["M5gam",] + mean(fullsample$finR) - mean(Fs$M5gam)
  pdat["M5inv",] <- pdat["M5inv",] + mean(fullsample$finR) - mean(Fs$M5inv)
  pdat["M5eth",] <- pdat["M5eth",] + mean(fullsample$ethR) - mean(Fs$M5eth)
  pdat["M5hea",] <- pdat["M5hea",] + mean(fullsample$heaR) - mean(Fs$M5hea)
}


# get some aggregate indicator variables (for different profiles)
mAge <- rbind(round(tapply(fullsample$Age, fullsample$class, mean), 1))
mAge[] <- paste(mAge, "y", sep="")
colnames(mAge) <- paste("C", colnames(mAge), sep="")

pFem <- tapply(fullsample$Gender, fullsample$class, table)
pFem <- data.frame(matrix(unlist(pFem), nrow=length(pFem), byrow=T))
pFem <- paste(round(apply(pFem, 1, prop.table)[1,] * 100, 0), "%", sep="")
names(pFem) <- colnames(mAge)

class_ok <- which(!is.na(fullsample$class))
class_nok <- which(is.na(fullsample$class))
fullsample <- subset(fullsample, !is.na(class))
print(prop.table(c(length(class_ok), length(class_nok))))


# get MEANs and SDs of factor scores for separate clusters (and compare with model-inferred means)
emp_means <- t(aggregate(Fs[class_ok,], by=list(fullsample$class), mean))
emp_sds <- t(aggregate(Fs[class_ok,], by=list(fullsample$class), sd))
colnames(emp_means) <- paste("C", emp_means[1,], sep="")
colnames(emp_sds) <- paste("C", emp_sds[1,], sep="")
emp_means <- emp_means[-1,]
emp_sds <- emp_sds[-1,]
emp_means <- emp_means[,colnames(pdat)]
emp_sds <- emp_sds[,colnames(pdat)]
emp_means <- apply(emp_means, c(1,2), as.numeric)
emp_sds <- apply(emp_sds, c(1,2), as.numeric)
emp_sem <- emp_sds / sqrt(nrow(fullsample))
pdat
emp_means


# set largest class as reference
fullsample$class <- relevel(fullsample$class, ref = order(table(fullsample$class), decreasing=T)[1])

# Predictors
fullsample$Age <- scale(fullsample$Age)

fullsample$Gender <- as.factor(fullsample$Gender)

fullsample$Partner <- NA
fullsample$Partner[is.element(fullsample$Marital_Status, c(2,4))] <- "Yes"
fullsample$Partner[is.element(fullsample$Marital_Status, c(1,3,5,6))] <- "No"
fullsample$Partner <- as.factor(fullsample$Partner)

fullsample$Political <- relevel(fullsample$Political, ref = "Ind.")


# run multinomial regression with predictors and DV cluster membership
library(nnet)
mymod <- multinom(class ~ Age + Gender + Partner + Children + Education + Income + Political, data=fullsample)
cs <- t(round(coef(mymod), 2))
cs <- round(exp(cs), 2)

z <- summary(mymod)$coefficients/summary(mymod)$standard.errors
ps <- (1 - pnorm(abs(z), 0, 1)) * 2
ps <- t(ps)
round(ps, 2)

sign_ind <- which(ps < .05)
cs[sign_ind] <- paste(cs[sign_ind], "*", sep="")

cs <- cs[-which(rownames(cs) == "(Intercept)"),]
colnames(cs) <- paste("C", colnames(cs), sep="")
missing <- colnames(pdat)[which(!is.element(colnames(pdat), colnames(cs)))]
cs <- cbind(cs, 1)
colnames(cs)[ncol(cs)] <- missing
cs <- cs[,match(colnames(pdat), colnames(cs))]


# plot cluster profiles
pdf(file=paste("../output/stage3/lpa/", sel_m, ".pdf", sep=""), width=10, height=4)
layout(matrix(c(1,2), ncol=1))
par(mar=c(1,4,2,0))

ydim <- ceiling(max(colSums(abs(t(c_means)))))
#cols = c("red", colorRampPalette(c("aquamarine1", "blue4"))(m$d-1))

library(viridis)
p_cols <- c("red", plasma(ncol(c_means)-1, begin=.10, end=.95))
#p_dens <- rep(c(NA, 35), 99)[1:length(p_cols)]
#p_angl <- rep(c(0, 90, 0, 45), 99)[1:length(p_cols)]
p_dens <- c(NA, 35, NA, NA, 35, NA)
p_angl <- c(0, 90, 0, 0, 45, 0)

# b <- barplot(pdat_pos, xlim=c(0, ncol(pdat)+1.5), ylim=c(-ydim,ydim), border="white", las=1, col=p_cols, density=p_dens, angle=p_angl, ylab="Mean factor scores")#, names=paste("C", 1:ncol(pdat), sep=""))
# barplot(pdat_neg, add=T, border="white", col=p_cols, density=p_dens, angle=p_angl, yaxt="n", names=rep("", ncol(pdat)))

if (rescale == T) ylim <- c(1,7) else ylim=c(-1.5,1.5)

plot(1, xlim=c(.5, ncol(pdat)+.5), ylim=ylim, type="n", las=1, xaxt="n", xlab=NA, ylab="Factor score", frame=F)
abline(h=4, lty=3)

#axis(1, at=1:ncol(pdat), colnames(pdat))
#text(x=1:ncol(pdat), y=-1.75, paste("Profile ", c("A","B","C","D"), " (", colnames(pdat), ")", sep=""), xpd=T)
text(x=1:ncol(pdat), y=-1.75, prof_lab, xpd=T)

abline(h=0, lty=3)

for (i in 1:ncol(pdat)) {
  
  shift <- seq(i-.25, i+.25, length.out=c(nrow(pdat)))
  
  rect(i-.4, ylim[1], i+.4, ylim[2], col=gray(.95), border=0)
  
  vals <- pdat[,i]
  for (j in 1:length(vals)) {
    lines(x=rep(shift[j], 2), y=c(0, vals[j]), col=p_cols[j], lwd=14, lend=1)
  
    # add estimated cluster means for both subsamples  
    lines(x=rep(shift[j]-0.01, 2), y=c(0, pdat_A[j,i]), col="lightgrey", lwd=2, lend=1)
    lines(x=rep(shift[j]+0.01, 2), y=c(0, pdat_B[j,i]), col="lightgrey", lwd=2, lend=1)
    
    # add empirical means
    lines(x=c(shift[j]-.025, shift[j]+.025), y=rep(emp_means[j,i], 2), col="black", lwd=1, lend=1)
    
    # add +/- 2 SEM
    lines(x=rep(shift[j], 2), y=c((emp_means - 2*emp_sem)[j,i], (emp_means + 2*emp_sem)[j,i]), col="black", lwd=3, lend=1)
    
    
    
  }
}

lab <- toupper(gsub(toupper(sel_m), "", row.names(t(c_means))))
lab2 <- lab
lab2 <- gsub("\\<R\\>", "General", lab2)
lab2 <- gsub("\\<SOC\\>", "Social", lab2)
lab2 <- gsub("\\<REC\\>", "Recreational", lab2)
lab2 <- gsub("\\<GAM\\>", "Gambling", lab2)
lab2 <- gsub("\\<INV\\>", "Investment", lab2)
lab2 <- gsub("\\<ETH\\>", "Ethical", lab2)
lab2 <- gsub("\\<HEA\\>", "Health", lab2)

l <- legend("top", lab2, box.lty=0, horiz=T, y.intersp=-3, xpd=T, bg=NA)

for (j in 1:length(p_cols)) {
  x <- l$text$x[j] - 0.1
  y <- l$text$y[j]
  s1 <- 0.035
  s2 <- 0.14
  rect(x-s1+.05, y-s2, x+s1+.04, y+s2, col=p_cols[j], xpd=T, border=0)
}

abline(h=0)

par(mar=c(0,4,1,0))
plot(pdat, type="n", xlim=c(0.5, ncol(pdat)+.5), ylim=c(0,nrow(cs)+3), xaxt="n", yaxt="n", xlab="", ylab="", frame=F)

Ns <- table(fullsample$class)
names(Ns) <- paste("C", names(Ns), sep="")
Ns <- Ns[colnames(cs)]
#Ns <- paste("N=", Ns, sep="")
mAge <- mAge[,colnames(cs)]
pFem <- pFem[colnames(cs)]

cs <- rbind(Ns, pFem, mAge, rep("", ncol(cs)), cs)

rownames(cs)[which(rownames(cs) == "Ns")] <- "N"
rownames(cs)[which(rownames(cs) == "pFem")] <- "Female"
rownames(cs)[which(rownames(cs) == "mAge")] <- "Mean Age"
rownames(cs)[which(rownames(cs) == "Age")] <- "Age (SD)"
rownames(cs)[which(rownames(cs) == "Gender2")] <- "Gender (male)"
rownames(cs)[which(rownames(cs) == "PartnerYes")] <- "With partner"
rownames(cs)[which(rownames(cs) == "PoliticalDem.")] <- "Democrat"
rownames(cs)[which(rownames(cs) == "PoliticalRep.")] <- "Republican"


text(cs,
     x=matrix(1:ncol(pdat), nrow=nrow(cs), ncol=ncol(pdat), byrow=T),
     y=matrix(nrow(cs):1, nrow=nrow(cs), ncol=ncol(cs), byrow=F),
     cex=.8, xpd=T)
#abline(v=b)
text(rownames(cs), x=0, y=nrow(cs):1, xpd=T, pos=4, cex=.8)
dev.off()




fullsample <- cbind(fullsample, Fs[row.names(fullsample),])
fullsample <- cbind(fullsample, unc=part_unc[as.numeric(row.names(fullsample))])

levels(fullsample$class) <- 1:length(levels(fullsample$class))

library(scales)
cols <- as.numeric(fullsample$class) + 1
cols <- alpha(cols, fullsample$unc)


# plot classification
pdf(file=paste("../output/stage3/lpa/", sel_m, "_classification.pdf", sep=""), width=10, height=10)
p_dat <- fullsample[,grepl("M5", colnames(fullsample))]
names(p_dat) <- toupper(gsub(toupper(sel_m), "", names(p_dat)))
plot(p_dat, col=cols, pch=18, cex=.5)
dev.off()


# plot classification probabilities
pdf(file=paste("../output/stage3/lpa/", sel_m, "_classprobs.pdf", sep=""), width=10, height=6)
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.25,.75,0))

p_cols2 <- inferno(n = length(unique(unlist(part_class))), begin=.2)

p_ok <- part_p
p_nok <- part_p
for (c in c_ok[c(1,3,4,2)])  {
  
  p_cols2 <- rep("cyan", length(p_cols2))
  p_cols2[c] <- "green4"
  
  ind <- which(part_class != c)
  p_ok[ind, c] <- NA
  p_nok[-ind, c] <- NA

  p_dat <- data.frame(p = part_p[,c], c = part_class)
  #p_dat <- p_dat[order(p_dat$p, decreasing=T),]
  p_dat <- p_dat[order(p_dat$c),]
  p_dat$col <- p_dat$col <- p_cols2[p_dat$c]
  p_dat$ind <- 1:nrow(p_dat)
  
  #barplot(p_dat$p, ylim=c(0, 1), col=p_dat$c, las=1, border=0)
  
  current_title <- prof_lab[grep(c, prof_lab)]
  current_title <- gsub("P", "Probability of being classified to p", current_title)
  
  plot(1, type="n", xlim=c(1,3200), ylim=c(-0.1,1), las=1, xlab="Participants", ylab="", main=current_title, frame=0, xaxt="n")
  for (x in 1:nrow(p_dat)) {
    lines(x=c(x,x), y=c(0,p_dat$p[x]), col=p_dat$col[x], lwd=.0001)
  }
  xs <- seq(1, 3200, length.out=5)
  axis(1, at=xs, floor(xs))
  text("Classification probability", srt=90, x=-550, y=.5, xpd=T)
  
  ranges <- tapply(p_dat$ind, list(p_dat$cluster), range)
  for (j in 1:length(ranges)) {
    range <- ranges[[j]]
    if (j == c) col2 = "green4" else col2 = "darkgrey"
    lines(x=c(range[1], range[1]), y=c(-0.1,-0.02), xpd=T, col=col2)
    lines(x=c(range[2], range[2]), y=c(-0.1,-0.02), xpd=T, col=col2)
    lines(x=c(range[1], range[2]), y=c(-0.1,-0.1), xpd=T, col=col2)
    #rect(range[1], -.1, range[2], 1.02, col=NA, border="grey", xpd=T)
    text(paste("C", names(ranges)[j], sep=""), x=mean(range), y=-.05, cex=.6, col=col2)
  }
  
}
dev.off()

# print p's with which participants were classified to "their" cluster
print(round(apply(p_ok, 2, mean, na.rm=T), 2))

# print p with which participants were classified to any other cluster
print(round(mean(unlist(p_nok), na.rm=T), 2))


pdat <- cbind(pdat, data.frame("EX" = c(.5, .25, 0.05, -.9, -1, .5, 1.1)))

# plot profiles seperately
for (i in 1:ncol(pdat)) {
  
  c <- colnames(pdat)[i]
  
  png(file=paste("../output/stage3/lpa/", sel_m, "_", c, ".png", sep=""), width=3000, height=900, res=300)
  
  par(mar=c(1,4,1,1))
  
  plot(1, xlim=c(.75, 1.25), ylim=ylim, type="n", las=1, xaxt="n", xlab=NA, ylab="", yaxt="n", frame=F)
  #axis(2, labels=F)
  
  rect(.75, ylim[1], 1.25, ylim[2], col=gray(.95), border=0)
  
  shift <- seq(1-.2, 1+.2, length.out=c(nrow(pdat)))
  
  
  
  arrows(x0=.73, y0=0.3, y1=1.4, xpd=T)
  arrows(x0=.73, y0=-0.3, y1=-1.4, xpd=T)
  
  text(0.7, .8, srt=90, "more\nrisk-seeking", xpd=T, cex=.9)
  text(0.7, -.8, srt=90, "more\nrisk-averse", xpd=T, cex=.9)
  
  vals <- pdat[,i]
  for (j in 1:length(vals)) {
    lines(x=rep(shift[j], 2), y=c(0, vals[j]), col=p_cols[j], lwd=60, lend=1)
    text(shift[j], 1.5, xpd=T, lab2[j], cex=1, pos=3)
    
  }
  
  #lines(c(.69, 1.25), c(0, 0), xpd=T)
  lines(c(.75, 1.25), c(0, 0), xpd=T)
  text(.73, 0, "Average", xpd=T)
  
  dev.off()
}


# plot "original" DOSPERT raw scores (for comparisons)
pdf(file=paste("../output/stage3/lpa/", sel_m, "_DOSPERT_raw.pdf", sep=""), width=10, height=4)
#p_cols3 <- p_cols
#p_cols3 <- p_cols3[-1]
#p_cols3 <- p_cols3[-3]
p_cols3 <- cividis(n=nrow(dospert_cmeans))

b <- barplot(dospert_cmeans, beside=T, ylim=c(0,6), yaxt="n", col=p_cols3, border="white", ylab="Average DOSPERT-Score", names=prof_lab)
axis(2, at=0:6, 1:7, las=1)
abline(h=3, lty=3)
legend(x=12.5, xjust=0.5, y=7.5, xpd=T, horiz=T, c("Social", "Recreational", "Financial", "Ethical", "Health"), box.lwd=0, pch=15, col=p_cols3)
dev.off()