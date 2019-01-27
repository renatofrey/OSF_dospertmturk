# run exploratory factor analyses

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
nobs <- nrow(d)
nvar <- ncol(d)

# determine appropriate number of factors: PCA analysis
my.pca <- princomp(cormat)
summary(my.pca)
# plot(my.pca, las=2, ylim=c(0,1))

# determine number of factors to extract
library(psych)
library(nFactors)
ev <- eigen(cormat)
ap <- parallel(subject=nobs, var=nvar, rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
nfact <- as.numeric(nS$Components["noc"])
pdf(file=paste("../output/stage2/screeplot_", sel, ".pdf", sep=""))
plotnScree(nS)
dev.off()

# EFA for model 3
j <- 6
m3 <- fa(r=cormat, covar=F, n.obs=nobs, nfactors=j, rotate="promax")
print(m3$loadings, digits=2, cutoff=.2, sort=T)
m3 <- fa.sort(m3)
colnames(m3$loadings) <- paste("F", 1:j, sep="")
tab_m3 <- fa2latex(m3, cumvar=T, big = .2,
                   caption = "EFA with six factors for ``risk-taking propensity'' (promax rotation; absolute loadings exceeding .2 are highlighted).",
                   label = "tab_Rm3",
                   heading = "",
                   font.size ="small",
                   silent = T)
tab_m3 <- gsub("table", "table*", tab_m3, fixed=T)
cat(tab_m3, file = paste("../output/stage2/tab_", sel, "m3.tex", sep=""))


# EFA for model 5
k <- 6
m5 <- fa(r=cormat, covar=F, n.obs=nobs, nfactors=1+k, rotate="bifactor")
print(m5$loadings, digits=2, cutoff=.2, sort=T)
m5 <- fa.sort(m5)
colnames(m5$loadings) <- c("R", paste("F", 1:k, sep=""))
tab_m5 <- fa2latex(m5, cumvar=T, big = .2,
                   caption = "EFA with one general and six specific factors for ``risk-taking propensity'' (bifactor rotation; absolute loadings exceeding .2 are highlighted).",
                   label = "tab_Rm5",
                   heading = "",
                   font.size ="small",
                   silent = T)
tab_m5 <- gsub("table", "table*", tab_m5, fixed=T)
cat(tab_m5, file = paste("../output/stage2/tab_", sel, "m5.tex", sep=""))


# EFA for model 6
k2 <- 4
m6 <- fa(r=cormat, covar=F, n.obs=nobs, nfactors=1+k2, rotate="bifactor")
print(m6$loadings, digits=2, cutoff=.2, sort=T)
m6 <- fa.sort(m6)
colnames(m6$loadings) <- c("R", paste("F", 1:k2, sep=""))
tab_m6 <- fa2latex(m6, cumvar=T, big = .2,
                   caption = "EFA with one general and four specific factors for ``risk-taking propensity'' (bifactor rotation; absolute loadings exceeding .2 are highlighted).",
                   label = "tab_Rm6",
                   heading = "",
                   font.size ="small",
                   silent = T)
tab_m6 <- gsub("table", "table*", tab_m6, fixed=T)
cat(tab_m6, file = paste("../output/stage2/tab_", sel, "m6.tex", sep=""))



# create models for subsequent CFAs
source("efa_mstring.R")
model_strings <- NULL

# define cutoff
t = .2

m3_str <- get_mstring(m3$loadings, t=t)
cat(m3_str$str)
lab <- rep("", ncol(m3_str$tab))
lab[1] <- "Model 3:"
model_strings <- rbind(model_strings, lab, m3_str$tab)

m5_str <- get_mstring(m5$loadings, t=t, bifactor=T, biallvars=T)
cat(m5_str$str)
lab[1] <- "Model 5:"
model_strings <- rbind(model_strings, lab, m5_str$tab)

m6_str <- get_mstring(m6$loadings, t=.2, bifactor=T, biallvars=F)
cat(m6_str$str)
lab[1] <- "Model 6:"
model_strings <- rbind(model_strings, lab, m6_str$tab)

rownames(model_strings) <- 1:nrow(model_strings)

library(xtable)
xtab <- xtable(model_strings,
               type="latex",
               label=paste("tab:modelstrings"),
               caption=paste("Models for ``risk-taking propensity'', to be tested using confirmatory factor analyses in stage-III."),
               sanitize=identiy
               #align=c("lp{1in} ", rep("rp{1in} ", ncol(tab2)))
)

output <- print(xtab,
                include.rownames=F,
                include.colnames=F,
                size="tiny",
                #table.placement = getOption("xtable.table.placement", "H"),
                caption.placement = "top",
                file="")

output <- gsub("\\centering", "\\centering \\setlength\\tabcolsep{.5pt}", output, fixed=T)

output <- sub("\\hline\n  \\hline\n", "\\hline\n", output, fixed=T)
output <- gsub("\\begin{table}", "\\begin{table*}", output, fixed=T)
output <- gsub("\\end{table}", "\\end{table*}", output, fixed=T)

cat(output)
cat(output, file="../output/stage2/tab_modelstrings.tex")




# create network plot (using m3 for grouping)
factors <- apply(m3$loadings, 1, function(x) {which.max(x)})

grps <- sapply(1:nfact, function(x) {as.numeric(which(x == factors))})
if (is.null(dim(grps))) names(grps) <- paste("F", 1:nfact, sep="") else {
  grps <- data.frame(grps)
  colnames(grps) <- paste("F", 1:nfact, sep="")
}

pdf(paste("../output/stage2/network_", sel, "m3.pdf", sep=""), width=12, height=9)
source("p_qgraph.R")
dev.off()

save(d, cormat, nobs, nvar, m3, m5, m6, m3_str, m5_str, m6_str,
     file=paste("../objects/stage2/efa_", sel, ".Rdata", sep=""))