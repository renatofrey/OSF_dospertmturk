args <- (commandArgs(TRUE))
if (length(args) == 0) {
  sel <- "full"
  print("No arguments supplied. Using full sample.")
} else {
  print(args)
  for (i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}

library(lavaan)
source("functions.R")
load("../objects/stage2/efa_R.Rdata")

# read data
subsampleA <- read.csv("../data/subsampleA.csv", row.names=1)
subsampleB <- read.csv("../data/subsampleB.csv", row.names=1)
full <- read.csv("../data/fullsample.csv", row.names=1)

labels <- names(full)

# decipher blinded dataset
key <- read.csv("../data/key.csv", row.names=1)

for (k in nrow(key):1) {
  new <- as.character(key[k,"new"])
  ori <- as.character(key[k,"original"])
  
  m3_str$str <- gsub(new, ori, m3_str$str, fixed=T)
  m3_str$tab <- gsub(new, ori, m3_str$tab, fixed=T)
  
  m5_str$str <- gsub(new, ori, m5_str$str, fixed=T)
  m5_str$tab <- gsub(new, ori, m5_str$tab, fixed=T)
  
  m6_str$str <- gsub(new, ori, m6_str$str, fixed=T)
  m6_str$tab <- gsub(new, ori, m6_str$tab, fixed=T)
}

m1_str <- paste('
# latent variables
R =~ ', paste(labels[grepl("R_", labels)], collapse=" + ", sep=""),'
', sep="")
cat(m1_str)
m1_fit <- cfa(m1_str, data=get(sel), std.lv=T, estimator="ML", se="bootstrap")

m2_str <- paste('
# latent variables
ETH =~ ', paste(labels[grepl("ethR_", labels)], collapse=" + ", sep=""),'
FIN =~ ', paste(labels[grepl("finR_", labels)], collapse=" + ", sep=""),'
HEA =~ ', paste(labels[grepl("heaR_", labels)], collapse=" + ", sep=""),'
REC =~ ', paste(labels[grepl("recR_", labels)], collapse=" + ", sep=""),'
SOC =~ ', paste(labels[grepl("socR_", labels)], collapse=" + ", sep=""),'
', sep="")
cat(m2_str)
m2_fit <- cfa(m2_str, data=get(sel), std.lv=T, estimator="ML", se="bootstrap")


m3_str$str <- gsub("F1", "REC", m3_str$str)
m3_str$str <- gsub("F2", "GAM", m3_str$str)
m3_str$str <- gsub("F3", "SOC", m3_str$str)
m3_str$str <- gsub("F4", "ETH", m3_str$str)
m3_str$str <- gsub("F5", "HEA", m3_str$str)
m3_str$str <- gsub("F6", "INV", m3_str$str)
cat(m3_str$str)
m3_fit <- cfa(m3_str$str, data=get(sel), std.lv=T, estimator="ML", se="bootstrap")
# note: "equivalent" solution with higher-order factor has fewer parameters and a worse absolute fit!


m4_str <- paste('
# latent variables
R =~ ', paste(labels[grepl("R_", labels)], collapse=" + ", sep=""),'
ETH =~ ', paste(labels[grepl("ethR_", labels)], collapse=" + ", sep=""),'
FIN =~ ', paste(labels[grepl("finR_", labels)], collapse=" + ", sep=""),'
HEA =~ ', paste(labels[grepl("heaR_", labels)], collapse=" + ", sep=""),'
REC =~ ', paste(labels[grepl("recR_", labels)], collapse=" + ", sep=""),'
SOC =~ ', paste(labels[grepl("socR_", labels)], collapse=" + ", sep=""),'
', sep="")
cat(m4_str)
m4_fit <- cfa(m4_str, data=get(sel), std.lv=T, orthogonal=T, estimator="ML", se="bootstrap")


m5_str$str <- gsub("F1", "SOC", m5_str$str)
m5_str$str <- gsub("F2", "REC", m5_str$str)
m5_str$str <- gsub("F3", "GAM", m5_str$str)
m5_str$str <- gsub("F4", "INV", m5_str$str)
m5_str$str <- gsub("F5", "ETH", m5_str$str)
m5_str$str <- gsub("F6", "HEA", m5_str$str)
cat(m5_str$str)
m5_fit <- cfa(m5_str$str, data=get(sel), std.lv=T, orthogonal=T, estimator="ML", se="bootstrap")


m6_str$str <- gsub("F1", "SOC", m6_str$str)
m6_str$str <- gsub("F2", "HEA", m6_str$str)
m6_str$str <- gsub("F3", "REC", m6_str$str)
m6_str$str <- gsub("F4", "INV", m6_str$str)
cat(m6_str$str)
m6_fit <- cfa(m6_str$str, data=get(sel), std.lv=T, orthogonal=T, estimator="ML", se="bootstrap")


save(m1_fit, m2_fit, m3_fit, m4_fit, m5_fit, m6_fit,
     file=paste("../objects/stage3/cfas_", sel, ".Rdata", sep=""))