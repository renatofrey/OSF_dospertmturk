load("../objects/stage3/cfa_fits.Rdata")

pdf("../output/stage3/cfas_fits.pdf", height=6, width=8)

par(mfrow=c(2,4), mar=c(4,4,3,2))

sel_models <- ""
ls <- c(1,1,1,3,3,3)

measures <- colnames(fits_full)[c(9,1,2,5,6,3,4)]

for (meas in measures) {

  p_data <- cbind(fits_subsampleA[, meas],
                  fits_subsampleB[, meas],
                  fits_full[, meas])
  p_data <- t(p_data)
  
  p_type = "b"
  if (is.element(meas, c("bic", "aic"))) p_type <- "p"
  
  if (meas == "r2") p_title <- expression(bold(R^2)) else p_title <- toupper (meas)
  
  matplot(p_data, type=p_type, las=1, main=p_title, ylab="", xaxt="n", xlim=c(.5, 3.5), pch=1:6, lty=ls)
  #text(1, p_data[1,], paste("M", as.character(1:6, sep="")), cex=1)
  #text(2, p_data[2,], paste("M", as.character(1:6, sep="")), cex=1)
  
  axis(1, at=1:3, c("A", "B", "Full"), mgp=c(2,2,0))
    
}

#frame()

frame()

for (i in 1:6) {
  ys <- seq(0.85, 0.15, length.out=6)
  y <- ys[i]
  lines(x=c(0.05, 0.35), y=c(y, y), lty=ls[i], col=i, lwd=2)
  points(0.2, y, pch=i, col=i, cex=1)
  text(0.45, y=y, pos=4, paste("Model", i))
}


dev.off()




# generate a latex table

tab <- data.frame("Model" = paste("M", 1:6, sep=""))
  #"Bifactor" = c("No", "No", "No", "Yes", "Yes", "Yes")
  #"Items" = c(30, 30, 26, 30, 30, 28))

f <- fits_full
colnames(f) <- toupper(colnames(f))
f <- f[,c("DF", "RMSEA", "SRMR", "CFI", "TLI", "BIC", "AIC")]
tab <- cbind(tab, f)

#tab$Items <- as.character(tab$Items)
#tab$NPAR <- as.character(tab$NPAR)
tab$DF <- as.character(tab$DF)

tab$BIC <- as.character(round(tab$BIC, 0))
tab$AIC <- as.character(round(tab$AIC, 0))

# add explained variance
load("../objects/stage3/cfas_full.Rdata")
source("functions.R")
vars <- as.data.frame(t(sapply(paste(rownames(tab), "_fit", sep=""), function(x) {get_var(get(x))})))

vars[apply(vars, c(1,2), function(x) {x == 0})] <- "--"

tab$`R\\textsuperscript{2}` <- vars$total_expl
tab$`R\\textsuperscript{2}(R)` <- vars$total_R
tab$`R\\textsuperscript{2}(Fs)` <- vars$total_Fs



library(xtable)
xtab <- xtable(tab,
               caption="Model fits (full sample)",
               label="tab:fits_full",
               type="latex"
)

xtab <- print(xtab,
              table.placement = "t",
              caption.placement = "top",
              include.rownames = F,
              sanitize.text.function = identity,
              floating.environment="table",
              size = "small",
              file="")

str_note <- "Note. DF = degrees of freedom, RMSEA = root mean square error of approximation, SRMR = standardized root mean square residual, CFI = comparative fix index, TLI = Tucker-Lewis index, BIC = Bayesian Information Criterion, AIC = Akaike information criterion, R\\textsuperscript{2} = Explained variance (in total, R = by the general factor, Fs = by the specific factors)."

xtab <- gsub("\\end{table", paste("\n\\raggedright\n{\\footnotesize ", str_note, "}\n\n\\end{table", sep=""), xtab, fixed=T)

cat(xtab, file=paste("../output/stage3/tab_fits_full.tex", sep=""))