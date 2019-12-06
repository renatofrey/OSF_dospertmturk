library(semPlot)
source("functions.R")

for (sel in c("subsampleA", "subsampleB", "full")) {

  fits <- NULL
    
  load(paste("../objects/stage3/cfas_", sel, ".Rdata", sep=""))
  
  # get explained variances from all models
  ms <- paste("m", 1:6, "_fit", sep="")
  vars <- t(sapply(ms, function(x) {get_var(get(x))}))
  
  if (F) {
    pdf("../output/stage3/cfas_var.pdf", width=10, height=7)
    p_cols <- c(rgb(1,0,0,.25), p_col <- rgb(0,1,0,.25))
    barplot(t(vars[,c("total_R","total_Fs")]), las=1, col=p_cols, border=0, ylab="Explained variance", ylim=c(0, .5), names=paste("Model", 1:6))
    legend("topleft", horiz=F, pch=15, col=p_cols, c("R (general factor)", "Fs (specific factors"), box.lty=0)
    dev.off()
  }
  
  # determine factor inter-correlations (m2 and m3)
  if (sel == "full") {
    for (j in c("m2_fit", "m3_fit")) {
      pars <- parameterestimates(get(j))
      pars <- subset(pars, op == "~~" & nchar(lhs) < 4)
      pars <- subset(pars, lhs != rhs)
      print(j)
      print(round(summary(abs(pars$est)), 2))
    } 
    
    compare(m4_fit, m2_fit)
    
  }
  
  # Model 1
  fits <- rbind(fits, m1 = get_fits(m1_fit))
  pdf(file=paste("../output/stage3/cfa_", sel, "/R_m1.pdf", sep=""))
  p <- semPaths(m1_fit, layout="circle", whatLabel="std", nCharNodes=6, intercepts=F, thresholds=F, residuals=F, exoCov=T, mar=c(2,2,2,2), edge.label.cex=.5, sizeLat=8, sizeMan=6, DoNotPlot=T)
  p$graphAttributes$Edges$label.color <- rep("black", length(p$graphAttributes$Edges$labels))
  p$layout[31,2] <- 0
  plot_var(p)
  lab <- bquote("Model 1:" ~ R^2 ~ "=" ~ .(vars[["m1_fit","total_expl"]]) ~ "(R: "~.(vars[["m1_fit","expl_R"]]*100)~"% / Fs: "~.(vars[["m1_fit","expl_Fs"]]*100)~"%)") 
  title(lab, line=3, font=1, cex.main=.8)
  dev.off()
  
  # Model 2
  fits <- rbind(fits, m2 = get_fits(m2_fit))
  pdf(file=paste("../output/stage3/cfa_", sel, "/R_m2.pdf", sep=""))
  p <- semPaths(m2_fit, layout="circle", whatLabel="std", nCharNodes=6, intercepts=F, thresholds=F, residuals=F, exoCov=T, mar=c(2,2,2,2), edge.label.cex=.5, sizeLat=8, sizeMan=6, DoNotPlot=T)
  p$graphAttributes$Edges$label.color <- rep("black", length(p$graphAttributes$Edges$labels))
  plot_var(p)
  lab <- bquote("Model 2:" ~ R^2 ~ "=" ~ .(vars[["m2_fit","total_expl"]]) ~ "(R: "~.(vars[["m2_fit","expl_R"]]*100)~"% / Fs: "~.(vars[["m2_fit","expl_Fs"]]*100)~"%)") 
  title(lab, line=3, font=1, cex.main=.8)
  dev.off()
  
  # Model 3
  fits <- rbind(fits, m3 = get_fits(m3_fit))
  pdf(file=paste("../output/stage3/cfa_", sel, "/R_m3.pdf", sep=""))
  p <- semPaths(m3_fit, layout="circle", whatLabel="std", nCharNodes=6, intercepts=F, thresholds=F, residuals=F, exoCov=T, mar=c(2,2,2,2), edge.label.cex=.5, sizeLat=8, sizeMan=6, DoNotPlot=T)
  p$graphAttributes$Edges$label.color <- rep("black", length(p$graphAttributes$Edges$labels))
  plot_var(p)
  lab <- bquote("Model 3:" ~ R^2 ~ "=" ~ .(vars[["m3_fit","total_expl"]]) ~ "(R: "~.(vars[["m3_fit","expl_R"]]*100)~"% / Fs: "~.(vars[["m3_fit","expl_Fs"]]*100)~"%)") 
  title(lab, line=3, font=1, cex.main=.8)
  dev.off()
  
  # Model 4
  fits <- rbind(fits, m4 = get_fits(m4_fit))
  pdf(file=paste("../output/stage3/cfa_", sel, "/R_m4.pdf", sep=""), width=14)
  p <- semPaths(m4_fit, layout="tree2", whatLabel="std", nCharNodes=6, intercepts=F, thresholds=F, residuals=F, exoCov=F, bifactor="R", mar=c(4,1,4,1), edge.label.cex=.3, sizeLat=6, sizeMan=3.4, DoNotPlot=T)
  p$graphAttributes$Edges$label.color <- rep("black", length(p$graphAttributes$Edges$labels))
  plot_var(p)
  lab <- bquote("Model 4:" ~ R^2 ~ "=" ~ .(vars[["m4_fit","total_expl"]]) ~ "(R: "~.(vars[["m4_fit","expl_R"]]*100)~"% / Fs: "~.(vars[["m4_fit","expl_Fs"]]*100)~"%)") 
  title(lab, line=3, font=1, cex.main=.8)
  dev.off()
  
  # Model 5
  fits <- rbind(fits, m5 = get_fits(m5_fit))
  pdf(file=paste("../output/stage3/cfa_", sel, "/R_m5.pdf", sep=""), width=15, height=5)
  p <- semPaths(m5_fit, layout="tree2", whatLabel="std", nCharNodes=6, intercepts=F, thresholds=F, residuals=F, exoCov=F, bifactor="R", mar=c(3,1,5,1), edge.label.cex=.3, sizeLat=6, sizeMan=3.4, DoNotPlot=T)
  p$graphAttributes$Edges$label.color <- rep("black", length(p$graphAttributes$Edges$labels))
  
  library(viridis)
  p_cols <- c("red", plasma(6, begin=.10, end=.95))
  ind <- match(c("R", "SOC", "REC", "GAM", "INV", "ETH", "HEA"), p$graphAttributes$Nodes$labels)
  p$graphAttributes$Nodes$color[ind] <- p_cols
  p$graphAttributes$Nodes$label.color[ind] <- "white"
  
  plot_var(p, ratio=(14/5)/2)
  lab <- bquote("Model 5:" ~ R^2 ~ "=" ~ .(vars[["m5_fit","total_expl"]]) ~ "(R: "~.(vars[["m5_fit","expl_R"]]*100)~"% / Fs: "~.(vars[["m5_fit","expl_Fs"]]*100)~"%)") 
  title(lab, line=3, font=1, cex.main=.8)
  dev.off()
  
  # Model 6
  fits <- rbind(fits, m6 = get_fits(m6_fit))
  pdf(file=paste("../output/stage3/cfa_", sel, "/R_m6.pdf", sep=""), width=14)
  p <- semPaths(m6_fit, layout="tree2", whatLabel="std", nCharNodes=6, intercepts=F, thresholds=F, residuals=F, exoCov=F, bifactor="R", mar=c(4,1,4,1), edge.label.cex=.3, sizeLat=6, sizeMan=3.4, DoNotPlot=T)
  p$graphAttributes$Edges$label.color <- rep("black", length(p$graphAttributes$Edges$labels))
  plot_var(p)
  lab <- bquote("Model 6:" ~ R^2 ~ "=" ~ .(vars[["m6_fit","total_expl"]]) ~ "(R: "~.(vars[["m6_fit","expl_R"]]*100)~"% / Fs: "~.(vars[["m6_fit","expl_Fs"]]*100)~"%)") 
  title(lab, line=3, font=1, cex.main=.8)
  dev.off()
  
  assign(paste("fits_", sel, sep=""), fits)
  assign(paste("vars_", sel, sep=""), vars)
  
}

save(fits_subsampleA, fits_subsampleB, fits_full,
     vars_subsampleA, vars_subsampleB, vars_full,
     file="../objects/stage3/cfa_fits.Rdata")