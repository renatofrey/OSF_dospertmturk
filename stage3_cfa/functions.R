get_fits <- function(m) {
  library(lavaan)
  
  fits <- round(inspect(m, "fitmeasures")[c("cfi", "tli", "bic", "aic", "rmsea", "srmr", "npar", "df")], 3)
  fits <- c(fits, r2 = round(sum(inspect(m, "r2")) / length(inspect(m, "r2")), 2))
  return(fits)
}

get_var <- function(m, prec=2) {
  library(lavaan)
  
  pars <- parameterEstimates(m, standardized=T)
  var_r <- data.frame(subset(pars, lhs == "R" & op == "=~")[,c("lhs", "rhs", "est", "std.all")])
  var_e <- data.frame(subset(pars, lhs == rhs & est != 1)[,c("lhs", "rhs", "est", "std.all")])
  var_f <- data.frame(subset(pars, lhs != "R" & op == "=~")[,c("lhs", "rhs", "est", "std.all")])
  
  # Total explained / unexplained variance
  total_expl <- 1-round(sum(var_e$std.all)/nrow(var_e), prec)
  total_unexpl <- round(sum(var_e$std.all)/nrow(var_e), prec)
  
  # Proportion of total variance explained by R / Fs
  total_R <- round(sum(var_r[,"std.all"]^2) / nrow(var_e), prec)
  total_Fs <- round(sum(var_f[,"std.all"]^2) / nrow(var_e), prec)
  
  # Shares of explained variance
  expl_R <- round(sum(var_r[,"std.all"]^2) / (sum(var_r[,"std.all"]^2) + sum(var_f[,"std.all"]^2)), prec)
  expl_Fs <- round(sum(var_f[,"std.all"]^2) / (sum(var_r[,"std.all"]^2) + sum(var_f[,"std.all"]^2)), prec)
  
  vars <- as.data.frame(cbind(total_expl, total_unexpl, total_R, total_Fs, expl_R, expl_Fs))
  return(vars)
  
}

plot_var <- function(s, ratio=2) {
  
  l <- gsub("R_", "", s$graphAttributes$Nodes$labels, fixed=F)
  #s$graphAttributes$Nodes$label.color <- "white"
  s$graphAttributes$Nodes$labels <- l
  plot(s)
  
  nodes <- s$graphAttributes$Nodes$labels
  labels <- as.character(nodes)
  man <- labels[grepl("R_", names(nodes))]
  
  node_inds <- which(is.element(labels, man))
  
  for (node_ind in node_inds) {
    
    if (!is.element(node_ind, s$Edgelist$to)) next;
    
    x <- s$layout[node_ind,1]
    y <- s$layout[node_ind,2]
    
    edge_inds <- as.numeric(which(s$Edgelist$to == node_ind))
    
    for (seg in 1:length(edge_inds)) {
      
      est <- as.numeric(s$graphAttributes$Edges$labels)[edge_inds[seg]]
      r2 <- est^2
      
      node_from <- as.numeric(s$Edgelist$from[edge_inds[seg]])
      y_from <- s$layout[node_from,2]
      if (labels[node_from] == "R") pos <- "top" else pos <- "bot"
      
      size <- 0.15
      
      if (length(unique(s$layout[,2])) <= 3) factor <- 5 else factor <- 2
      
      x1 <- x-size/factor
      x2 <- x+size/factor
      y1 <- y-size/ratio
      y2 <- y+size/ratio
      
      xdiff <- x2-x1
      ydiff <- y2-y1
      
      seg1 <- ydiff * r2
      
      if (pos == "top") {
        y1 <- y2 - seg1
        p_col <- rgb(1,0,0,.25)
      }
      
      if (pos == "bot") {
        y2 <- y1 + seg1
        p_col <- rgb(0,1,0,.25)
      }
      
      rect(x1, y1, x2, y2, col=p_col, border=0)
      
    }
    
  }
  
  for (i in 1:nrow(s$layout)) {
    
    x <- s$layout[i,1]
    y <- s$layout[i,2]
    
    #text(x, y, nodes[i], col="black")
    
  }
  
}


compare <- function(fitA, fitB) {
  modelcomp <- anova(fitA, fitB)
  
  bicA <- BIC(fitA)
  bicB <- BIC(fitB)
  bf <- round(exp(-1/2 * (bicB - bicA)), 8)
  
  cfiA <- round(fitMeasures(fitA)["cfi"], 2)
  cfiB <- round(fitMeasures(fitB)["cfi"], 2)
  
  print(modelcomp)
  
  print(paste("CFI A:", cfiA, ", CFI B:", cfiB, ". CFI-Difference:", cfiB-cfiA))
  print(paste("BIC A:", round(bicA, 2), ", BIC B:", round(bicB, 2), ". BIC-Difference:", round(bicB-bicA, 2)))
  print(paste("Bayesfactor B | A:", bf))
  
}