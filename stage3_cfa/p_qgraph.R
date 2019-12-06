library(qgraph)
library(viridis)

# functions to rotate entire plot
rotMat <- function(x){
  matrix(c(cos(x),sin(x),-sin(x),cos(x)),2,2)
}
rotateLayout <- function(layout, rotation){
  layout %*% rotMat(rotation)
}

cols <- viridis(length(sel_ms), begin=0, end=.95)
cols <- c(cols, "black", grey(.4))
cols[5] <- "red"

#ord <- rownames(m3$loadings)
#cormat <- cormat[ord, ord]

Q <- qgraph(cormat, minimum=.0, cut=0, layout="spring", vsize=4, legend=F, mar=c(1.5,2.5,1.5,2), shape="circle", label.color="white", color=cols, borders=F, border.color="black", groups=grps, labels=rownames(cormat), posCol=grey(.3), negCol=grey(.3), edge.labels=F, DoNotPlot=T)


# rotate entire plot?
if (T) {
  rot <- 0.80 # rotation in percentage [0-1]
  Q <- qgraph(Q, layout = rotateLayout(Q$layout, pi))
}


p_cols <- c("red", plasma(6, begin=.10, end=.95))
ind <- match(paste("M5", c("r", "soc", "rec", "gam", "inv", "eth", "hea"), sep=""), Q$graphAttributes$Nodes$labels)
Q$graphAttributes$Nodes$color[ind] <- p_cols

Q$graphAttributes$Nodes$labels <- substr(colnames(cormat), 1, 5)

weights <- Q$Edgelist$weight

# adjust postive / negative line type
ind_pos <- which(weights >= 0)
ind_neg <- which(weights < 0)
Q$graphAttributes$Edges$lty[ind_pos] <- 1
Q$graphAttributes$Edges$lty[ind_neg] <- 2

# adjust line widths
line_cutoff <- 0
ind_plot <- which(abs(weights) >= line_cutoff)
ind_noplot <- setdiff(1:length(weights), ind_plot)

weights_cat <- cut(weights, breaks=seq(-1, 1, length.out=21), labels=c(-10:-1, 1:10))
weights_cat <- abs(as.numeric(as.character(weights_cat)))

exist_min <- min(weights_cat[ind_plot])
exist_max <- max(weights_cat[ind_plot])

cats_lab <- paste((0:9)/10, "-", (1:10)/10, sep="")
cats_lwd <- seq(.5, 2, length.out=10)

p_lwd <- cats_lwd[weights_cat]
#p_lwd[ind_noplot] <- 0

Q$graphAttributes$Edges$width <- p_lwd


# adjust cutoff labels
label_cutoff <- 1
ind_noplot <- which(abs(weights) < label_cutoff)
Q$graphAttributes$Edges$labels[ind_noplot] <- ""



# adjust node shapes
Q$graphAttributes$Nodes$shape[grps$`SOEP risk items`] <- "square"
Q$graphAttributes$Nodes$width[grps$`SOEP risk items`] <- 4
Q$graphAttributes$Nodes$shape[grps$`Risk-return framework`] <- "triangle"
Q$graphAttributes$Nodes$width[grps$`Risk-return framework`] <- 4

Q$graphAttributes$Nodes$borders[ind_m5] <- T
Q$graphAttributes$Nodes$border.width[ind_m5] <- 2
Q$graphAttributes$Nodes$border.color[ind_m5] <- "black"

Q$graphAttributes$Nodes$width[grps$`Model 5`] <- 5.5


plot(Q)
legend("topright", cats_lab[exist_min:exist_max], lwd=cats_lwd[exist_min:exist_max], box.lty=0, cex=.9, title="Correlation")
pchs <- c(rep(19, length(sel_ms)), 17, 15)
pchs[5] <- 21
cols[5] <- "black"

legend("topleft", names(grps), pch=pchs, pt.bg="white", col=cols, box.lty=0, cex=.9, horiz=F)