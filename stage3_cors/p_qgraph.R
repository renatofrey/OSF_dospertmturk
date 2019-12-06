library(qgraph)
library(viridis)

# functions to rotate entire plot
rotMat <- function(x){
  matrix(c(cos(x),sin(x),-sin(x),cos(x)),2,2)
}
rotateLayout <- function(layout, rotation){
  layout %*% rotMat(rotation)
}

cols <- viridis(length(grps))

ord <- rownames(m3$loadings)
cormat <- cormat[ord, ord]

Q <- qgraph(cormat, minimum=.0, cut=0, layout="spring", vsize=5, legend=F, mar=c(2,6,4,2), shape="square", label.color="white", color=cols, borders=F, border.color="black", groups=grps, labels=substr(colnames(cormat), 1, 5), posCol="black", negCol="red", edge.labels=F, DoNotPlot=T)

# rotate entire plot?
if (T) {
  rot <- 0.80 # rotation in percentage [0-1]
  Q <- qgraph(Q, layout = rotateLayout(Q$layout, pi))
}

weights <- Q$Edgelist$weight

# adjust postive / negative line type
ind_pos <- which(weights >= 0)
ind_neg <- which(weights < 0)
Q$graphAttributes$Edges$lty[ind_pos] <- 1
Q$graphAttributes$Edges$lty[ind_neg] <- 2

# adjust line withs
line_cutoff <- 0
ind_plot <- which(abs(weights) >= line_cutoff)
ind_noplot <- setdiff(1:length(weights), ind_plot)

weights_cat <- cut(weights, breaks=seq(-1, 1, length.out=21), labels=c(-10:-1, 1:10))
weights_cat <- abs(as.numeric(as.character(weights_cat)))

exist_min <- min(weights_cat[ind_plot])
exist_max <- max(weights_cat[ind_plot])

cats_lab <- paste((0:9)/10, "-", (1:10)/10, sep="")
cats_lwd <- seq(.5, 3, length.out=10)

p_lwd <- cats_lwd[weights_cat]
#p_lwd[ind_noplot] <- 0

Q$graphAttributes$Edges$width <- p_lwd


# adjust cutoff labels
label_cutoff <- 1
ind_noplot <- which(abs(weights) < label_cutoff)
Q$graphAttributes$Edges$labels[ind_noplot] <- ""


# adjust node shapes
circle_ind <- which(is.element(Q$graphAttributes$Nodes$labels, c()))
Q$graphAttributes$Nodes$shape[circle_ind] <- "circle"
Q$graphAttributes$Nodes$width[circle_ind] <- 7

plot(Q)
legend("bottomleft", cats_lab[exist_min:exist_max], lwd=cats_lwd[exist_min:exist_max], box.lty=0, cex=1.2, title="Correlation")
legend("topleft", names(grps), pch=15, col=cols, box.lty=0, cex=1.2, title="Item loads strongest on factor...", horiz=T)