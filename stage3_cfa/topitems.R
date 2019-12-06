items <- read.csv("../data/items.csv")
items$item <- gsub("RT_", "R_", items$item)
key <- read.csv("../data/key.csv", row.names=1)

ord <- apply(m5$loadings, 2, function(x) {order(abs(x), decreasing=T)})
top <- ord[1:3,]

top_items <- apply(top, c(1,2), function(x) {
  blinded <- rownames(m5$loadings)[x]
  original <- key[match(blinded, key$new), "original"]
})

for(i in 1:ncol(top_items)) {
  print(colnames(top_items)[i])
  print(items[match(top_items[,i], items$item), "text"])
  print("-------")
  
}