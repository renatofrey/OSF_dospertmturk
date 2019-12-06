library(xtable)

items <- read.csv("../data/items.csv")
items$item <- gsub("RT_", "", items$item)
colnames(items) <- c("Item", "Activity")

# generate a latex table
xtab <- xtable(items,
               caption="List of items used in the DOSPERT",
               label="tab:items",
               type="latex"
)

# add the following before begin{tabular}
# \renewcommand{\arraystretch}{0.8} % Tighter

xtab <- print(xtab,
              table.placement = "t",
              caption.placement = "top",
              include.rownames = F,
              floating.environment="table",
              size = "small",
              file="")

#str_note <- "Note. DF = degrees of freedom, RMSEA = root mean square error of approximation, SRMR = standardized root mean square residual, CFI = comparative fix index, TLI = Tucker-Lewis index, BIC = Bayesian Information Criterion, AIC = Akaike information criterion, R\\textsuperscript{2} = Explained variance (in total, R = by the general factor, Fs = by the specific factors)."
str_note <- ""

xtab <- gsub("\\end{table", paste("\n\\raggedright\n{\\footnotesize ", str_note, "}\n\n\\end{table", sep=""), xtab, fixed=T)

cat(xtab, file=paste("../output/items.tex", sep=""))