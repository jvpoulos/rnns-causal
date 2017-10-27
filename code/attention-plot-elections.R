library(plotly)
library(dplyr)
library(reshape2)
library(readr)

results.directory <-"~/Dropbox/github/rnns-causal/results/"

Range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))} # fn to normalize Attn 0 to 1

# votediff

votediff.names <- colnames(votediff)[-1]

votediff.attn <- read_csv(paste0(results.directory, "elections/votediff/attention.csv"), col_names = FALSE)

colnames(votediff.attn) <- votediff.names

rownames(votediff.attn) <- votediff.y.val$year

votediff.attn <- t(votediff.attn)

votediff.attn <- cbind(votediff.attn,colsplit(votediff.names,"[.]", c("Category","City","State")))

# clean Category

levels(votediff.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

votediff.attn.plot <- plot_ly(
  x = votediff.attn$Category, y = factor(votediff.attn$State, levels=rev(levels(votediff.attn$State))),
  z = Range01(votediff.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: revenues',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(votediff.attn.plot, file = paste0(results.directory, "plots/votediff-attn-hsa.html"))