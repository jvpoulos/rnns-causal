library(plotly)
library(dplyr)
library(reshape2)
library(readr)

results.directory <-"~/Dropbox/github/rnns-causal/results/"

#votediff.attn  <- read_csv(paste0(results.directory, "elections/sim/encoded.csv"), col_names = FALSE)
votediff.attn  <- read_csv(paste0(results.directory, "elections/votediff/encoded.csv"), col_names = FALSE)

votediff.attn <- t(votediff.attn)

# Plot

f2 <- list(
  family = "Old Standard TT, serif",
  size = 10,
  color = "black"
)

a <- list(
  title = "Encoder hidden units",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  ticks= ""
)

b <- list(
  title = "t'",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = TRUE,
  ticks= "",
  showgrid = FALSE
)

votediff.attn.plot <- plot_ly(
 # x = 48:52, y = rownames(votediff.attn),
  x = votediff.y.test$year, y = rownames(votediff.attn),
  z = as.matrix(votediff.attn), type = "heatmap", name="Activations",
  height = 800, width=600
) %>%
  layout(title = "Encoder-decoder",
         yaxis = a,
         xaxis = b,
         margin = list(l = 150, r = 0, b = 50, t = 50, pad = 2)) %>% 
  colorbar(title = "Encoder activations") 
htmlwidgets::saveWidget(votediff.attn.plot, file = paste0(results.directory, "plots/elections-activations.html"))
#htmlwidgets::saveWidget(votediff.attn.plot, file = paste0(results.directory, "plots/sim-activations.html"))