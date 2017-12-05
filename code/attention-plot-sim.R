library(plotly)
library(dplyr)
library(reshape2)
library(readr)

results.directory <-"~/Dropbox/github/rnns-causal/results/"

# votediff

votediff.attn  <- read_csv(paste0(results.directory, "elections/votediff-sim/attention.csv"), col_names = FALSE)

votediff.attn <- t(votediff.attn)

# Plot

f2 <- list(
  family = "Old Standard TT, serif",
  size = 10,
  color = "black"
)

a <- list(
  title = "Predictor",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  ticks= ""
)

b <- list(
  title = "Time-step",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = TRUE,
  ticks= "",
  showgrid = FALSE
)

votediff.attn.plot <- plot_ly(
  x = votediff.x.train$year[15:47], y = rownames(votediff.attn),
 z = as.matrix(votediff.attn), type = "heatmap", name="Attention",
 height = 800, width=600
) %>%
  layout(xaxis = b,
         yaxis = a,
         margin = list(l = 100, r = 50, b = 50, t = 50, pad = 2)) %>% 
  colorbar(title = "Attention") 
htmlwidgets::saveWidget(votediff.attn.plot, file = paste0(results.directory, "plots/sim-attn.html"))