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

levels(votediff.attn$Category) <- c("Vote margin")

# Reshape

votediff.attn <- reshape(votediff.attn[!colnames(votediff.attn)%in% c("Category"),], direction="long", varying=list(names(votediff.attn)[1:3]), v.names="value", 
          idvar=c("City","State"), timevar="year", times=2002:2004)

votediff.attn$id <- factor(paste(votediff.attn$City,votediff.attn$State,sep=", "))

# Normalize attn and subset

votediff.attn$value <- Range01(votediff.attn$value)

# Subset to experimental controls
votediff.attn <- votediff.attn[votediff.attn$id %in% unique(paste(fg.ads$city[fg.ads$treat==0],fg.ads$state[fg.ads$treat==0],sep=", "))[-1],]

votediff.attn$id <- droplevels(votediff.attn$id )

# Plot

votediff.attn.plot <- plot_ly(
  x = factor(votediff.attn$year), y = factor(votediff.attn$id, levels=rev(levels(votediff.attn$id))),
  z = votediff.attn$value, type = "heatmap", name="Attention",
  height = 800, width = 600
) %>%
  layout(title = '',
         xaxis = list(title = 'Time-step'),
         yaxis = list(title = 'Predictor'),
         margin = list(l = 200, r = 50, b = 50, t = 50, pad = 2)) %>% 
  colorbar(title = "Attention") 
htmlwidgets::saveWidget(votediff.attn.plot, file = paste0(results.directory, "plots/votediff-attn.html"))