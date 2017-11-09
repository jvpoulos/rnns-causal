library(plotly)
library(dplyr)
library(reshape2)
library(readr)

results.directory <-"~/Dropbox/github/rnns-causal/results/"

# votediff

votediff.names <- colnames(votediff.x.train)[-1]

votediff.attn  <- read_csv(paste0(results.directory, "elections/votediff/attention.csv"), col_names = FALSE)

colnames(votediff.attn) <- votediff.names 

rownames(votediff.attn) <- votediff.x.train$year

votediff.attn <- t(votediff.attn)

votediff.attn <- cbind(votediff.attn,colsplit(votediff.names,"[.]", c("Category","City","State")))

# Reshape

votediff.attn$id <- paste(votediff.attn$City,votediff.attn$State,sep=", ")

votediff.attn$id <- gsub("([a-z])([A-Z])", "\\1 \\2", votediff.attn$id) # put spaces back into labels


# Fix ID labels
fg.ads$city <- gsub("([a-z])([A-Z])", "\\1 \\2", fg.ads$city) # put spaces back into labels

exp.cities <- sort(unique(paste(fg.ads$city[fg.ads$treat==0],fg.ads$state[fg.ads$treat==0],sep=", "))[-1])

#votediff.attn$id[votediff.attn$id %in% exp.cities] <- paste0('<b>', votediff.attn$id[votediff.attn$id %in% exp.cities] , '</b>') # P&G cities are boldfaced

# Subset to experimental cities

votediff.attn <- votediff.attn[votediff.attn$id %in% exp.cities,]


# Plot

f2 <- list(
  family = "Old Standard TT, serif",
  size = 10,
  color = "black"
)

a <- list(
 # title = "AXIS TITLE",
 # titlefont = f2,
  showticklabels = TRUE,
  tickangle = 0.35,
  tickfont = f2,
  autorange = "reversed"
)

votediff.attn.plot <- plot_ly(
  x = votediff.x.train$year, y = votediff.attn$id,
  z = as.matrix(votediff.attn[1:47]), type = "heatmap", name="Attention",
  height = 800, width=600
) %>%
  layout(title = '',
         yaxis = a,
       #  xaxis = list(title = 'Time-step'),
      #   yaxis = list(title = 'Predictor'),
         margin = list(l = 100, r = 50, b = 50, t = 50, pad = 2)) %>% 
  colorbar(title = "Attention") 
htmlwidgets::saveWidget(votediff.attn.plot, file = paste0(results.directory, "plots/votediff-attn.html"))