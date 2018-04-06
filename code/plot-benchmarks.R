library(readr)
library(ggplot2)
library(wesanderson)

rnns_causal_benchmark <- read_csv(paste0(results.directory,"rnns-causal-benchmark.csv"))

rnns_causal_benchmark <-rnns_causal_benchmark[rnns_causal_benchmark$Data!="ARMA",]

# Plot

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(face="bold", size=16, vjust=1)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = "top"
                     , legend.justification = c(1,0))

# benchmarks.plot.mspe <- ggplot(rnns_causal_benchmark, aes(Data, MSPE, shape = Model, colour = Model)) + 
#   geom_jitter(width = 0.2, height = 0, size=5, alpha=0.8, stroke=2) +
#   scale_shape_manual("Model",values=c("BSTS"=3,"Encoder-decoder"=4,"LSTM"=5,"SCM"=6),
#                         labels=c("BSTS", "Encoder-decoder","LSTM","SCM")) +
#   scale_colour_manual(name="Model", values = c("BSTS" = wes_palette("Darjeeling")[4], "Encoder-decoder" = wes_palette("Darjeeling")[2], "LSTM" = wes_palette("Darjeeling")[3],"SCM" = wes_palette("Darjeeling")[5]),
#                       labels=c("BSTS", "Encoder-decoder", "LSTM","SCM")) +
#   ylab("Mean Squared Prediction Error (MSPE)") +
#   ggtitle("Counterfactual prediction error") +
#   theme.blank # I think something is wrong with MSPE calculations
# 
# ggsave(paste0(results.directory,"plots/benchmark-mspe.png"), benchmarks.plot.mspe, width=11, height=8.5)

benchmarks.plot.ape <- ggplot(rnns_causal_benchmark, aes(Data, APE, shape = Model, colour = Model)) + 
  geom_jitter(width = 0.2, height = 0, size=5, alpha=0.8, stroke=2) +
  scale_shape_manual("Model",values=c("BSTS"=3,"Encoder-decoder"=4,"LSTM"=5,"SCM"=6),
                     labels=c("BSTS", "Encoder-decoder","LSTM","SCM")) +
  scale_colour_manual(name="Model", values = c("BSTS" = wes_palette("Darjeeling")[4], "Encoder-decoder" = wes_palette("Darjeeling")[2], "LSTM" = wes_palette("Darjeeling")[3],"SCM" = wes_palette("Darjeeling")[5]),
                      labels=c("BSTS", "Encoder-decoder", "LSTM","SCM")) +
  ylab("Mean Absolute Prediction Error (MAPE)") +
  ggtitle("Counterfactual prediction error") +
  theme.blank

ggsave(paste0(results.directory,"plots/benchmark-ape.png"), benchmarks.plot.ape, width=11, height=8.5)

