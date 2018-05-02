library(readr)
library(ggplot2)
library(wesanderson)

# Get high and low MSPEs (no time dimension)

CollectMSPE <-function(n.pre, forecast, true, x){
  library(ftsa)
  return(error(forecast=as.matrix(forecast)[,x], true=as.matrix(true[(n.pre+1):nrow(true),])[,x], method = "mse"))
}

# lstm
basque.lstm.mses <- sapply(1:length(basque.controls), CollectMSPE, 
                         n.pre=14, 
                         forecast=as.matrix(basque.lstm.pred.control),
                         true=basque.x)

california.lstm.mses <- sapply(1:length(california.controls), CollectMSPE, 
                             n.pre=19, 
                             forecast=as.matrix(california.lstm.pred.control),
                             true=california.x)

germany.lstm.mses <- sapply(1:length(germany.controls), CollectMSPE, 
                          n.pre=30, 
                          forecast=as.matrix(germany.lstm.pred.control),
                          true=germany.x)

# encoder.decoder
basque.encoder.decoder.mses <- sapply(1:length(basque.controls), CollectMSPE, 
                           n.pre=14, 
                           forecast=as.matrix(basque.encoder.decoder.pred.control),
                           true=basque.x)

california.encoder.decoder.mses <- sapply(1:length(california.controls), CollectMSPE, 
                               n.pre=19, 
                               forecast=as.matrix(california.encoder.decoder.pred.control),
                               true=california.x)

germany.encoder.decoder.mses <- sapply(1:length(germany.controls), CollectMSPE, 
                            n.pre=30, 
                            forecast=as.matrix(germany.encoder.decoder.pred.control),
                            true=germany.x)

# synth
basque.synth.mses <- sapply(1:length(basque.controls), CollectMSPE, 
                            n.pre=14, 
                            forecast=synth.basque.controls.preds[(14+1):nrow(synth.basque.controls.preds),],
                            true=basque.x)

california.synth.mses <- sapply(1:length(california.controls), CollectMSPE, 
                                n.pre=19, 
                                forecast=synth.california.controls.preds[(19+1):nrow(synth.california.controls.preds),],
                                true=california.x)

germany.synth.mses <- sapply(1:length(germany.controls), CollectMSPE, 
                             n.pre=30, 
                             forecast=synth.germany.controls.preds[(30+1):nrow(synth.germany.controls.preds),],
                             true=germany.x)

# Compile MSPEs
gans.causal.benchmark <- data.frame("Model"=rep(c("LSTM","Encoder-decoder","SCM"),each=3),
                                    "Data"=rep(c("Basque", "California","Germany"),3),
                                    "MSPE"= c(basque.lstm.mse, california.lstm.mse, germany.lstm.mse,
                                              basque.encoder.decoder.mse, california.encoder.decoder.mse, germany.encoder.decoder.mse,
                                              basque.synth.mse, california.synth.mse, germany.synth.mse),
                                    "MSPE.hi"= c(basque.lstm.mse+1.96*sd(basque.lstm.mses),california.lstm.mse+1.96*sd(california.lstm.mses),germany.lstm.mse+1.96*sd(germany.lstm.mses),
                                                 basque.encoder.decoder.mse+1.96*sd(basque.encoder.decoder.mses),california.encoder.decoder.mse+1.96*sd(california.encoder.decoder.mses),germany.encoder.decoder.mse+1.96*sd(germany.encoder.decoder.mses),
                                                 basque.synth.mse+1.96*sd(basque.synth.mses),california.synth.mse+1.96*sd(california.synth.mses),germany.synth.mse+1.96*sd(germany.synth.mses)),
                                    "MSPE.lo"= c(basque.lstm.mse-1.96*sd(basque.lstm.mses),california.lstm.mse-1.96*sd(california.lstm.mses),germany.lstm.mse-1.96*sd(germany.lstm.mses),
                                                 basque.encoder.decoder.mse-1.96*sd(basque.encoder.decoder.mses),california.encoder.decoder.mse-1.96*sd(california.encoder.decoder.mses),germany.encoder.decoder.mse-1.96*sd(germany.encoder.decoder.mses),
                                                 basque.synth.mse-1.96*sd(basque.synth.mses),california.synth.mse-1.96*sd(california.synth.mses),germany.synth.mse-1.96*sd(germany.synth.mses))
)

# Plot

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(face="bold", size=16, vjust=1)
                     , axis.ticks.x=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = "top"
                     , legend.justification = c(1,0)
                     , legend.background=element_blank())

benchmarks.plot.mspe <- ggplot(gans.causal.benchmark, aes(Data, MSPE, shape = Model, colour = Model)) + 
  geom_pointrange(size=1.2, alpha=0.9,position=position_dodge(width=0.5), aes(ymin=MSPE.lo, ymax=MSPE.hi)) +
  scale_shape_manual("Model",values=c("Encoder-decoder"=4,"LSTM"=5,"SCM"=6),
                        labels=c("Encoder-decoder","LSTM","SCM")) +
  scale_colour_manual(name="Model", values = c("Encoder-decoder" = wes_palette("Darjeeling")[2],"LSTM" = wes_palette("Darjeeling")[3],"SCM" = wes_palette("Darjeeling")[5]),
                      labels=c("Encoder-decoder","LSTM","SCM")) +
  ylab("Mean Squared Prediction Error (MSPE)") +
#  ggtitle("Prediction error (placebo tests)") +
  theme.blank + theme(legend.key=element_blank())

ggsave(paste0(results.directory,"plots/benchmark-mspe.png"), benchmarks.plot.mspe, width=11, height=8.5)

# Compile FPRs
rnns.causal.fpr <- data.frame("Model"=rep(c("LSTM","Encoder-decoder","SCM"),each=3),
                                    "Data"=rep(c("Basque", "California","Germany"),3),
                                    "FPR"= c(lstm.basque.fpr, lstm.california.fpr, lstm.germany.fpr,
                                             encoder.decoder.basque.fpr, encoder.decoder.california.fpr, encoder.decoder.germany.fpr,
                                              synth.basque.fpr, synth.california.fpr, synth.germany.fpr))

benchmarks.plot.fpr <- ggplot(rnns.causal.fpr, aes(Data, FPR, shape = Model, colour = Model)) + 
  geom_point(size=5, stroke=2,position=position_dodge(width=0.5)) +
  scale_shape_manual("Model",values=c("Encoder-decoder"=4,"LSTM"=5,"SCM"=6),
                     labels=c("Encoder-decoder","LSTM","SCM")) +
  scale_colour_manual(name="Model", values = c("Encoder-decoder" = wes_palette("Darjeeling")[2],"LSTM" = wes_palette("Darjeeling")[3],"SCM" = wes_palette("Darjeeling")[5]),
                      labels=c("Encoder-decoder","LSTM","SCM")) +
  ylab("False Positive Rate (FPR)") +
#  ggtitle("FPR (placebo tests)") +
  theme.blank + theme(legend.key=element_blank())

ggsave(paste0(results.directory,"plots/benchmark-fpr.png"), benchmarks.plot.fpr, width=11, height=8.5)