library(ggplot2)
library(latex2exp)

df1<- readRDS("results/encoder-decoder/sine/sine-placebo-results-aug.rds")

sine.plot <- ggplot(data = df1, aes(log(N), y, color = Method, shape = Method)) +
  geom_point(size = 3, position=position_dodge(width=0.3)) +
  geom_line(position=position_dodge(width=0.3)) +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1:7)) +
  theme_bw() +
  xlab(TeX('log(N)')) +
  ylab("Average RMSE") +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 12)) +
  theme(legend.title=element_text(family="serif", size = 12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
 # theme( legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

ggsave("results/plots/sine-sim-aug.png", sine.plot, width=8.5, height=11)