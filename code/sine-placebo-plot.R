library(ggplot2)
library(latex2exp)
library(dplyr)

# Fixed NT: 100,000
sinen1000t100 <- readRDS("results/plots/sine_N_1000_T_100_numruns_100_num_treated_500_simultaneuous_1.rds")
sinen800t125 <- readRDS("results/plots/sine_N_800_T_125_numruns_100_num_treated_400_simultaneuous_1.rds")
sinen500t200 <- readRDS("results/plots/sine_N_500_T_200_numruns_100_num_treated_250_simultaneuous_1.rds")
sinen250t400 <- readRDS("results/plots/sine_N_250_T_400_numruns_100_num_treated_125_simultaneuous_1.rds")
sinen100t1000 <- readRDS("results/plots/sine_N_100_T_1000_numruns_100_num_treated_50_simultaneuous_1.rds")

df1 <-rbind(sinen100t1000,sinen250t400,sinen500t200,sinen800t125,sinen1000t100)
df1$x <- c(rep(100, 7), rep(250, 7), rep(500, 7), rep(800, 7), rep(1000, 7))

df1 <- df1 %>% group_by(x) %>% mutate(y = y,
                                      lb= lb,
                                      ub = ub)

breaks <- round(log(unique(df1$x)),1)
print(breaks)

sine <- ggplot(data = df1[df1$Method!="VAR",], aes(log(x), y, color = Method, shape = Method)) +
  geom_point(size = 5, position=position_dodge(width=0.8)) +
  geom_line(position=position_dodge(width=0.8)) +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.8,
    linetype = "solid",
    position=position_dodge(width=0.8)) +
  scale_shape_manual(values=c(1:6,8)) +
  scale_x_continuous(breaks=c(breaks[1],
                              breaks[2],
                              breaks[3],
                              breaks[4],
                              breaks[5]), labels=print(breaks)) +
  theme_bw() +
  xlab(TeX('log(N)')) +
  ylab("Average RMSE") +
  theme(axis.title=element_text(family="serif", size=14)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 12)) +
  theme(legend.title=element_text(family="serif", size = 12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

ggsave("results/plots/sine-sim.png", sine + theme( legend.position = "none"), width=8.5, height=11) 
ggsave("results/plots/sine-sim-slides.png", sine + ggtitle("Sine waves: N x T = 15k, 25% missing") + theme(plot.title = element_text(family="serif", size=14, hjust = 0.5)))