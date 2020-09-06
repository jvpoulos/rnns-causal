library(ggplot2)
library(latex2exp)
library(dplyr)

# Fixed dimensions: NxT=400,000
load("results/plots/stock_N_200_T_2000_numruns_10_num_treated_100_simultaneuous_1.rds")
stockn200t2000 <- df1
load("results/plots/stock_N_400_T_1000_numruns_10_num_treated_200_simultaneuous_1.rds")
stockn400t1000 <- df1
stockn600t667 <- load("results/plots/stock_N_600_T_667_numruns_10_num_treated_300_simultaneuous_1.rds")
stockn800t500 <- load("results/plots/stock_N_800_T_500_numruns_10_num_treated_400_simultaneuous_1.rds")
stockn1000t400 <- load("results/plots/stock_N_1000_T_400_numruns_10_num_treated_500_simultaneuous_1.rds")
stockn1200t333 <- load("results/plots/stock_N_1200_T_333_numruns_10_num_treated_600_simultaneuous_1.rds")
stockn1400t286 <- load("results/plots/stock_N_1400_T_286_numruns_10_num_treated_700_simultaneuous_1.rds")
stockn1600t250 <- load("results/plots/stock_N_1600_T_250_numruns_10_num_treated_800_simultaneuous_1.rds")
stockn1800t222 <- load("results/plots/stock_N_1800_T_222_numruns_10_num_treated_900_simultaneuous_1.rds")
stockn2000t200 <- load("results/plots/stock_N_2000_T_200_numruns_10_num_treated_1000_simultaneuous_1.rds")

df1 <-rbind(stockn100t1000,stockn250t400,stockn500t200,stockn800t125,stockn1000t100)
df1$x <- c(rep(100, 7), rep(250, 7), rep(500, 7), rep(800, 7), rep(1000, 7))

df1 <- df1 %>% group_by(x) %>% mutate(y = y,
                                      lb= lb,
                                      ub = ub)

breaks <- round(log(unique(df1$x)),1)
print(breaks)

stock <- ggplot(data = df1[df1$Method!="VAR",], aes(log(x), y, color = Method, shape = Method)) +
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

ggsave("results/plots/stock-sim.png", stock + theme( legend.position = "none"), width=8.5, height=11) 
ggsave("results/plots/stock-sim-slides.png", stock + ggtitle("stock waves: N x T = 15k, 25% missing") + theme(plot.title = element_text(family="serif", size=14, hjust = 0.5)))