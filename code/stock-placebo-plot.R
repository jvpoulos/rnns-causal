library(ggplot2)
library(latex2exp)
library(dplyr)

# Fixed NT: 150,000
stockn2000t75 <- readRDS("results/table-results/stock_N_2000_T_75_numruns_100_num_treated_1000_simultaneuous_1.rds") # 32528902
stockn1500t100 <- readRDS("results/table-results/stock_N_1500_T_100_numruns_100_num_treated_750_simultaneuous_1.rds") # 32496785
stockn1000t150 <- readRDS("results/table-results/stock_N_1000_T_150_numruns_100_num_treated_500_simultaneuous_1.rds") # 32587423
stockn500t300 <- readRDS("results/table-results/stock_N_500_T_300_numruns_100_num_treated_250_simultaneuous_1.rds") # 32578816
stockn250t600 <- readRDS("results/table-results/stock_N_250_T_600_numruns_100_num_treated_125_simultaneuous_1.rds")
stockn100t1500 <- readRDS("results/table-results/stock_N_100_T_1500_numruns_100_num_treated_50_simultaneuous_1.rds")

df1 <-rbind(stockn100t1500,stockn250t600,stockn500t300,stockn1000t150,stockn1500t100,stockn2000t75)
df1$x <- c(rep(100, 7), rep(250, 7), rep(500, 7), rep(1000, 7), rep(1500, 7), rep(2000, 7))

df1 <- df1 %>% group_by(x) %>% mutate(y = y,
                                      lb= lb,
                                      ub = ub)

breaks <- round(log(unique(df1$x)),1)
print(breaks)

stock <- ggplot(data = df1, aes(log(x), y, color = Method, shape = Method)) +
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
                              breaks[5],
                              breaks[6]), labels=print(breaks)) +
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
ggsave("results/plots/stock-sim-slides.png", stock + ggtitle("Stock market prices: N x T = 15k, 25% missing") + theme(plot.title = element_text(family="serif", size=14, hjust = 0.5)))