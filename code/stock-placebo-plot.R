library(ggplot2)
library(latex2exp)
library(dplyr)

# Fixed dimensions
#df1<- readRDS("results/plots/stock-placebo-results-fixed.rds")

n10 <- readRDS("results/plots/stock_fixed_N_10_T_980_numruns_20_num_treated_5_simultaneuous_1.rds")
n50 <- readRDS("results/plots/stock_fixed_N_50_T_196_numruns_20_num_treated_25_simultaneuous_1.rds")
n100 <- readRDS("results/plots/stock_fixed_N_100_T_98_numruns_20_num_treated_50_simultaneuous_1.rds")
n200 <- readRDS("results/plots/stock_fixed_N_200_T_49_numruns_20_num_treated_100_simultaneuous_1.rds")

df1 <-rbind(n10,n50,n100,n200)

df1 <- df1 %>% group_by(N) %>% mutate(y = y,
                                      lb= lb,
                                      ub = ub)

stock <- ggplot(data = df1, aes(log(N), y, color = Method, shape = Method)) +
  geom_point(size = 5, position=position_dodge(width=0.3)) +
  geom_line(position=position_dodge(width=0.3)) +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1:6,8)) +
  theme_bw() +
  xlab(TeX('log(N)')) +
  ylab("Average RMSE") +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 12)) +
  theme(legend.title=element_text(family="serif", size = 12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

ggsave("results/plots/stock-sim-fixed.png", stock + theme( legend.position = "none"), width=8.5, height=11)
ggsave("results/plots/stock-sim-fixed-slides.png", stock + ggtitle("Stock market, fixed size") + theme(plot.title = element_text(family="serif", size=16, hjust = 0.5)))

# Increasing T
#df1<- readRDS("results/plots/stock-placebo-results.rds")

t250 <- readRDS("results/plots/stock_N_100_T_250_numruns_20_num_treated_50_simultaneuous_1.rds")
t500 <- readRDS("results/plots/stock_N_100_T_500_numruns_20_num_treated_50_simultaneuous_1.rds")
t1000 <- readRDS("results/plots/stock_N_100_T_1000_numruns_20_num_treated_50_simultaneuous_1.rds")
t1500 <- readRDS("results/plots/stock_N_100_T_1500_numruns_20_num_treated_50_simultaneuous_1.rds")

df1 <-rbind(t250,t500,t1000,t1500)

df1 <- df1 %>% group_by(N) %>% mutate(y = y,
                                      lb= lb,
                                      ub = ub)

stock <- ggplot(data = df1, aes(log(N*T), y, color = Method, shape = Method)) +
  geom_point(size = 5, position=position_dodge(width=0.3)) +
  geom_line(position=position_dodge(width=0.3)) +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1:6,8)) +
  theme_bw() +
  xlab(TeX('log(N)')) +
  ylab("Average RMSE") +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 12)) +
  theme(legend.title=element_text(family="serif", size = 12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

ggsave("results/plots/stock-sim.png", stock + theme( legend.position = "none"), width=8.5, height=11)
ggsave("results/plots/stock-sim-slides.png", stock + ggtitle("Stock market, increasing T") + theme(plot.title = element_text(family="serif", size=16, hjust = 0.5)))