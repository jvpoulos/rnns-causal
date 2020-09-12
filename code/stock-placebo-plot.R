library(ggplot2)
library(latex2exp)
library(dplyr)

sim.label <- c("stag","sim")
for(sim in c(0,1)){
  # Fixed dimensions: NxT=400,000
  load(paste0("results/plots/stock_N_200_T_2000_numruns_10_num_treated_100_simultaneuous_",sim,".rds"))
  stockn200t2000 <- df1
  load(paste0("results/plots/stock_N_400_T_1000_numruns_10_num_treated_200_simultaneuous_",sim,".rds"))
  stockn400t1000 <- df1
  load(paste0("results/plots/stock_N_600_T_667_numruns_10_num_treated_300_simultaneuous_",sim,".rds"))
  stockn600t667 <- df1
  load(paste0("results/plots/stock_N_800_T_500_numruns_10_num_treated_400_simultaneuous_",sim,".rds"))
  stockn800t500 <- df1
  load(paste0("results/plots/stock_N_1000_T_400_numruns_10_num_treated_500_simultaneuous_",sim,".rds"))
  stockn1000t400 <- df1
  load(paste0("results/plots/stock_N_1200_T_333_numruns_10_num_treated_600_simultaneuous_",sim,".rds"))
  stockn1200t333 <- df1
  load(paste0("results/plots/stock_N_1400_T_286_numruns_10_num_treated_700_simultaneuous_",sim,".rds"))
  stockn1400t286 <- df1
  load(paste0("results/plots/stock_N_1600_T_250_numruns_10_num_treated_800_simultaneuous_",sim,".rds"))
  stockn1600t250 <- df1
  load(paste0("results/plots/stock_N_1800_T_222_numruns_10_num_treated_900_simultaneuous_",sim,".rds"))
  stockn1800t222 <- df1
  load(paste0("results/plots/stock_N_2000_T_200_numruns_10_num_treated_1000_simultaneuous_",sim,".rds"))
  stockn2000t200 <- df1
  
  df1 <-rbind(stockn200t2000, stockn400t1000, stockn600t667, stockn800t500, stockn1000t400, stockn1200t333, stockn1400t286, stockn1600t250, stockn1800t222, stockn2000t200)
  df1$x <- c(rep(200, 8), rep(400, 8), rep(600, 8), rep(800, 8), rep(1000, 8), rep(1200, 8), rep(1400, 8), rep(1600, 8), rep(1800, 8), rep(2000, 8))
  
  df1 <- df1 %>% group_by(x) %>% mutate(y = y,
                                        lb= y-1.96*se,
                                        ub =y+1.96*se)
  
  df1 <- df1[df1$Method %in% c("Encoder-decoder","SCM-EN","VAR"),]
  breaks <- unique(round(log(unique(df1$x))))
  
  stock <- ggplot(data = df1, aes(log(x), y, color = Method, shape = Method)) +
    geom_jitter(size = 3, position=position_dodge(width=0.25)) +
    geom_line(position=position_dodge(width=0.25), linetype = "dashed") +
    geom_errorbar(
      aes(ymin = lb, ymax = ub),
      width = 0.1,
      linetype = "solid",
      position=position_dodge(width=0.25)) +
    scale_shape_manual(values=c(2,6,8)) +
    scale_color_manual(values=c("#F8766D","#CD9600","#7CAE00","#00BE67","#00BFC4","#00A9FF","#C77CFF","#FF61CC")[c(2,6,8)]) +
    scale_x_continuous(breaks=breaks, labels=breaks) +
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
  
  ggsave(paste0("results/plots/stock-",sim.label[sim+1],".png"), stock + theme( legend.position = "none"), scale=1.25) 
  ggsave(paste0("results/plots/stock-",sim.label[sim+1],"-slides.png"), stock + ggtitle("stock waves: N x T = 15k, 25% missing") + theme(plot.title = element_text(family="serif", size=14, hjust = 0.5)), scale=1.25)
}