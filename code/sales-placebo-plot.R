library(ggplot2)
library(latex2exp)
library(dplyr)

sim.label <- c("stag","sim")
for(sim in c(0,1)){
  # Fixed dimensions: NxT=400,000
  load(paste0("results/plots/sales_N_190_T_1900_numruns_10_num_treated_100_simultaneuous_",sim,".rds"))
  salesn200t2000 <- df1
  load(paste0("results/plots/sales_N_380_T_950_numruns_10_num_treated_200_simultaneuous_",sim,".rds"))
  salesn400t1000 <- df1
  load(paste0("results/plots/sales_N_570_T_633_numruns_10_num_treated_300_simultaneuous_",sim,".rds"))
  salesn600t667 <- df1
  load(paste0("results/plots/sales_N_760_T_475_numruns_10_num_treated_400_simultaneuous_",sim,".rds"))
  salesn800t500 <- df1
  load(paste0("results/plots/sales_N_950_T_380_numruns_10_num_treated_500_simultaneuous_",sim,".rds"))
  salesn1000t400 <- df1
  load(paste0("results/plots/sales_N_1140_T_317_numruns_10_num_treated_600_simultaneuous_",sim,".rds"))
  salesn1200t333 <- df1
  load(paste0("results/plots/sales_N_1330_T_271_numruns_10_num_treated_700_simultaneuous_",sim,".rds"))
  salesn1400t286 <- df1
  load(paste0("results/plots/sales_N_1520_T_238_numruns_10_num_treated_800_simultaneuous_",sim,".rds"))
  salesn1600t250 <- df1
  load(paste0("results/plots/sales_N_1710_T_211_numruns_10_num_treated_900_simultaneuous_",sim,".rds"))
  salesn1800t222 <- df1
  load(paste0("results/plots/sales_N_1900_T_190_numruns_10_num_treated_1000_simultaneuous_",sim,".rds"))
  salesn2000t200 <- df1
  
  df1 <-rbind(salesn200t2000, salesn400t1000, salesn600t667, salesn800t500, salesn1000t400, salesn1200t333, salesn1400t286, salesn1600t250, salesn1800t222, salesn2000t200)
  df1$x <- c(rep(190, 8), rep(380, 8), rep(570, 8), rep(760, 8), rep(950, 8), rep(1140, 8), rep(1330, 8), rep(1520, 8), rep(1710, 8), rep(1900, 8))
  
  df1 <- df1 %>% group_by(x) %>% mutate(y = y,
                                        lb= y-1.96*se,
                                        ub =y+1.96*se)
  
  df1 <- df1[df1$Method %in% c("Encoder-decoder","SCM-EN","VAR"),]
  breaks <- unique(round(log(unique(df1$x))))
  
  sales <- ggplot(data = df1, aes(log(x), y, color = Method, shape = Method)) +
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
  
  ggsave(paste0("results/plots/sales-",sim.label[sim+1],".png"), sales + theme( legend.position = "none"), scale=1.25) 
  ggsave(paste0("results/plots/sales-",sim.label[sim+1],"-slides.png"), sales + ggtitle("Sales data N x T = 15k, 25% missing") + theme(plot.title = element_text(family="serif", size=14, hjust = 0.5)), scale=1.25)
}