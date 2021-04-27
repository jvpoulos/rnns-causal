library(ggplot2)
library(latex2exp)
library(dplyr)

sim.label <- c("stag")
for(sim in c(0)){
  
  load(paste0("results/rbf_N_1000_T_500_numruns_100_num_treated_500_simultaneuous_",sim,".rds"))
  
  df1 <- df1 %>% group_by(x) %>% mutate(y = y,
                                        lb= y-1.96*se,
                                        ub = y+1.96*se)
  
  df1 <- df1
  df1$Method <- droplevels(df1$Method)
  
  rfb <- ggplot(data = df1, aes(x, y, color = Method, shape = Method)) +
    geom_point(size = 4, position=position_dodge(width=0.2)) +
    geom_line(position=position_dodge(width=0.2), linetype = "dashed") +
    geom_errorbar(
      aes(ymin = lb, ymax = ub),
      width = 0.3,
      linetype = "solid",
      position=position_dodge(width=0.2)) +
    scale_shape_manual(values=c(1:6,8)) +
    scale_x_continuous(breaks=c(unique(df1$x)[1],unique(df1$x)[2],unique(df1$x)[3]), labels=c("0.25","0.50","0.75")) +
    theme_bw() +
    xlab(TeX('$T_0/T$')) +
    ylab("Average RMSE") +
    theme(axis.title=element_text(family="serif", size=16)) +
    theme(axis.text=element_text(family="serif", size=14)) +
    theme(legend.text=element_text(family="serif", size = 12)) +
    theme(legend.title=element_text(family="serif", size = 12)) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
    theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background
  
  ggsave(paste0("results/plots/rfb-",sim.label[sim+1],".png"), rfb + theme( legend.position = "none"), scale=1.25)
  ggsave(paste0("results/plots/rfb-",sim.label[sim+1],"-slides.png"), rfb + ggtitle("RBF") + theme(plot.title = element_text(family="serif", size=16, hjust = 0.5)), scale=1.25)
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_color_hue(7)