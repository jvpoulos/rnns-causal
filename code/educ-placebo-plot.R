library(ggplot2)
library(latex2exp)
library(dplyr)

sim.label <- c("stag","sim")
for(sim in c(0,1)){
  
  load(paste0("results/plots/educ_pc_N_16_T_148_numruns_100_num_treated_8_simultaneuous_",sim,".rds"))
  
  df1 <- df1 %>% group_by(x) %>% mutate(y = y,
                                        lb= y-1.96*se,
                                        ub = y+1.96*se)
  
  df1 <- df1
  df1$Method <- droplevels(df1$Method)
  
  educ <- ggplot(data = df1, aes(x, y, color = Method, shape = Method)) +
    geom_point(size = 3, position=position_dodge(width=0.25)) +
    geom_errorbar(
      aes(ymin = lb, ymax = ub),
      width = 0.3,
      linetype = "solid",
      position=position_dodge(width=0.25)) +
    scale_shape_manual(values=c(1:6,0,8)) +
    scale_x_continuous(breaks=c(unique(df1$x)[1],unique(df1$x)[2],unique(df1$x)[3]), labels=c("0.25","0.50","0.75")) +
    scale_y_continuous(breaks=seq(1,2.25,0.25), labels=c("1.00", "1.25", "1.50", "1.75", "2.00", "2.25")) +
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
  
  if(sim==1) {
    educ <- educ +coord_cartesian(ylim=c(1,2.50))
  } else{
    educ <- educ +coord_cartesian(ylim=c(1,2.25))
  }
  
  ggsave(paste0("results/plots/educ-",sim.label[sim+1],".png"), educ + theme( legend.position = "none"), scale=1.25)
  ggsave(paste0("results/plots/educ-",sim.label[sim+1],"-slides.png"), educ + ggtitle("Education spending") + theme(plot.title = element_text(family="serif", size=16, hjust = 0.5)), scale=1.25)
}

# 
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_color_hue(8)