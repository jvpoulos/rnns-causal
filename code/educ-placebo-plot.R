library(ggplot2)
library(latex2exp)
library(dplyr)

load("results/plots/educ_pc_N_16_T_148_numruns_100_num_treated_8_simultaneuous_1.rds") # 100 runs

df1 <- df1 %>% group_by(x) %>% mutate(y = y,
                                      lb= y-1.96*se,
                                      ub = y+1.96*se)

educ <- ggplot(data = df1[df1$y<2 & df1$Method!=
                            "SCM-ENT",], aes(x, y, color = Method, shape = Method)) +
  geom_point(size = 5, position=position_dodge(width=0.1)) +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.1)) +
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

ggsave("results/plots/educ-sim.png", educ + theme( legend.position = "none"), scale=1.25)
ggsave("results/plots/educ-sim-slides.png", educ + ggtitle("Education spending") + theme(plot.title = element_text(family="serif", size=16, hjust = 0.5)), scale=1.25)

# 
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_color_hue(8)