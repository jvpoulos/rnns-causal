library(ggplot2)
library(latex2exp)
library(dplyr)

load(paste0(results.directory, "plots/educ_pc_N_16_T_156_numruns_10_num_treated_8_simultaneuous_1.rds"))

df1 <- df1 %>% group_by(x) %>% mutate(y = log(y),
                                      lb= log(lb),
                                      ub = log(ub))

educ <- ggplot(data = df1, aes(x, y, color = Method, shape = Method)) +
  geom_point(size = 2, position=position_dodge(width=0.1)) +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.1)) +
  scale_shape_manual(values=c(1:8)) +
  scale_x_continuous(breaks=c(0.25,0.5,0.75,1), labels=c("0.25","0.5","0.75","1")) +
  theme_bw() +
  xlab(TeX('$T_0/T$')) +
  ylab("Average RMSE (ln)") +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 12)) +
  theme(legend.title=element_text(family="serif", size = 12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme( legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

ggsave(paste0(results.directory, "plots/educ-sim.png"), educ, width=8.5, height=11)