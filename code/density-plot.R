## Plot public education spending by pre/post and treatment status

require(ggplot2)
require(reshape2)

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
state.land.states <- state.abb[!state.abb %in% pub.states] # 20 state land states

df <- data.frame(t(capacity.outcomes$educ.pc$M))

df$id <- rownames(df)

df.m <- melt(df, "id")
df.m$time <- factor(ifelse(df.m$id<1869,"Pre-period","Post-period"))
levels(df.m$time ) <- rev( levels(df.m$time ))
df.m$status <- factor(ifelse(df.m$variable%in%pub.states,"Treated","Control"))

#df.m <- df.m[df.m$time!="Post-period" | df.m$status!="Treated",] # Censor Y_t

p <- ggplot(data = df.m, aes(x=value)) + geom_density(aes(fill=status), alpha = 0.4) +
  facet_wrap( ~ time) + 
  scale_fill_brewer(palette = "Set1") +
  ylab("Density") + 
  xlab("Log per-capita education spending") +
scale_fill_manual(values = c("red","blue"), labels= c("Control", "Treated"), name="Treatment status") +
  theme(legend.justification = c(0.95, 0.95), legend.position = c(0.95, 0.95),legend.background = element_rect(colour = "black"))

ggsave(paste0(results.directory,"plots/educ-dens.png"), p, width=8.5, height=11)

# two-sample Kolmogorov-Smirnov test
X.c <- df[colnames(df) %in% state.land.states][as.numeric(rownames(df))<1869,]
X.t <- df[colnames(df) %in% pub.states][as.numeric(rownames(df))<1869,]

t.test(X.t,X.c,alternative="two.sided", conf.level = 0.95)