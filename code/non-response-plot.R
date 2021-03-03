###################################
# Summarize non-response and treatment status in education spending data #
###################################

library(ggplot2)
library(wesanderson)
library(reshape)
library(reshape2)
library(stringr)
library(grid)

# Read data
capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-none.rds"))

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
state.states <- sort(rownames(capacity.outcomes$educ.pc$M)[!rownames(capacity.outcomes$educ.pc$M) %in% pub.states])
  
# Prepare outcomes data
educ <- capacity.outcomes$educ.pc
treat <- educ$mask # NxT masked matrix 
Y.missing <- educ$M.missing # NxT # 1=observed, NA=missing/imputed
Y <- educ$M # NxT

pub.states <- pub.states[pub.states%in%rownames(Y)]

# N x T Heatmap: Nonresponse

Y.missing[is.na(Y.missing)] <- 2 # 1=observed, 2=missing
Y.missing <- Y.missing-1  # 1=missing, 0=observed

Y.missing <- Y.missing[match(c(rev(state.states),rev(pub.states)), row.names(Y.missing)),]

Y.missing.m <- melt(Y.missing)
colnames(Y.missing.m) <- c("State","Year","value")

Y.missing.m$Year <- factor(Y.missing.m$Year)

Y.missing.m$State <- as.factor(Y.missing.m$State)

year.labels <- c(colnames(Y.missing)[seq(1,ncol(Y.missing),10)])

text_high <- textGrob("Treated", gp=gpar(fontsize=9, fontface="bold"), rot=90)
text_low <- textGrob("Control", gp=gpar(fontsize=9, fontface="bold"),rot=90)

nonre.heat <- ggplot(Y.missing.m, aes(Year,State)) + geom_tile(aes(fill = value),
                                                                  colour = "white") + 
  scale_fill_gradient(low="lightblue", high="red",
                      breaks=c(0, 1),
                      limits=c(0, 1),
                      labels=rev(c("Missing/Imputed","Present"))) + 
  theme_grey(base_size=9) + labs(x="Year", y=" ") + 
  theme_bw() + 
  geom_hline(yintercept = 19.5) +
  annotation_custom(text_high,xmin=-12,xmax=-17,ymin=28,ymax=28) + 
  annotation_custom(text_low,xmin=-12,xmax=-17,ymin=10,ymax=10) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(expand = c(0, 0), breaks= year.labels, labels = year.labels) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill=str_wrap('Observations', 15)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), legend.title = element_text(size = 9, face="bold"), 
        axis.text.x = element_text(size=9),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks=element_blank())

ggsave("results/plots/missing-heatmap.png", nonre.heat, scale=1.25)

# N x T Heatmap: Treatment

treat[rownames(treat)%in%pub.states,] <- treat[rownames(treat)%in%pub.states,] +1

states.sort <- c(rev(state.states), names(sort(rowSums(treat[rownames(treat)%in%pub.states,])))) # sort by initial entry

treat <- treat[match(states.sort, row.names(treat)),]

treat.m <- melt(treat)
colnames(treat.m) <- c("State","Year","value")

treat.m$Year <- factor(treat.m$Year)

treat.m$State <- as.factor(treat.m$State)

year.labels <- c(colnames(treat)[seq(1,ncol(treat),10)])

pal <- wes_palette("Zissou1",5, type = "discrete")

treat.heat <- ggplot(treat.m, aes(Year,State)) + geom_tile(aes(fill = value),
                                                               colour = "white") + 
  scale_fill_gradientn(colours=pal[c(2,3,5)],
                      breaks=c(0,1,2),
                      limits=c(0,2),
                      labels=rev(c("Treated (post)", "Treated (pre)","Control"))) + 
  theme_grey(base_size=9) + labs(x="Year", y=" ") + 
  theme_bw() + 
  geom_hline(yintercept = 19.5) +
  annotation_custom(text_high,xmin=-12,xmax=-17,ymin=28,ymax=28) + 
  annotation_custom(text_low,xmin=-12,xmax=-17,ymin=10,ymax=10) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(expand = c(0, 0), breaks= year.labels, labels = year.labels) +
  scale_y_discrete(expand = c(0, 0)) +
  geom_vline(xintercept = "1869",linetype="dashed") +
  labs(fill=str_wrap('Treatment status', 25)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), legend.title = element_text(size = 9, face="bold"), 
        axis.text.x = element_text(size=9),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks=element_blank()) 

ggsave("results/plots/treat-heatmap.png", treat.heat, scale=1.25)