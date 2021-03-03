###################################
# Plot autocorrelation function for the placebo test datasets #
###################################

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)

ggacf <- function(x, ci=0.95, type="correlation", xlab="Lag", ylab=NULL,
                  ylim=NULL, main=NULL, ci.col="blue", lag.max=NULL) {
  
  x <- as.data.frame(x)
  
  x.acf <- acf(x, plot=F, lag.max=lag.max, type=type)
  
  ci.line <- qnorm((1 - ci) / 2) / sqrt(x.acf$n.used)
  
  d.acf <- data.frame(lag=x.acf$lag, acf=x.acf$acf)
  
  g <- ggplot(d.acf, aes(x=lag, y=acf)) +
    geom_hline(yintercept=0) +
    geom_segment(aes(xend=lag, yend=0)) +
    geom_hline(yintercept=ci.line, color=ci.col, linetype="dashed") +
    geom_hline(yintercept=-ci.line, color=ci.col, linetype="dashed") +
    theme_bw() +
    xlab(xlab) +
    ggtitle(ifelse(is.null(main), "", main)) + theme(plot.title = element_text(hjust = 0.5)) +
    if (is.null(ylab))
      ylab(ifelse(type=="partial", "PACF", "ACF"))
  else
    ylab(ylab)
  
  g + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            axis.text.x = element_text(size=9),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            axis.ticks=element_blank())
}


# Education spending data

capacity.outcomes <- readRDS("data/capacity-outcomes-locf.rds")

# Stock market data

stock.market <- t(read.csv('data/returns_no_missing.csv',header=F)) # N X T

# Sine waves

Y.train <-  read.csv('data/sine_train_real.csv',header=F)
Y.val <- read.csv('data/sine_val_real.csv',header=F) 
Y.test <- read.csv('data/sine_test_real.csv',header=F)

sine.waves <- data.matrix(rbind(Y.train,Y.val,Y.test)) # N X T
colnames(sine.waves) <- 1:ncol(sine.waves)
rownames(sine.waves) <- 1:nrow(sine.waves)

# GP data

Y.train <-  read.csv('data/gp_rbf_train_real.csv',header=F)
Y.val <- read.csv('data/gp_rbf_val_real.csv',header=F) 
Y.test <- read.csv('data/gp_rbf_test_real.csv',header=F)

gp <- data.matrix(rbind(Y.train,Y.val,Y.test)) # N X T
colnames(gp) <- 1:ncol(gp)
rownames(gp) <- 1:nrow(gp)

# Arrange plots

set.seed(2020)
random.id.educ <- sample(1:nrow(capacity.outcomes$educ.pc$M),1)
random.id.stock <- sample(1:nrow(stock.market),1)
random.id.synth <- sample(1:nrow(sine.waves),1)

acf_order <- grid.arrange(ggacf(t(capacity.outcomes$educ.pc$M)[,random.id.educ], lag.max = 50, main="Education spending",xlab="", ylab=""), 
             ggacf(t(stock.market)[1:500,][,random.id.stock], lag.max = 50, main="Stock prices",xlab="", ylab=""),
             ggacf(t(sine.waves)[1:500,][,random.id.synth], lag.max = 50, main="Sine waves",xlab="", ylab=""),
             ggacf(t(gp)[1:500,][,random.id.synth], lag.max = 50, main="Gaussian processes",xlab="", ylab=""),
             ncol=4, respect=TRUE, bottom="Lag", left="Autocorrelation function")  # textGrob(parse(text = ' "Lag (" * italic("k") * ")" '))

ggsave("results/plots/acf-placebo-plots.png", acf_order, scale=1.5)