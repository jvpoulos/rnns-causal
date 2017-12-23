TsPlotSim <- function(df, main = "") {
  library(ggplot2)
  library(zoo)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = year)) +
    
    theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
    
    # line colours
    geom_line(data = df, aes(y = y.true, colour = "Observed votediff", linetype="Observed votediff"), show.legend = TRUE, size=0.75) +
    
    geom_line(data = df, aes(y = y.pred, colour = "Predicted votediff", linetype="Predicted votediff"), show.legend = TRUE, size=1.5) +
    
    geom_line(data = df, aes(y = y.true.c, colour = "True counterfactual", linetype="True counterfactual"), show.legend = FALSE, size=1.5) +
    
    # intervals
    geom_ribbon(data = df, aes(ymin = pred.votediff.min, ymax=pred.votediff.max, colour="Predicted votediff"), alpha=.3, size=0, show.legend = FALSE) +
    
    # horizontal line to indicate zero values
    geom_hline(yintercept = 0, size = 0.5, colour = "black") +
    
    # main y-axis title
    ylab("") +
    
    # main x-axis title
    xlab("") +
    
    # main chart title
    ggtitle(main)
  
  # vertical line to indicate intervention
  
  intervention <- geom_vline(xintercept=c(48), linetype=c(2))
  
  # annotation text
  
  ann_text <- data.frame(year = c(45, 52), value=2.8, 
                         series = factor("Winner margin time-series", levels = c("Winner margin time-series","Pointwise impact","Cumulative impact")),
                         lab = c("pre-period \n (training)", "post-\n period \n (test)"))
  
  # legend 
  
  gg.xts <- gg.xts +
    intervention +
   # ticks +
    theme( legend.title = element_blank()
           , legend.position = c(0.2,0.9)
           , legend.justification = c(1,0)
           #  , legend.position = "top"
           , legend.background = element_rect()
           , axis.text=element_text(size=12)
           , axis.title.x=element_blank()
           , axis.ticks.x=element_blank()
           , axis.ticks.y=element_blank()
           , plot.title = element_text(hjust = 0.5)
           , legend.text=element_text(size=12, family = "serif")
           , legend.box = "horizontal" # not working?)
    ) + geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=5) +
    scale_y_continuous(name="") +
    scale_colour_manual(name="", values = c("Observed votediff" = wes_palette("Darjeeling")[5], "Predicted votediff" = wes_palette("Darjeeling")[5], "True counterfactual" = wes_palette("Darjeeling")[4]),
                        labels=c("Observed outcome", "Predicted outcome","True counterfactual")) +
    scale_linetype_manual(name="", values = c("True counterfactual"="dotted","Predicted votediff" = "dashed", "Observed votediff" = "solid"),
                          labels=c("Observed outcome", "Predicted outcome","True counterfactual"))  + 
    theme(legend.key.width=unit(3,"line")) 
  return(gg.xts)
}