TsPlotElections <- function(df, main = "") {
  library(ggplot2)
  library(zoo)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = year)) +
  
  # panel layout
 # facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
  theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
  
  # line colours
    geom_line(data = df, aes(y = y.true, colour = "Observed votediff", linetype="Observed votediff"), show.legend = TRUE, size=0.75) +
   
    geom_line(data = df, aes(y = y.pred, colour = "Predicted votediff", linetype="Predicted votediff"), show.legend = TRUE, size=1.5) +
   
#    geom_line(data = subset(df, variable == "Pointwise votediff"), aes(y = value, colour = "Predicted votediff", linetype="Predicted votediff"), show.legend = FALSE, size=0.5) +
   
  # intervals
    geom_ribbon(data = df, aes(ymin = pred.votediff.min, ymax=pred.votediff.max, colour="Predicted votediff"), alpha=.3, size=0, show.legend = FALSE) +
    
  #  geom_ribbon(data = subset(df, variable == "Pointwise votediff"), aes(ymin = pointwise.votediff.min, ymax=pointwise.votediff.max, colour="Predicted votediff"), alpha=.2, size=1, show.legend = FALSE) +
    
  # horizontal line to indicate zero values
  geom_hline(yintercept = 0, size = 0.5, colour = "black") +
  
  # main y-axis title
  ylab("") +
  
  # main x-axis title
  xlab("") +
  
  # main chart title
  ggtitle(main)
  
  # vertical line to indicate intervention
  
  intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("2005-12-31 00:00:00",tz="UTC")),
                                          as.numeric(as.POSIXct("2006-12-31 00:00:00",tz="UTC"))), linetype=c(2,3))

  # horizontal ticks
  
  # ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"), 
  #                    time_trans(tz="UTC"),
  #                    limits = c(as.POSIXct("1948-12-30 19:00:00"), as.POSIXct("2010-12-30 19:00:00")))
 
# annotation text
  
  ann_text <- data.frame(year = c(as.POSIXlt("1985-01-01 EST"), as.POSIXlt("2010-01-01 EST")), value=90, 
                           series = factor("Winner margin time-series", levels = c("Winner margin time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-period \n (training)", "post-period \n (test)"))

# legend 

  gg.xts <- gg.xts +
    intervention +
   # ticks +
    theme( legend.title = element_blank()
         , legend.position = c(0.35,0.90)
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
    scale_y_continuous(name="Winner margin (%)") +
    scale_colour_manual(name="", values = c("Observed votediff" = wes_palette("Darjeeling")[5], "Predicted votediff" = wes_palette("Darjeeling")[5]),
                        labels=c("Observed winner margin", "Predicted winner margin")) +
    scale_linetype_manual(name="", values = c("Predicted votediff" = "dashed", "Observed votediff" = "solid"),
                          labels=c("Observed winner margin", "Predicted winner margin"))  + 
    theme(legend.key.width=unit(3,"line")) 
return(gg.xts)
}