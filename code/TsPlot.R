TsPlot <- function(df, main = "",y.title,limits,breaks,hline) {
  library(ggplot2)
  library(zoo)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = year)) +
    
    # panel layout
    facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
    theme(strip.text= element_text(size = 14, family = "serif", face='bold'), strip.background = element_blank()) +
    
    # line colours
    geom_line(data = subset(df, variable == "observed.pls"), aes(y = value, colour = "observed.pls", linetype="observed.pls"), show.legend = TRUE, size=1) +
    geom_line(data = subset(df, variable == "predicted.pls"), aes(y = value, colour = "predicted.pls", linetype="predicted.pls"), show.legend = FALSE, size=1) +
    geom_line(data = subset(df, variable == "pointwise.pls"), aes(y = value, colour = "pointwise.pls", linetype="pointwise.pls"), show.legend = FALSE, size=1) +

    geom_line(data = subset(df, variable == "observed.sls"), aes(y = value, colour = "observed.sls", linetype="observed.sls"), show.legend = TRUE, size=1) +
    
    # intervals
    geom_ribbon(data = subset(df, variable == "pointwise.pls"), aes(ymin = lower, ymax=upper, colour="pointwise.pls"), alpha=.1, size=0.2, show.legend = FALSE) +

    # horizontal line to indicate zero values
    geom_hline(aes(yintercept = hline), size = 0.5, colour = "black") +
    
    # main y-axis title
    ylab(y.title) +
    
    # x-axis title
    xlab("\nTime") +
  
    # main chart title
    ggtitle(main)  
  
  # vertical line to indicate intervention
  
  intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("1869-1-31 00:00:00",tz="UTC"))), linetype=2)
  
  # horizontal ticks
  
  ticks <- scale_x_datetime(breaks=breaks,
                            labels=date_format("%Y"), 
                            limits=limits) 
  
  # legend 
  
  gg.xts <- gg.xts +
    intervention +
    ticks + 
    theme( legend.title = element_blank()
           , plot.title = element_text(hjust = 0.5)
           , legend.justification = c(1,0)
            , legend.position = "none"
           , legend.background = element_rect()
           , axis.text=element_text(size=12)
           , axis.title.x=element_text(size = 14)
           , axis.title.y=element_text(size = 14)
           , legend.text=element_text(size=14, family = "serif")
           , legend.box = "horizontal" # not working?)
    ) + 
    scale_colour_manual(name="", values = c(  "observed.pls" = wes_palette("Darjeeling1")[5], 
                                              "observed.sls" = wes_palette("Darjeeling1")[1], 
                                              "predicted.pls" = wes_palette("Darjeeling1")[5],
                                              "pointwise.pls" = wes_palette("Darjeeling1")[5]),
                        labels=c("Observed treated", "Observed control", 
                                 "Predicted treated", "Pointwise treated")) +
    scale_linetype_manual(name="", values = c("observed.pls" = "solid", 
                                              "observed.sls" = "dashed", 
                                              "predicted.pls" = "dotted",
                                              "pointwise.pls" = "dotdash"),
                          labels=c("Observed treated", "Observed control", 
                                   "Predicted treated", "Pointwise treated")) +
    theme(legend.key.width=unit(4,"line")) 
  return(gg.xts)
}