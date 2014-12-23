# All code written by Paul Tanger. Please cite appropriately.

#install.packages("ggplot2")
#library(ggplot2)
mydotplotSetXcolor = function(mydata, mytitle, myxlab, myylab, fontsize=12, pointsize=4, errbarht=0.4){
  dotplot <- ggplot(mydata, aes(x=mean, y= line)) +
    #geom_dotplot(binaxis = "y", stackgroups = FALSE, binpositions="all", stackdir="center", dotsize=2)
  geom_point(aes(shape=enviro, color=enviro, fill=enviro),   size=pointsize)
  
  # use custom colors
  dotplot <- dotplot + scale_fill_manual(values=cbPalette2)
  dotplot <- dotplot + scale_color_manual(values=cbPalette2)
  
  # split by tissue.. 
  dotplot <- dotplot + facet_grid(tissue ~ .)
  
  # add error bars
  dotplot <- dotplot + geom_errorbarh(aes(xmin=lowerCI, xmax=upperCI, color=enviro), height=errbarht, size=1.2) # adds error bars.. slightly larger than grid lines and height just big enough to see even if smaller than dot
  
  # add title
  dotplot <- dotplot + ggtitle(mytitle)
  
  # add labels
  dotplot <- dotplot +  xlab(myxlab) + ylab(myylab)
  
  # adjust scale
  dotplot <- dotplot + scale_x_continuous(limits= c(0, max(mydata$mean)+5), breaks = round((seq(0, max(mydata$mean)+5, by = 5)/5)) * 5)
  
  dotplot <- dotplot + theme( # add elements to theme
    plot.margin = unit(c(1,1,1,1), "lines"), # make margins a little bigger so y axis label fits
    plot.title = element_text(face="bold", size=fontsize+2),
    axis.text = element_text(color="black", size=fontsize),
    axis.title.x = element_text(face="bold", size=fontsize, vjust=0),
    axis.title.y = element_text(face="bold", size=fontsize, angle=90, vjust=0),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size=1, color="black"),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid.major.x = element_blank(), # switch off major gridlines
    panel.grid.major.y = element_line(color="grey", size=1), # grid line size is 1... a bit smaller than error bars
    panel.grid.minor = element_blank(), # switch off minor gridlines
    #legend.key = element_rect(color=NULL),
    legend.position = "top",
    #legend.position = c(0.9,.8), # manually position the legend (numbers being from 0,0 at bottom left of whole plot to 1,1 at top right)
    legend.title = element_blank(), # switch off the legend title
    legend.text = element_text(size=fontsize),
    #legend.key.size = unit(1.5, "cm"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend  
    panel.background = element_blank(), # panel background
    #plot.background = , # plot background
    strip.text = element_text(size=fontsize, face="bold"),
    strip.background = element_blank(), # background of facet label
    panel.border = element_rect(fill=NA)
  )
    return(dotplot)
}