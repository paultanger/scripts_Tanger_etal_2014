# All code written by Paul Tanger. Please cite appropriately.

mybargraph = function(mydata, mytitle, myxlab, myylab, fontsize=12, errbarwdth=.3, errorbar=T, valuetext=T, breaky=5, textadj=1){
  # since things inside aes, look for data in mydata df, add a column with the vjust parameter
  mydata$textadj=textadj
  dotplot <- ggplot(mydata, aes(x= reorder(line, mean), y=mean)) +
    geom_bar(stat="identity", color="black", fill="#E69F00") +
    if(valuetext){
      geom_text(aes(label=round(mean), vjust=textadj ), size=fontsize-12) # position=position_dodge(width=0.9), vjust=-0.25
    }
  
  # add error bars
  if(errorbar==T){
    dotplot <- dotplot + geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), width=errbarwdth)
  }
  
  # add title
  dotplot <- dotplot + ggtitle(mytitle)
  
  # add labels
  dotplot <- dotplot +  xlab(myxlab) + ylab(myylab)
  
  # adjust scale
  dotplot <- dotplot + scale_y_continuous(limits= c(0, max(mydata$upperCI)+breaky), breaks = round((seq(0, max(mydata$upperCI)+breaky, by = breaky)/breaky)) * breaky)

  dotplot <- dotplot + theme( 
    plot.margin = unit(c(1,1,1,1), "lines"), # make margins a little bigger so y axis label fits
    axis.line = element_line(color="grey", linetype="solid"), # for some reason, you need to turn both on...
    axis.line.x = element_blank(), # then turn x off..
    plot.title = element_text(face="bold", size=fontsize+2),
    axis.text = element_text(color="black", size=fontsize),
    axis.text.x = element_text(angle=45, size=fontsize-4, vjust=1.2, hjust=1), # may need to mess with this.. big number, closer to x
    axis.title.y = element_text(face="bold", size=fontsize, vjust=1.5), # closer to zero, further away..
    axis.title.x = element_text(face="bold", size=fontsize, angle=90, vjust=0),
    axis.ticks.y = element_line(size=1, color="grey"),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()    
  )
  return(dotplot)
}