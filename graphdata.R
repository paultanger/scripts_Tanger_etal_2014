# All code written by Paul Tanger. Please cite appropriately.

setwd("../output/")
summary.stats = read.delim("SummaryData5Lines_Rearrange.tsv")

library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

# define color palettes - colorblind friendly
# with black
cbPalette2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

################################################################
# GET MEANS OF ALL VARIETIES AND PLOT THEM
################################################################

# try collapsing data from all lines
setwd("../fieldvsgh_final/")
source("collapsedplotfunction.R")
tocollapse = summary.stats[,c(1,3,4,7)]
collapsestats <- aggregate(mean ~ enviro + tissue + parameter, data=tocollapse, FUN=function(x) c(length(x), mean(x), sd(x), sd(x)/sqrt(length(x)), sd(x)/mean(x)))
collapsestats <- cbind(collapsestats[,1:3], as.data.frame(collapsestats[,4]))
names(collapsestats) <- c("enviro", "tissue", "parameter", "n", "mean", "SD", "SE", "CV")
collapsestats$lowerCI <- collapsestats$mean - collapsestats$SD
collapsestats$upperCI <- collapsestats$mean + collapsestats$SD

forage.summary.data <- subset(collapsestats, parameter %in% c("Lignin", "Cellulose", "Hemicellulose", "Ash"))
nick.summary.data <- subset(collapsestats, parameter %in% c("glucose_base", "pentose_base", "glucose_water", "pentose_water"))
decon.summary.data <- subset(collapsestats, parameter %in% c("Klason_Lignin", "Klason_Ash", "Total_Glucose", "Total_Xylose"))
effic.summary.data = subset(collapsestats, parameter %in% c("Glu_Efficiency", "Pen_Efficiency"))
other.summary.data = subset(collapsestats, parameter %in% c("HRGP", "MLG", "AcBrLignin"))

forage.summary.data$parameter = with(forage.summary.data, factor(parameter, levels=c("Ash", "Lignin", "Hemicellulose", "Cellulose") ))

#setup font and point sizes
fontsize=27
pointsize=8

foragecol = collapsedplot(forage.summary.data, "", "% dry matter", "", fontsize, pointsize)

nick.summary.data$parameter = with(nick.summary.data, factor(parameter, levels=rev(levels(parameter)) ))

sugarcol = collapsedplot(nick.summary.data, "", "% as received mass", "", fontsize, pointsize)

decon.summary.data$parameter = with(decon.summary.data, factor(parameter, levels=rev(c("Total_Glucose", "Total_Xylose", "Klason_Lignin", "Klason_Ash")) ))

deconcol = collapsedplot(decon.summary.data, "", "% as received mass", "", fontsize, pointsize)

# swap factor level order for these graphs.. Glu on top..
effic.summary.data$parameter = with(effic.summary.data, factor(parameter, levels=rev(levels(parameter))))

efficcol = collapsedplot(effic.summary.data, "", "% of total sugar", "", fontsize, pointsize)

# set compositional axis to be the same
foragecol = foragecol + scale_x_continuous(limits= c(0, 45), breaks = seq(0, 45, by = 5))
deconcol = deconcol + scale_x_continuous(limits= c(0, 45), breaks = seq(0, 45, by = 5))

setwd("../output/")

# just save separately and arrange in illustrator..
ggsave("Fig2a.eps", foragecol, width=7, height=6, units="in", scale=1.5) # eps embeds fonts
ggsave("Fig2b.eps", deconcol, width=7, height=6, units="in", scale=1.5) # eps embeds fonts
ggsave("Fig3a.eps", sugarcol, width=7, height=6, units="in", scale=1.5) # eps embeds fonts
ggsave("Fig3b.eps", efficcol, width=7, height=6, units="in", scale=1.5) # eps embeds fonts

################################################################
# GRAPH STUFF SEPARATELY
################################################################

# with SD
summary.stats$lowerCI <- summary.stats$mean - summary.stats$SD
summary.stats$upperCI <- summary.stats$mean + summary.stats$SD

# since we only have one plant for azucena in GH, remove CI.. since this is not
# a true CI.. this is just tech rep variation..
summary.stats$lowerCI[which(summary.stats$line=="Azucena" & summary.stats$enviro=="GH")] = NA
summary.stats$upperCI[which(summary.stats$line=="Azucena" & summary.stats$enviro=="GH")] = NA

# reorder data for graphs
summary.stats$line = with(summary.stats, factor(line, levels=rev(levels(line))))

forage.summary.data <- subset(summary.stats, parameter %in% c("Lignin", "Cellulose", "Hemicellulose", "Ash"))
nick.summary.data <- subset(summary.stats, parameter %in% c("glucose_base", "pentose_base", "glucose_water", "pentose_water"))
decon.summary.data <- subset(summary.stats, parameter %in% c("Klason_Lignin", "Klason_Ash", "Total_Glucose", "Total_Xylose"))
effic.summary.data = subset(summary.stats, parameter %in% c("Glu_Efficiency", "Pen_Efficiency"))

# other stuff
other.summary.data = subset(summary.stats, parameter %in% c("Density", "HRGP", "MLG", "AcBrLignin"))

setwd("../fieldvsgh_final/")
source("tryggplotfunction.R")
source("tryggplotfunctionControlXaxisColor.R")

# share common legend..
#http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
ggplotlegend = function(plotobj){
  tmp <- ggplot_gtable(ggplot_build(plotobj))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

################################################################
# SUGAR DATA
################################################################

#setup font and point sizes
fontsize=20
pointsize=6

# subset nick data
glucose = subset(nick.summary.data, parameter %in% c("glucose_base"))
pentose = subset(nick.summary.data, parameter %in% c("pentose_base"))
gluwat = subset(nick.summary.data, parameter %in% c("glucose_water"))
penwat = subset(nick.summary.data, parameter %in% c("pentose_water"))
freeg = subset(summary.stats, parameter %in% c("glucose_free"))

gluplot  = mydotplotSetXcolor(glucose, "Glucose, after base pretreatment", "% as received mass", "", fontsize, pointsize, .5)
penplot  = mydotplotSetXcolor(pentose, "Pentose, after base pretreatment", "% as received mass", "", fontsize, pointsize, .5)
gluWplot = mydotplotSetXcolor(gluwat, "Glucose, after hot water pretreatment", "% as received mass", "",    fontsize, pointsize, .5)
penWplot = mydotplotSetXcolor(penwat, "Pentose, after hot water pretreatment", "% as received mass", "",    fontsize, pointsize, .5)

# free glucose
freeGplot = mydotplotSetXcolor(freeg, "Free glucose", "% as received mass", "",  fontsize, pointsize, .5)

# adjust to be constant X axis
gluWplot = gluWplot + scale_x_continuous(limits= c(0, 35), breaks = seq(0, 35, by = 5))
penplot = penplot + scale_x_continuous(limits= c(0, 6), breaks = seq(0, 6, by = 1))
penWplot = penWplot + scale_x_continuous(limits= c(0, 6), breaks = seq(0, 6, by = 1))

# set a legend.. doesn't matter which plot you use..
mylegend = ggplotlegend(gluplot)

sugarplots = arrangeGrob(mylegend, nrow=2, heights=c(1,10), # makes a small row for the legend at top, and big row for plots..
                         arrangeGrob( 
                           gluplot + theme(legend.position="none"),
                           penplot + theme(legend.position="none"),
                           gluWplot + theme(legend.position="none"),
                           penWplot + theme(legend.position="none"),
                           ncol=2) # defines columns for plots only
)
setwd("../output/")
ggsave("Fig4.eps", sugarplots, width=11, height=8.5, units="in", scale=1.5) # eps embeds fonts

################################################################
# FORAGE DATA
################################################################

# subset forage data
cellulose = subset(forage.summary.data, parameter %in% c("Cellulose"))
lignin = subset(forage.summary.data, parameter %in% c("Lignin"))
hemicell = subset(forage.summary.data, parameter %in% c("Hemicellulose"))
ash = subset(forage.summary.data, parameter %in% c("Ash"))

# get forage plots
cellplot = mydotplot(cellulose, "Cellulose", "% dry matter", "", 18, 6, .5)
lignplot = mydotplot(lignin, "Lignin", "% dry matter", "", 18, 6, .5)
hemiplot = mydotplot(hemicell, "Hemicellulose", "% dry matter", "", 18, 6, .5)
ash_plot = mydotplot(ash, "Ash", "% dry matter", "", 18, 6, .5)

# add more numbers to scales
cellplot = cellplot + scale_x_continuous(limits=c(20,45), breaks=seq(20,45, by=5) )
lignplot = lignplot + scale_x_continuous(limits=c(0,8), breaks=seq(0,8, by=1) )
hemiplot = hemiplot + scale_x_continuous(limits=c(10,35), breaks=seq(10,35, by=5) )
ash_plot = ash_plot + scale_x_continuous(limits=c(0,35), breaks=seq(0,35, by=5) )

# plot together and save
mylegend = ggplotlegend(cellplot)

forageplots = arrangeGrob(mylegend, nrow=2, heights=c(1,10), # makes a small row for the legend at top, and big row for plots..
                          arrangeGrob( 
                            cellplot + theme(legend.position="none"),
                            lignplot + theme(legend.position="none"),
                            hemiplot + theme(legend.position="none"),
                            ash_plot + theme(legend.position="none"),
                            ncol=2) # defines columns for plots only
)

ggsave("FigS2.eps", forageplots, width=11, height=8.5, units="in", scale=1.5) # eps embeds fonts

################################################################
# OTHER DATA
################################################################

density.summary.data = subset(other.summary.data, parameter %in% c("Density"))
hyp.summary.data = subset(other.summary.data, parameter %in% c("HRGP"))
MLG.summary.data = subset(other.summary.data, parameter %in% c("MLG"))
AcBrLignin.summary.data = subset(other.summary.data, parameter %in% c("AcBrLignin"))

hypplot = mydotplot(hyp.summary.data, "HRGPs", "% as received mass", "", 18, 6, .5)

densplot = mydotplot(density.summary.data, "Bulk density", "kg/m^3", "", 18, 6, .5)
densplot = densplot + xlab(expression(bold(kg/m^3)))

MLGnormplot = mydotplot(MLG.summary.data, "MLG", "abs 405/490", "", 18, 6, .5)

# add more numbers to scales
densplot = densplot + scale_x_continuous(limits=c(250,425), breaks=seq(250,425, by=50) )
MLGnormplot = MLGnormplot + scale_x_continuous(limits=c(3,20), breaks=seq(5,20, by=5) )

# from above, maybe best to fit free glucose here?
freeGplot = mydotplot(freeg, "Free glucose", "% as received mass", "",  18, 6, .5)
freeGplot = freeGplot + scale_x_continuous(limits=c(0.5,9), breaks=seq(0,10, by=2) )

# save them separate...
ggsave("FigS5.eps", freeGplot, width=7, height=6, units="in", scale=1.5) # eps embeds fonts
ggsave("FigS6.eps", MLGnormplot, width=7, height=6, units="in", scale=1.5) # eps embeds fonts
ggsave("FigS7.eps", hypplot, width=7, height=6, units="in", scale=1.5) # eps embeds fonts
ggsave("FigS8.eps", densplot, width=7, height=6, units="in", scale=1.5) # eps embeds fonts

################################################################
# DECON DATA
################################################################

# subset decon data
totglu = subset(decon.summary.data, parameter %in% c("Total_Glucose"))
totxyl = subset(decon.summary.data, parameter %in% c("Total_Xylose"))
klaslig = subset(decon.summary.data, parameter %in% c("Klason_Lignin"))
klasash = subset(decon.summary.data, parameter %in% c("Klason_Ash"))

# get decon plots
totgluplot = mydotplot(totglu, "Total Glucose", "% as received", "", 18, 6, .5)
totxylplot = mydotplot(totxyl, "Total Xylose", "% as received", "", 18, 6, .5)
klasligplot = mydotplot(klaslig, "Klason Lignin", "% as received", "", 18, 6, .5)
klasashplot = mydotplot(klasash, "Klason Ash", "% as received", "", 18, 6, .5)

# add more numbers to scales
totgluplot = totgluplot + scale_x_continuous(limits=c(18,50), breaks=seq(20,50, by=5) )
totxylplot = totxylplot + scale_x_continuous(limits=c(11.5,18.5), breaks=seq(12,18, by=2) )
klasligplot = klasligplot + scale_x_continuous(limits=c(6.5,20), breaks=seq(8,20, by=2) )
klasashplot = klasashplot + scale_x_continuous(limits=c(0,30), breaks=seq(0,35, by=5) )

mylegend = ggplotlegend(totgluplot)

# plot together and save
deconplots = arrangeGrob(mylegend, nrow=2, heights=c(1,10), # makes a small row for the legend at top, and big row for plots..
                          arrangeGrob( 
                            totgluplot + theme(legend.position="none"),
                            klasligplot + theme(legend.position="none"),
                            totxylplot + theme(legend.position="none"),
                            klasashplot + theme(legend.position="none"),
                            ncol=2) # defines columns for plots only
)
ggsave("FigS3.eps", deconplots, width=11, height=8.5, units="in", scale=1.5) # eps embeds fonts

################################################################
# EFFICIENCY DATA
################################################################

# subset % total data
pctglutot = subset(effic.summary.data, parameter %in% c("Glu_Efficiency"))
pctxyltot = subset(effic.summary.data, parameter %in% c("Pen_Efficiency"))

pcttotgluplot = mydotplot(pctglutot, "glucose after base pretreatment efficiency", "% of total glucose", "", fontsize, pointsize, .5)
pcttotxylplot = mydotplot(pctxyltot, "pentose after base pretreatment efficiency", "% of total xylose", "", 18, 6, .5)

ggsave("FigS4a.eps", pcttotgluplot, width=7, height=6, units="in", scale=1.5) # eps embeds fonts
ggsave("FigS4b.eps", pcttotxylplot, width=7, height=6, units="in", scale=1.5) # eps embeds fonts

setwd("../fieldvsgh_final/")