# All code written by Paul Tanger. Please cite appropriately.

setwd("../input/")
# get data from Table S3 and S4
data = read.csv("fromtableS3andS4.csv")

# subset it
sugar = subset(data, parameter == "glucose_stem")
mlg = subset(data, parameter == "MLG")

# convert MLG to %
mlg$mean = mlg$mean *0.1

# percent
sugar$pct = sugar$mean * 100

# get means
sugarmeans <- aggregate(pct ~ line, data=sugar, FUN=function(x) c(length(x), mean(x), sd(x), sd(x)/sqrt(length(x)), sd(x)/mean(x)))
sugarmeans <- cbind(sugarmeans[,1], as.data.frame(sugarmeans[,2]))
names(sugarmeans) <- c("line", "n", "mean", "SD", "SE", "CV")

mlgmeans <- aggregate(mean ~ line, data=mlg, FUN=function(x) c(length(x), mean(x), sd(x), sd(x)/sqrt(length(x)), sd(x)/mean(x)))
mlgmeans <- cbind(mlgmeans[,1], as.data.frame(mlgmeans[,2]))
names(mlgmeans) <- c("line", "n", "mean", "SD", "SE", "CV")

# sort by pct
sugarmeans = sugarmeans[order(sugarmeans$mean),]
mlgmeans = mlgmeans[order(mlgmeans$mean),]

# make error bars
sugarmeans$lowerCI <- sugarmeans$mean - sugarmeans$SD
sugarmeans$upperCI <- sugarmeans$mean + sugarmeans$SD

mlgmeans$lowerCI <- mlgmeans$mean - mlgmeans$SD
mlgmeans$upperCI <- mlgmeans$mean + mlgmeans$SD

# if any less than zero, make them zero...
sugarmeans$lowerCI[sugarmeans$lowerCI < 0] = 0
mlgmeans$lowerCI[mlgmeans$lowerCI < 0] = 0

# plot with ggplot2
library(ggplot2)
library(grid)

# with black
cbPalette2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

setwd("../fieldvsgh_final/")
source("bargraphfunction.R")

oldmlgplot = mybargraph(mlgmeans, "", "", "MLG % as received mass", fontsize=29, errorbar=T, breaky=2, textadj=-0.5, valuetext=F)
oldsugarplot = mybargraph(sugarmeans, "", "", "glucose yield % as received mass", fontsize=29, breaky=5, errorbar=T, valuetext=F, textadj=-0.5)

setwd("../output")
ggsave("Fig1a.eps", oldmlgplot, width=7, height=6, units="in", scale=1.5) # eps embeds fonts
ggsave("Fig1b.eps", oldsugarplot, width=7, height=6, units="in", scale=1.5) # eps embeds fonts

# pdf('Fig1a.pdf', width=11, height=8.5)
# # postscript("Fig1a.eps", height=8, width=8, paper="special", family="Helvetica", fonts="Helvetica", horizontal=FALSE, onefile=FALSE)
# mybargraph(mlgmeans, "", "", "MLG % as received mass", fontsize=29, errorbar=T, breaky=2, textadj=-0.5, valuetext=F)
# dev.off()
# 
# pdf('Fig1b.pdf', width=11, height=8.5)
# # postscript("Fig1b.eps", height=8, width=8, paper="special", family="Helvetica", fonts="Helvetica", horizontal=FALSE, onefile=FALSE)
# mybargraph(sugarmeans, "", "", "glucose yield % as received mass", fontsize=29, breaky=5, errorbar=T, valuetext=F, textadj=-0.5)
# dev.off()

# generate figure S1
setwd("../input/")
corrdata = read.csv("20linesdataforcorrelations.csv")
as.data.frame(colnames(corrdata))
corrdata = corrdata[,c(3:8)]
colnames(corrdata) = c("Pen Yield (% AR)", "Glu Yield (% AR)", "MLG (ug/g)", "AcBr Lignin (ug/mg)", "Sacc sugars (% AR)", "Sacc Yield (ug/mg)")
corrdata = corrdata[,c(2,1,6,5,3,4)]

library(Hmisc)

panel.cor <- function(x, y, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x,y, use="pairwise.complete.obs") # na.or.complete doesn't work correctly.. removes rows..
  p <- rcorr(x,y, type="pearson")$P[1,2]
  rval <- formatC(r, 2, format="f")
  rval <- paste("r = ", rval, sep="")
  pval <- formatC(p, 2, format="f")
  pval <- paste("p = ", pval, sep="")
  if(pval == "p = 0.00"){ pval = "p < 0.01"}
  text(0.5, 0.7, rval, cex=1.5) # if this is too big, it messes up pdf export
  text(0.5, 0.3, pval, cex=1.5)
}

setwd("../output")
postscript("FigS1.eps", height=8, width=8, paper="special", family="Helvetica", fonts="Helvetica", horizontal=FALSE, onefile=FALSE)
pairs( ~ `Glu Yield (% AR)` + `Pen Yield (% AR)` + `Sacc Yield (ug/mg)` + `Sacc sugars (% AR)` + `MLG (ug/g)` + `AcBr Lignin (ug/mg)`, data=corrdata, main="", upper.panel=panel.cor)
dev.off()

setwd("../fieldvsgh_final/")
