# All code written by Paul Tanger. Please cite appropriately.

################################################################
# TABLE S8
################################################################
setwd("../output/")
summary.stats = read.delim("SummaryData5Lines_Rearrange.tsv")

summary.stats = summary.stats[,-c(5,6)]
forcorr = dcast(summary.stats[,c(1:5)], line + tissue + enviro ~ parameter, mean)

library(corrplot)
library(RColorBrewer)
scalebluered <- colorRampPalette(brewer.pal(8, "RdBu"))(8)

as.data.frame(colnames(forcorr))
# sort
forcorr = forcorr[,c(1:3,6,21,12,22,16,15,5,14,9,11,19,20,8,18,17,13,7)]
# get cors
corsmatrix = as.matrix(forcorr[4:20])
# corsmatrix = as.matrix(forcorr)
cors = cor(corsmatrix)
# get spearman cors
library(Hmisc)
scors = rcorr(corsmatrix, type="spearman")$r
scorsp = rcorr(corsmatrix, type="spearman")$P

# disable scientific notation
options(scipen=999)

# get matrix indices only when p value is less than <0.05
indices5 = which(scorsp<0.05 & scorsp>=0.01, arr.ind=T)
# and one for <0.01
indices1 = which(scorsp<0.01, arr.ind=T)
# and one for empty spaces
indicesnotsig = which(scorsp>0.05, arr.ind=T)
# export as supp table..
allspearmancorrs.r = formatC(round(scors, digits=3), 2, format="f")
allspearmancorrs.p = formatC(round(scorsp, digits=3), 3, format="f")
# if less than 0.00, change it..
allspearmancorrs.p[allspearmancorrs.p == "0.000"] = "< 0.001"
# only keep bottom of matrix
allspearmancorrs.r[upper.tri(allspearmancorrs.r)] = "" 
# and delete diagonals
diag(allspearmancorrs.r) = NA
allspearmancorrs.r[is.na(allspearmancorrs.r)] = ""
# now add stars to sig r values
# make a new matrix only when p value is less than <0.05
allspearmancorrs.rfinal = allspearmancorrs.r
allspearmancorrs.rfinal[indices5] = paste0(allspearmancorrs.rfinal[indices5], "  *")
allspearmancorrs.rfinal[indices1] = paste0(allspearmancorrs.rfinal[indices1], " **")
allspearmancorrs.rfinal[indicesnotsig] = paste0(allspearmancorrs.rfinal[indicesnotsig], "   ")
allspearmancorrs.rfinal[upper.tri(allspearmancorrs.rfinal)] = "" 
# delete last col.. unnesesary..
allspearmancorrs.rfinal = allspearmancorrs.rfinal[,-ncol(allspearmancorrs.rfinal)]
# export it
fileprefix = "TableS8.tsv"
write.table(allspearmancorrs.rfinal, file=fileprefix, sep="\t", quote=F, row.names=T, col.names=NA, na="") 

# copy in case we mess it up
plotscors = scors
plotpvals = scorsp

# define bonferroni alpha
vars = nrow(scorsp)
n = (vars * (vars - 1)) / 2
alpha = 0.05 / n

# so this plots with our custom bonferroni alpha value..
postscript('Fig6.eps', width=10, height=8.5, paper="special", family="Helvetica", fonts="Helvetica", horizontal=F, onefile=F, bg="white")
corrplot(plotscors, method="ellipse", col=scalebluered, shade.col=NA, 
         tl.col="black", tl.srt=45, tl.cex = 1, tl.offset=.4, type="lower", order="original", addrect=3,
         diag=FALSE, addgrid.col="black", title="", cl.pos="n",p.mat = scorsp, insig = "p-value", sig.level=alpha)
colorlegend(xlim=c(14,16), ylim=c(10,15), colbar=scalebluered, labels=c(seq(-1,1,.25)), align="r", ratio.colbar=.25, vertical=TRUE, addlabels=TRUE)
dev.off()

################################################################
# FIGURE S9
################################################################
setwd("../output/")
jmpcorrsbyE = read.table("AllData5LinesForCorrJMP_EnviroSeparate.tsv", sep = "\t", header = T)

# sort
jmpcorrsbyE = jmpcorrsbyE[,c(1:3,15:18,45:48)]
# get cors
Ecorsmatrix = as.matrix(jmpcorrsbyE[4:11])
# get spearman cors
Escors = rcorr(Ecorsmatrix, type="spearman")$r
Escorsp = rcorr(Ecorsmatrix, type="spearman")$P

# calculate bonferroni
vars = nrow(Escorsp)
n = (vars * (vars - 1)) / 2

# new alpha
alpha = 0.05 / n

# get plot
postscript('FigS9.eps', width=10, height=8.5, paper="special", family="Helvetica", fonts="Helvetica", horizontal=F, onefile=F, bg="white")
corrplot(Escors, method="ellipse", col=scalebluered, shade.col=NA, 
         tl.col="black", tl.srt=45, tl.cex = 1, tl.offset=.4, type="lower", order="original", addrect=3,
         diag=FALSE, addgrid.col="black", title="", cl.pos="n", p.mat = Escorsp, insig = "p-value", sig.level=alpha)
colorlegend(xlim=c(6,7), ylim=c(6,8), colbar=scalebluered, labels=c(seq(-1,1,.25)), align="r", ratio.colbar=.25, vertical=TRUE, addlabels=TRUE)
dev.off()


################################################################
# TABLE S9
################################################################

jmpcorrsbyE = read.table("AllData5LinesForCorrJMP_EnviroSeparate.tsv", sep = "\t", header = T)
# sort
jmpcorrsbyE = jmpcorrsbyE[,c(1,2,5:10,13:18,23:34,37,38,41:48)]

# matrix for corr
esepmatrix = as.matrix(jmpcorrsbyE[,c(3:36)])

Escors = rcorr(esepmatrix, type="spearman")$r
Escorsp = rcorr(esepmatrix, type="spearman")$P

# get matrix indices only when p value is less than <0.05
indices5 = which(Escorsp<0.05 & Escorsp>=0.01, arr.ind=T)
# and one for <0.01
indices1 = which(Escorsp<0.01, arr.ind=T)
indicesnotsig = which(Escorsp>0.05, arr.ind=T)
# export as supp table..
E.r = formatC(round(Escors, digits=3), 2, format="f")
E.p = formatC(round(Escorsp, digits=3), 3, format="f")
# if less than 0.00, change it..
E.p[E.p == "0.000"] = "< 0.001"
# only keep bottom of matrix
E.r[upper.tri(E.r)] = "" 
# and delete diagonals
diag(E.r) = NA
E.r[is.na(E.r)] = ""
#now add stars to sig r values
# make a new matrix only when p value is less than <0.05
E.rfinal = E.r
E.rfinal[indices5] = paste0(E.rfinal[indices5], "  *")
E.rfinal[indices1] = paste0(E.rfinal[indices1], " **")
# if not in either group, add two spaces to make everything even..
E.rfinal[indicesnotsig] = paste0(E.rfinal[indicesnotsig], "   ")
E.rfinal[upper.tri(E.rfinal)] = "" 
# delete last col.. unnesesary..
E.rfinal = E.rfinal[,-ncol(E.rfinal)]
# export it
fileprefix = "TableS9.tsv"
write.table(E.rfinal, file=fileprefix, sep="\t", quote=T, row.names=T, col.names=NA, na="") 

setwd("../fieldvsgh_final/")