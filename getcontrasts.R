# All code written by Paul Tanger. Please cite appropriately.
setwd("../output/")
alldata = read.delim("AllDataForLSMeansPdiff.tsv")

# disable scientific notation
options(scipen=999)

leafdata = subset(alldata, tissue == "leaf")
stemdata = subset(alldata, tissue == "stem")

# create matrices of response variables
leafresp  = as.matrix(cbind(leafdata[,4:20]))
stemresp  = as.matrix(cbind(stemdata[,4:20]))

# run fixed models with line* enviro, dump into lists
leaffits = lm(leafresp ~ line*enviro, data=leafdata)
stemfits = lm(stemresp ~ line*enviro, data=stemdata)

##########################################################
# check.. 
# getrespname = function(model){
#   return (all.vars(terms(model
#   ))[1])
# }
# run one at a time

# fitdata = leafdata
# tissue = "leaf"
# 
# fitdata = stemdata
# tissue = "stem"
# 
# par(mfrow=c(2,2))
# 
# fit = lm(Cellulose     ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(Lignin        ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(Ash           ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(Hemicellulose ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(glucose_base  ~ line*enviro, data=fitdata)             # why two groups in residuals for leaf? it is enviro...
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(pentose_base  ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(glucose_free  ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(glucose_water ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(pentose_water ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(MLG           ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# fit = lm(AcBrLignin   ~ line*enviro, data=fitdata)
# plot(fit, c(1,2), main=paste(tissue, getrespname(fit)), ask=F)
# 
# summary(fit)
##########################################################

# see if we can get the LSMEANS
#install.packages("lsmeans")
library(lsmeans)

# this doesn't work.. it averages over all responses..
#leaf.lsmeans = lsmeans(leaffits, ~ line*enviro)
#stem.lsmeans = lsmeans(stemfits, ~ line*enviro)
# from the author of the lsmeans package:
# lsmeans treats a multivariate response like another factor
# by default named 'rep.meas'

# so using the repeated measures inside lsmeans which it assigned as each response,
# get the lsmeans of each response separately
# returns an lsmeans object ...
leaf.lsmeans.lsmobj = lsmeans(leaffits, ~ line*enviro | rep.meas)
stem.lsmeans.lsmobj = lsmeans(stemfits, ~ line*enviro | rep.meas)

# extract data from lsmeans objects
leaflsmeansdf = summary(leaf.lsmeans.lsmobj)
stemlsmeansdf = summary(stem.lsmeans.lsmobj)

# combine and compare to plain old means, calculated in my mergalldatatogether script
leaflsmeansdf$tissue = "leaf"
stemlsmeansdf$tissue = "stem"
lsmeansdf = as.data.frame(rbind(leaflsmeansdf,stemlsmeansdf))

# get F tests for each line between two enviros
leafpdiff = contrast(leaf.lsmeans.lsmobj, adjust="none", by=c("rep.meas", "line"), method="pairwise")
stempdiff = contrast(stem.lsmeans.lsmobj, adjust="none", by=c("rep.meas", "line"), method="pairwise")

leafpdiffDF = summary(leafpdiff)
stempdiffDF = summary(stempdiff)

leafpdiffDF$tissue = "leaf"
stempdiffDF$tissue = "stem"

pdiffDF = rbind(leafpdiffDF,stempdiffDF)
pdifftomerge = pdiffDF[,c(3,9,2,8)]
colnames(pdifftomerge)[colnames(pdifftomerge)=="rep.meas"] = "parameter"

# merge with other data.. this becomes Table S2 in the publication
means = read.delim("SummaryData5Lines_Rearrange.tsv")

# subset summary data by enviro, and cbind to make it wide.. 
# with GH next to Field values for easy comparison.. then add pdiffs
GH    = subset(means, enviro == "GH")
field = subset(means, enviro == "field")

# cbind together..
summarywide = merge(GH,field, by=c("line","tissue","parameter"))
summarywide = summarywide[,-c(4,11,12)]
# rename cols
colnames(summarywide) = c("line", "tissue", "parameter", "units", 
                          "GH_n", "GH_mean", "GH_SD", "GH_SE", "GH_CV",  
                          "Field_n", "Field_mean", "Field_SD", "Field_SE", "Field_CV")

# cbind or merge pdiff to summarywide.. keep all, though no p values for acbr & mlg..
summaryfinal = merge(summarywide,pdifftomerge, by=c("line","tissue","parameter"), all.x=T)

# save it
# fileprefix = paste("TableS2_", format(Sys.time(),"%Y%m%d_%H%M"), sep="")
fileprefix = "TableS2.tsv"
write.table(summaryfinal, file=fileprefix, sep="\t", quote=F, row.names=F, col.names=T) 

setwd("../fieldvsgh_final/")
