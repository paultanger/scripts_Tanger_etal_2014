# All code written by Paul Tanger. Please cite appropriately.

# you need to be in the main directory "fieldvsgh_final":
# setwd("")
# merge all data together
# AcBr Lignin and MLG (kit method)
source("mergeAcBrMLGdata.R")
# Forage data (Cellulose, Lignin, Hemicellulose, Ash)
source("mergeforagedata.R")
# glucose and pentose yield
source("mergesugardata.R")
# bulk density
source("mergedensitydata.R")
# HRGPs
source("mergeHRGPdata.R")
# NREL composition (Glucose, Xylose, Klason Lignin and Ash)
source("mergeNRELdata.R")
# new MLG ELISA data
source("mergeMLGdata.R")

# here are our data names:
#mlgacbrdata
#nickdata
#foragedatafinal
#densitydata
#hypfinal
#deconfinal
#eMLGfinal

# since foragedatafinal already has the sampleinfo, just add to that
merge1 = merge(foragedatafinal, nickdatafinal, by.x="ID", by.y="SampleID", all=T)
merge2 = merge(merge1, mlgacbrdata, by="ID", all=T)
merge3 = merge(merge2, densitymeans,  by.x="ID", by.y="SampleID", all=T)
merge4 = merge(merge3, hypfinal[,c(1,3)], by.x="ID", by.y="tubeID", all=T)
merge5 = merge(merge4, deconfinal, by="ID", all=T)
merge6 = merge(merge5, eMLGfinal, by="ID", all=T)

# now we have all the data

# get rid of stuff we don't need for graphing
alldata = merge6[,c(2:4,8,14,16,18,20:36)]

# correct total glucose by free glucose
alldata$totgluC = alldata$tot_glu - alldata$glucose_free

# add calculated phenotypes to phenotype file
alldata$glu_base_effic = (alldata$glucose_base / alldata$totgluC) * 100
alldata$pen_base_effic = (alldata$pentose_base / alldata$tot_xyl) * 100

# change units of extensin to % dry weight.. right now it is ug/ml
alldata$HRGP = alldata$hyp / 10000

# change units of density from to kg/m3
alldata$density = alldata$density * 1000

# change units of AcBr lignin from ug/mg to %
alldata$acbr = alldata$acbr * 0.1

# filter for 5 lines
alldata <- subset(alldata, pretty.name %in% c("Aswina", "IR64-21", "LTH", "Azucena", "Zhenshan 97B"))

# get rid of MLG..
alldata = subset(alldata, select=-mlg)

# transform box cox for variables that need it..
# assumess data is loaded as "alldata", checks normality, transforms ones that need it & add to alldata as new cols
source("boxcoxtransform.R")

# export this to do model selection in jmp using transformed data
forJMPmodelselection = alldata[,c(2:4,5,6,28,7,29,8,9,30,10,31,13,34,14,35,16,36,19,20,21,38,22,39,24,41,25,42,26,43,27)]
# make pretty names
colnames(forJMPmodelselection) = c('tissue',
                                   'enviro',
                                   'line',
                                   'Cellulose',
                                   'LigninUN',
                                   'Lignin',
                                   'AshUN',
                                   'Ash',
                                   'Hemicellulose',
                                   'Glucose_baseUN',
                                   'Glucose_base',
                                   'Pentose_baseUN',
                                   'Pentose_base',
                                   'Pentose_waterUN',
                                   'Pentose_water',
                                   'Glucose_waterUN',
                                   'Glucose_water',
                                   'DensityUN',
                                   'Density',
                                   'Total_Xylose',
                                   'Klason_Lignin',
                                   'Klason_AshUN',
                                   'Klason_Ash',
                                   'MLGUN',
                                   'MLG',
                                   'Total_GlucoseUN',
                                   'Total_Glucose',
                                   'Glu_base_efficUN',
                                   'Glu_base_effic',
                                   'Pen_base_efficUN',
                                   'Pen_base_effic',
                                   'HRGP')
fileprefix = "DataforModelSelectionJMP.tsv"
setwd("../output")
write.table(forJMPmodelselection, file=fileprefix, sep="\t", quote=F, row.names=F, col.names=T) 

# write data for PCA in JMP
forjmp = alldata
forjmp = forjmp[,-c(1,28:43)]
colnames(forjmp) = c("tissue", "enviro", "line", 
    "Cellulose", 
    "Lignin", 
    "Ash", 
    "Hemicellulose", 
    "Glu Base Yield",  
    "Xyl Base Yield",  
    "glucose free", 
    "Glu Water Yielduncorr", 
    "Xyl Water Yield",
    "Glu Water Yield",
    "AcBr Lignin",
    "Density",
    "extensin",
    "Total Gluuncorr",
    "Total Xyl",
    "Klason Lignin",
    "Klason Ash",
    "MLG",
    "MLGnonnorm",
    "Total Glu",
    "Glu Base Efficiency",
    "Xyl Base Efficiency",
    "HRGP"
#    "Sugar Yield Base",
#    "Sugar Yield Water",
#    "Total Sugar",
#    "Sugar Base Efficiency",
#    "Sugar Water Efficiency"
    )

setwd("../output")
fileprefix = paste("AllData5LinesJMP_", format(Sys.time(),"%Y%m%d_%H%M"), sep="")
write.table(forjmp, file=paste(fileprefix, "tsv", sep="."), sep="\t", quote=F, row.names=F, col.names=T) 

# based on this in the GH we want to predict glucose base field, but we need transformed data to do it
topredictglufield = melt(alldata, id=c(1:4), na.rm=T)
topredictglufield = subset(topredictglufield, variable %in% c("glucose_base^-0.4", "klason_lig", "density^-2.2","HRGP", "totgluC^-1", "klason_ash^0.1"))
topredictglufield$par_enviro = do.call(paste, c(topredictglufield[c("variable","enviro")], sep="_"))
topredictglufield = dcast(topredictglufield[,c(4,2,6,7)], pretty.name + tissue ~ par_enviro, mean)
topredictglufield = topredictglufield[,c(1,2,4,5,8,10,12,14)]

fileprefix = "ForLinearModelstoPredictGlucoseField.tsv"
write.table(topredictglufield, file=fileprefix, sep="\t", quote=F, row.names=F, col.names=T) 

# for correlations between environments
forjmplong = melt(forjmp, id=c(1:3), na.rm=T)

forjmplong$par_enviro = do.call(paste, c(forjmplong[c("variable","enviro")], sep="_"))
forjmpcorrenviro = forjmplong[,c(3,1,6,5)]
forjmpcorrenvirofinal = dcast(forjmpcorrenviro, line + tissue ~ par_enviro, mean)

fileprefix = "AllData5LinesForCorrJMP_EnviroSeparate.tsv"
write.table(forjmpcorrenvirofinal, file=fileprefix, sep="\t", quote=F, row.names=F, col.names=T) 

# get and export data for pdiff(contrasts), lsmeans, and heritability estimates...
formodelstuff = alldata[,c(2:4,5,6,28,7,29,8,9,30,10,31,13,34,14,35,16,36,19,20,21,38,22,39,24,41,25,42,26,43,27)]
forpdiffetc = formodelstuff[,c(1:4,6,8,9,11,13,14,17,19,20,21,23,25,27,29,31,32)]
as.data.frame(colnames(forpdiffetc))
colnames(forpdiffetc) = c("tissue",
                      "enviro",
                      "line",
                      "Cellulose", 
                      "Lignin", 
                      "Ash", 
                      "Hemicellulose", 
                      "glucose_base",  
                      "pentose_base",   
                      "pentose_water",
                      "glucose_water",
                      "Density",
                      "Total_Xylose",
                      "Klason_Lignin",
                      "Klason_Ash",
                      "MLG",
                      "Total_Glucose",
                      "Glu_Efficiency",
                      "Pen_Efficiency",
                      "HRGP"
                      
                      
)

# fileprefix = paste("AllDataForLSMeansPdiff_", format(Sys.time(),"%Y%m%d_%H%M"), sep="")
# write.table(forpdiffetc, file=paste(fileprefix, "tsv", sep="."), sep="\t", quote=F, row.names=F, col.names=T) 
fileprefix = "AllDataForLSMeansPdiff.tsv"
write.table(forpdiffetc, file=fileprefix, sep="\t", quote=F, row.names=F, col.names=T) 

##################
## FOR GRAPHING
##################
forgraphs = alldata
as.data.frame(colnames(forgraphs))
forgraphs = forgraphs[,c(1:11,13:16,19,20:22,24:27)]
alldatalong = melt(forgraphs, id=c(1:4), na.rm=T)

# get summary data
summary.stats <- aggregate(value ~ line + tissue + enviro + pretty.name + variable, data=alldatalong, FUN=function(x) c(length(x), mean(x), sd(x), sd(x)/sqrt(length(x)), sd(x)/mean(x)))
# add names for new columns we calculated
summary.stats <- cbind(summary.stats[,1:5], as.data.frame(summary.stats[,6]))
names(summary.stats) <- c("line", "tissue", "enviro", "prettyname", "parameter", "n", "mean", "SD", "SE", "CV")

# change levels of parameters to look better
#as.data.frame(levels(summary.stats$parameter))
levels(summary.stats$parameter) = c("Cellulose", 
                                    "Lignin", 
                                    "Ash", 
                                    "Hemicellulose", 
                                    "glucose_base",  
                                    "pentose_base",  
                                    "glucose_free",  
                                    "pentose_water",
                                    "glucose_water",
                                    "AcBrLignin",
                                    "Density",
                                    "Total_Xylose",
                                    "Klason_Lignin",
                                    "Klason_Ash",
                                    "MLG",
                                    "Total_Glucose",
                                    "Glu_Efficiency",
                                    "Pen_Efficiency",
                                    "HRGP")

# add units column based on which parameter
summary.stats$units[summary.stats$parameter == "Cellulose"] = "% DM"
summary.stats$units[summary.stats$parameter == "Lignin"] = "% DM"
summary.stats$units[summary.stats$parameter == "Ash"] = "% DM"
summary.stats$units[summary.stats$parameter == "Hemicellulose"] = "% DM"
summary.stats$units[summary.stats$parameter == "glucose_base"] = "% AR"
summary.stats$units[summary.stats$parameter == "pentose_base"] = "% AR"
summary.stats$units[summary.stats$parameter == "glucose_free"] = "% AR"
summary.stats$units[summary.stats$parameter == "glucose_water"] = "% AR"
summary.stats$units[summary.stats$parameter == "pentose_water"] = "% AR"
summary.stats$units[summary.stats$parameter == "glucose_waterC"] = "% AR"
summary.stats$units[summary.stats$parameter == "AcBrLignin"] = "% AR"
summary.stats$units[summary.stats$parameter == "Density"] = "kg/m3"
summary.stats$units[summary.stats$parameter == "HRGP"] = "% AR"
summary.stats$units[summary.stats$parameter == "Total_Glucose"] = "% AR"
summary.stats$units[summary.stats$parameter == "Total_Xylose"] = "% AR"
summary.stats$units[summary.stats$parameter == "Klason_Lignin"] = "% AR"
summary.stats$units[summary.stats$parameter == "Klason_Ash"] = "% AR"
summary.stats$units[summary.stats$parameter == "MLG"] = "abs 405/490"
summary.stats$units[summary.stats$parameter == "Glu_Efficiency"] = "% Total Glu"
summary.stats$units[summary.stats$parameter == "Pen_Efficiency"] = "% Total Xyl"

# we can graph it now
# use pretty name..
summary.stats = summary.stats[,-1]
colnames(summary.stats)[colnames(summary.stats)=="prettyname"] = "line"

# sort by enviro, line, tissue, parameter
summary.stats = summary.stats[with(summary.stats, order(enviro, line, tissue, parameter)), ]
# rearrange in that order
summary.stats = summary.stats[, c(2,3,1,4,10,5:9)]

# output it
# fileprefix = paste("SummaryData5Lines_Rearrange_", format(Sys.time(),"%Y%m%d_%H%M"), sep="")
# write.table(summary.stats, file=paste(fileprefix, "tsv", sep="."), sep="\t", quote=F, row.names=F, col.names=T) 
fileprefix = "SummaryData5Lines_Rearrange.tsv"
write.table(summary.stats, file=fileprefix, sep="\t", quote=F, row.names=F, col.names=T) 

setwd("../fieldvsgh_final/")
