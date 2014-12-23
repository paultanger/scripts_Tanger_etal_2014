# All code written by Paul Tanger. Please cite appropriately.

setwd("../input")
# setup filenames
decon = "AcidHydrolysisData.xls"

# get data - base pretreatment
gluxyl = read.xlsx(decon,  stringsAsFactors=F,sheetName="forRtechrepsgluxyl", colIndex=c(22,24:26))
ligash = read.xlsx(decon,  stringsAsFactors=F,sheetName="forRtechrepslignash", colIndex=c(19,21:23))

# discard glu & xyl that are not our samples
gluxyl = gluxyl[is.na(gluxyl$notes),]

# discard lig & ash that are not our samples
ligash = ligash[!is.na(ligash$ID),]

# discard lig & ash if less than 0 or greater than 90% - this is impossible.. and discard entire row..
ligash2 = subset(ligash, lignin > 0 & lignin < 100)
ligash3 = subset(ligash2, ash > 0 & ash < 100)

# now look for outliers
library(car)
# boxplot(as.vector(gluxyl$glucose))
outlierTest(lm(glucose ~ 1, data=gluxyl), cutoff=0.05, n.max=Inf)
# these look ok..
# boxplot(as.vector(gluxyl$xylose))
outlierTest(lm(xylose ~ 1, data=gluxyl), cutoff=0.05, n.max=Inf)
# this one is a tech rep of sample 4.. and other samples are not close.. so delete it
gluxyl[gluxyl$ID == 4,]
gluxyl = gluxyl[!(gluxyl$ID == 4 & gluxyl$xylose > 19),]
# boxplot(as.vector(ligash3$lignin))
outlierTest(lm(lignin ~ 1, data=ligash3), cutoff=0.05, n.max=Inf)
# lignin above 25% is probably not real, especially if other tech reps don't support it
# also, other biological field samples don't support lignin that high
ligash = ligash3[!(ligash3$lignin > 25),]
# boxplot(as.vector(ligash3$ash))
outlierTest(lm(ash ~ 1, data=ligash3), cutoff=0.05, n.max=Inf)
# looks ok

# make data sets long
gluxylL = melt(gluxyl[,c(1,3,4)], id="ID")
ligashL = melt(ligash[,c(1,3,4)], id="ID")

# combine together
decondata = rbind(gluxylL,ligashL)

# get summary data.... check tech rep stuff.. then merge next...
techmeans <- aggregate(value ~ ID + variable, data=decondata, FUN=function(x) c(length(x), mean(x), sd(x), sd(x)/sqrt(length(x)), sd(x)/mean(x)))
# add names for new columns we calculated
techmeans <- cbind(techmeans[,1:2], as.data.frame(techmeans[,3]))
names(techmeans) <- c("ID", "parameter", "n", "mean", "SD", "SE", "CV")

# just keep means to merge with other data
deconmeans = techmeans[,c(1,2,4)]

# make wide
deconfinal = dcast(deconmeans, ID ~ parameter, mean)

# rename cols before merging with everything else
colnames(deconfinal)[colnames(deconfinal)=="glucose"] = "tot_glu"
colnames(deconfinal)[colnames(deconfinal)=="xylose"] = "tot_xyl"
colnames(deconfinal)[colnames(deconfinal)=="lignin"] = "klason_lig"
colnames(deconfinal)[colnames(deconfinal)=="ash"] = "klason_ash"

# merge with sample info
setwd("../fieldvsgh_final")
