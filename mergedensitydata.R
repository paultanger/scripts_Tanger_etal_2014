# All code written by Paul Tanger. Please cite appropriately.

setwd("../input")
# 3 tech reps of each sample..

# setup filenames
densityfile   = "BulkDensityData.xlsx"

# get data - base pretreatment
density = read.xlsx(densityfile,  stringsAsFactors=T, 
                    sheetName="final", colIndex=c(1,5), 
                    colClasses=c("character", "numeric"))

# rename cols
colnames(density) = c("SampleID", "density_g_ml")

# get stats of 3 tech reps
summary.stats <- aggregate(density_g_ml ~ SampleID, data=density, 
                           FUN=function(x) c(length(x), mean(x), sd(x), sd(x)/sqrt(length(x)), sd(x)/mean(x)))
summary.stats <- cbind(summary.stats[,1], as.data.frame(summary.stats[,2]))
names(summary.stats) <- c("SampleID", "n_tech_reps", "mean", "SD", "SE", "CV")

# prep means for merge
densitymeans = summary.stats[,c(1,3)]

# next with full sample info
sampleinfo = read.delim("sampleinfo.tsv", header=T)
colnames(sampleinfo)[colnames(sampleinfo)=="X..plants"] = "num_plants"
densitydata = merge(sampleinfo, densitymeans, all.y=T) 

colnames(densitymeans)[colnames(densitymeans)=="mean"] = "density"

# merge with other data...
setwd("../fieldvsgh_final")