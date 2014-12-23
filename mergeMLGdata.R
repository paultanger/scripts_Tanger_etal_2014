# All code written by Paul Tanger. Please cite appropriately.

setwd("../input")

# setup filenames
MLGfile   = "MLG ELISA sugar normalized.xlsx"
MLGfilenon = "MLG ELISA non sugar normalized.xlsx"

eMLG = read.xlsx(MLGfile, header=T, stringsAsFactors=F, 
                 sheetName="MLG ELISA sugar normalized 4-20", 
                 colIndex=c(1,6), rowIndex=14:81)

eMLGnon = read.xlsx(MLGfilenon, header=T, stringsAsFactors=F, 
                    sheetName="MLG ELISA non sugar normalized ", 
                    colIndex=c(1,6), rowIndex=15:82)

# rename cols before merge
colnames(eMLG)[colnames(eMLG)=="Ratio"] = "norm_ratio"
colnames(eMLGnon)[colnames(eMLGnon)=="Ratio"] = "non_norm_ratio"

# merge
eMLGmerge = merge(eMLG, eMLGnon, by="sample")

# remove extra label row
eMLGmerge = eMLGmerge[-67,]

# remove dash & rep from sample col
tempID   = do.call(rbind, strsplit(eMLGmerge[,1], "-"))
eMLGmerge[,1] = as.data.frame(as.numeric(tempID[,1])) # do the actual replacement

# sample "8" must actually be 18, because sample 8 never existed..
eMLGmerge$sample[eMLGmerge$sample == 8] = 18

# change to numeric
eMLGmerge$norm_ratio = as.numeric(eMLGmerge$norm_ratio)
eMLGmerge$non_norm_ratio = as.numeric(eMLGmerge$non_norm_ratio)

# make long
eMLGmergeL = melt(eMLGmerge, id="sample")

# get summary data.... check tech rep stuff.. then merge next...
techmeans <- aggregate(value ~ sample + variable, data=eMLGmergeL, FUN=function(x) c(length(x), mean(x), sd(x), sd(x)/sqrt(length(x)), sd(x)/mean(x)))
# add names for new columns we calculated
techmeans <- cbind(techmeans[,1:2], as.data.frame(techmeans[,3]))
names(techmeans) <- c("ID", "parameter", "n", "mean", "SD", "SE", "CV")

# just keep means to merge with other data
eMLGmeans = techmeans[,c(1,2,4)]

# make wide
eMLGfinal = dcast(eMLGmeans, ID ~ parameter, mean)

# rename cols before merging with everything else
colnames(eMLGfinal)[colnames(eMLGfinal)=="norm_ratio"] = "MLG_abs_nrmlzd"
colnames(eMLGfinal)[colnames(eMLGfinal)=="non_norm_ratio"] = "MLG_abs_NONnrmlzd"

# merge with sample info
setwd("../fieldvsgh_final")
