# All code written by Paul Tanger. Please cite appropriately.

setwd("../input")

# get the raw tech rep of the tech reps..
set1techreps = "HRGPpart1.xlsx"
set1 = read.xlsx(set1techreps, stringsAsFactors=F, 
                 sheetName="Sheet1", colIndex=c(1,2,4,6:8) )

# get rid of NA
set1 = set1[!is.na(set1$Hyp.ug.g.Assay.3.3),]

# merge with my sample IDs.. she used the original sampleIDs from the greenhouse sometimes..
originalID_toSampleID = read.csv("SampleInfo_GH_w_original_ID.txt")

# merge by tissue and ID..
set1merged = merge(set1, originalID_toSampleID, 
                   by.x=c("Original.line.number", "tissue"), 
                   by.y=c("original_ID", "tissue"), all.x=T)

# add field values to tube ID col when tubeID col is NA
# change from factor
set1merged$Original.line.number = as.character(set1merged$Original.line.number)
# when tube is NA, replace with field ID
set1merged$tube_ID[is.na(set1merged$tube_ID)] = set1merged$Original.line.number[is.na(set1merged$tube_ID)]

# keep only info we need
set1 = set1merged[,c(9,4:6)]
colnames(set1) = c("tube_ID", "rep1", "rep2", "rep3")
set1$assay = 1

###########################
# another rep is here
set2 = "HRGPpart2.xlsx"
set2 = read.xlsx(set2,  stringsAsFactors=F, sheetName="all data", colIndex=c(1:4) )

colnames(set2) = c("tube_ID", "rep1", "rep2", "rep3")
set2$assay = 2

###########################
# another rep is here
set3 = "HRGPpart3.xlsx"
set3 = read.xlsx(set3,  stringsAsFactors=F, sheetName="all data", colIndex=c(1:4) )

colnames(set3) = c("tube_ID", "rep1", "rep2", "rep3")
set3$assay = 3

# get rid of NA
set3 = set3[!is.na(set3$tube_ID),]

# combine together
set1$tube_ID = as.numeric(set1$tube_ID)
hypdata = rbind(set1,set2,set3)

# convert to long
hypdatalong = melt(hypdata, id=c("tube_ID", "assay"))
hypdatalong$value = as.numeric(hypdatalong$value)

# get means for each sample.. to merge in the merge all script..
hypfinal = aggregate(value ~ tube_ID, data=hypdatalong, FUN=function(x) c(length(x), mean(x), sd(x), sd(x)/sqrt(length(x)), sd(x)/mean(x)))
hypfinal <- cbind(hypfinal[,1], as.data.frame(hypfinal[,2]))
names(hypfinal) <- c("tubeID", "n_assay_reps", "hyp", "SD", "SE", "CV")
hypfinal$CVpercent = hypfinal$CV * 100

# merge with other data...
setwd("../fieldvsgh_final")
