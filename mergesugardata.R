# All code written by Paul Tanger. Please cite appropriately.

setwd("../input")
# well rep is the 3 technical reps on the plate
# assay rep is the 4 absorbance readings of glucose or pentose (nested within well reps)

# setup filenames
set1base   = "SugarData_Base6mM.xls"
set1free   = "SugarData_FreeGluc.xls"
set1water  = "SugarData_Water.xls"
set2       = "SugarData_Set2.xls"

# get data - base pretreatment
set1bG <-read.xlsx(set1base,  stringsAsFactors=T,sheetName="ProG", colIndex=1:6, colClasses=c(rep("character", 2), rep("numeric", 4)))
set1bP <-read.xlsx(set1base,  stringsAsFactors=T,sheetName="ProP", colIndex=1:6, colClasses=c(rep("character", 2), rep("numeric", 4)))
# get data - this is glucose without pretreatment - just the ProG tab 
set1fG <-read.xlsx(set1free,  stringsAsFactors=T,sheetName="ProG", colIndex=1:6, colClasses=c(rep("character", 2), rep("numeric", 4)))
# get data - hot water pretreatment
set1wG <-read.xlsx(set1water,  stringsAsFactors=T,sheetName="ProG", colIndex=1:6, colClasses=c(rep("character", 2), rep("numeric", 4)))
set1wP <-read.xlsx(set1water,  stringsAsFactors=T,sheetName="ProP", colIndex=1:6, colClasses=c(rep("character", 2), rep("numeric", 4)))
# get data - the second set of field samples... base, free, water all in same tabs..
set2G <-read.xlsx(set2,  stringsAsFactors=F,sheetName="ProG", colIndex=1:6, colClasses=c(rep("character", 2), rep("numeric", 4)))
set2P <-read.xlsx(set2,  stringsAsFactors=F,sheetName="ProP", colIndex=1:6, colClasses=c(rep("character", 2), rep("numeric", 4)))

###########
# first cleanup the second set of data
# make a little function to process all the same

cleanset2 = function(df){
  # remove extra rows that got imported
  df  = df[!is.na(df$Input),]
  # split the input column
  split = matrix(unlist(strsplit(df$Input, "_")), ncol=2, byrow=T)
  df = cbind(df, as.data.frame(split))
  # remove Nick's control samples
  df = df[grep("^I",df$V2),]
  # remove unneeded input column
  df = df[,c(2:ncol(df))]
  colnames(df)[colnames(df)=="V1"] = "treatment"
  colnames(df)[colnames(df)=="V2"] = "Input"
  return(df)
}

# clean set2
set2Gclean = cleanset2(set2G)
set2Pclean = cleanset2(set2P)

# split into dfs for each treatment - easier to combine with set1
set2bG = subset(set2Gclean, treatment== "Base", select=c(7,1:5))
set2fG = subset(set2Gclean, treatment== "Free", select=c(7,1:5))
set2wG = subset(set2Gclean, treatment== "Water", select=c(7,1:5))
set2bP = subset(set2Pclean, treatment== "Base", select=c(7,1:5))
set2wP = subset(set2Pclean, treatment== "Water", select=c(7,1:5))

#########################
# now for set1...
# make a little function to process all the same
cleanset = function(df){
  # remove extra rows that got imported
  df = df[!is.na(df$Input),]
  # rename col
  colnames(df)[colnames(df)=="Input"] = "barcode"
  colnames(df)[colnames(df)=="Output"] = "well_rep"
  # remove Nick's control samples
  df = df[grep("^I",df$barcode),]
  # convert to long format
  longdf = melt(df, id.vars=c("barcode", "well_rep"))
  # convert to percent
  longdf$value = longdf$value * 100
  # split assay rep col
  longdf$rep = as.numeric(substring(longdf$variable, 2,2))
  # drop unneeded col
  longdf = subset(longdf, select=-variable)
  return(longdf)
}

# clean set 1
set1bG     = cleanset(set1bG)
set1bP     = cleanset(set1bP)
set1fG     = cleanset(set1fG)
set1wG     = cleanset(set1wG)
set1wP     = cleanset(set1wP)

# clean set 2
set2bG     = cleanset(set2bG)
set2bP     = cleanset(set2bP)
set2fG     = cleanset(set2fG)
set2wG     = cleanset(set2wG)
set2wP     = cleanset(set2wP)

# combine together
glubase = rbind(set1bG,set2bG)
penbase = rbind(set1bP,set2bP)
freeglu = rbind(set1fG,set2fG)
gluwate = rbind(set1wG,set2wG)
penwate = rbind(set1wP,set2wP)

# you can check stuff here.. like tech rep variation of assay reps and well reps
glubase$parameter  = "glubase"
penbase$parameter  = "penbase"
freeglu$parameter  = "freeglu"
gluwate$parameter  = "gluwate"
penwate$parameter  = "penwate"

# a little functions to get the means of each barcode rep 
# (we are getting means of 12 points - 3 well reps of 4 assay reps)
getmeans = function(df){
  df = aggregate(value ~ barcode, data=df, mean)
  return(df)
}

# get means
glubase     = getmeans(glubase)
penbase     = getmeans(penbase)
freeglu     = getmeans(freeglu)
gluwate     = getmeans(gluwate)
penwate     = getmeans(penwate)

# give specific names to value cols before merge
colnames(glubase)[colnames(glubase)=="value"] = "glucose_base"
colnames(penbase)[colnames(penbase)=="value"] = "pentose_base"
colnames(freeglu)[colnames(freeglu)=="value"] = "glucose_free"
colnames(gluwate)[colnames(gluwate)=="value"] = "glucose_water"
colnames(penwate)[colnames(penwate)=="value"] = "pentose_water"

# merge them together
tomerge = list(glubase,penbase,freeglu,gluwate,penwate)
allnickdata = Reduce(function(...) merge(..., all=T), tomerge)

# now merge with sample info
# first with barcode to ourID
barcodes = read.csv("sugareleasebarcodes.csv", header=T)
datawithID = merge(barcodes[1:2], allnickdata, by="barcode", all=T)

# next with full sample info
sampleinfo = read.delim("sampleinfo.tsv", header=T)
colnames(sampleinfo)[colnames(sampleinfo)=="X..plants"] = "num_plants"
nickdata = merge(sampleinfo, datawithID, by.x="SampleID", by.y="ourID", all.y=T) 

# discard sample 51.. it was a control
nickdata = nickdata[!is.na(nickdata$line),]

# subtract free glucose from hot water treated glucose
nickdata$glucose_waterC = nickdata$glucose_water - nickdata$glucose_free

# convert to long format and get means for some duplicates.. they had tech reps..or were run in both set 1 & 2
nickdatalong = melt(nickdata[,c(1,9:14)], id="SampleID")
nickdatafinal = dcast(nickdatalong, SampleID ~ variable, mean)

# merge with other data
setwd("../fieldvsgh_final")