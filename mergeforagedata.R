# All code written by Paul Tanger. Please cite appropriately.

setwd("../input")

# setup filenames
foragefile = "ForageData.XLS"   # original forage analysis data file from UCD

# import data.. default colClasses is NA... otherwise specify as char or numeric not factors..
foragedata    = read.xlsx(foragefile, header=F, stringsAsFactors=F, sheetName="REPORT", 
                          colIndex=c(1,3:8), rowIndex=14:84,
                          colClasses=c(NA, rep("numeric", 7)))

# remove duplicates.. they didn't have enough sample to do them..
# also note that results are reported on 100% Dry Matter, based on estimating dry matter in
# samples 5, 10, 15, etc ... dry matter was reported as 94.9%
foragedata = foragedata[!is.na(foragedata$X3),]

# give col names
colnames(foragedata) = c("ID", "cellulose", "ligninWash", "ligninAshFree", "NDF", "ash", "ADF")

# calculate hemicellulose
foragedata$hemicellulose = foragedata$NDF - foragedata$ADF

# merge with sample info
# this comes from Paul T's access database "alldata_compiledv3"
sampleinfo = read.delim("sampleinfo.tsv", header=T)
colnames(sampleinfo)[colnames(sampleinfo)=="X..plants"] = "num_plants"

# UCD required IDs from 1-100.. here is how it translates to our IDs.. also from Paul T's access db
UCDtoCSU = read.delim("UCD_to_CSU_ID_table_forage_data.tsv")

# proper variety names - from Jahn et al 2011 Plant Physiology
linenames = read.csv("VarietyInfo.csv")

# rename ID cols to match
names(sampleinfo)[names(sampleinfo) == "SampleID"] <- "ID"
names(UCDtoCSU)[names(UCDtoCSU) == "OurIDforaccess"] <- "ID"

# merge sample info with UCD ID
sampleinfofinal = merge(sampleinfo, UCDtoCSU, by="ID", all.x=T)

# merge with proper variety names
sampleinfofinal = merge(sampleinfofinal, linenames, by="line", all.x=T)

# merge data with metadata
names(foragedata)[names(foragedata) == "ID"] <- "IDforUCD"
# note that sample 61 IR64 GH (a control sample) becomes NA..
foragedatafinal = merge(sampleinfofinal, foragedata, by="IDforUCD", all.y=T)
# discard sample 61..
foragedatafinal = foragedatafinal[!is.na(foragedatafinal$ID),]

# remove IDforUCD
foragedatafinal = foragedatafinal[,-1]
# now get means of that technical rep
library(reshape2)
foragedatafinallong = melt(foragedatafinal, id=c(1:13))
foragedatafinal = dcast(foragedatafinallong, line + ID + tissue + enviro + pooled + num_plants + forage + pretty.name + IRGC + full.name + Country.of.origin + Varietal.Group + Variety.Class ~ variable, mean)

# merge with other data
setwd("../fieldvsgh_final")
