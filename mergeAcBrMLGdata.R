# All code written by Paul Tanger. Please cite appropriately.

setwd("../input")

# load libraries
#install.packages("xlsx")
library(xlsx)
#install.packages("rJava")
#library(rJava)

# setup filenames
acbr  = "AcBrLigninData.xlsx"
mlg   = "MLG_kitmethod.xlsx"

# get data - base pretreatment
acbrdata = read.xlsx(acbr,  stringsAsFactors=F,sheetName="Sheet1", rowIndex=16:80 , colIndex=c(1,10))
mlgdata1 = read.xlsx(mlg,  stringsAsFactors=F, sheetName="Sheet1", rowIndex=1:42, colIndex=1:2)
mlgdata2 = read.xlsx(mlg,  stringsAsFactors=F, sheetName="Sheet1", rowIndex=46:73, colIndex=3:4)
colnames(mlgdata1) = c("ID", "mlg")
colnames(mlgdata2) = c("ID", "mlg")
# get rid of A, B, C etc
mlgdata1$ID = substring(mlgdata1$ID, 1, nchar(mlgdata1$ID)-1)
# merge parts
mlgdata = rbind(mlgdata1,mlgdata2)
# fix acbr
colnames(acbrdata) = c("ID", "acbr")
# get means of tech reps
mlgmeans = aggregate(mlg ~ ID, data=mlgdata, mean, na.action=na.omit)
acbrmeans = aggregate(acbr ~ ID, data=acbrdata, mean, na.action=na.omit)
# miguel says if mlg < 0 then make it zero
mlgmeans$mlg[mlgmeans$mlg < 0] = 0
# merge together
mlgacbrdata = merge(mlgmeans, acbrmeans, by="ID", all=T)
# get rid of sample 51 - a control sample
mlgacbrdata = mlgacbrdata[!mlgacbrdata$ID == 51,]

# merge with sample info
setwd("../fieldvsgh_final")
