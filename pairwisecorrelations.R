# All code written by Paul Tanger. Please cite appropriately.

setwd("../input/")
jmpcorrs = read.csv("AllcorrFINAL.csv")

# we need columns next to eachother
GH    = subset(jmpcorrs, enviro == "GH")
field = subset(jmpcorrs, enviro == "field")
all = subset(jmpcorrs, enviro == "All")

corrwide1 = merge(GH, field, by=c("Variable", "by.Variable"), suffixes=c("GH","Field"))
corrwide = merge(corrwide1, all, by=c("Variable", "by.Variable"), suffixes=c("x","Both"))
setwd("../output")
write.table(corrwide, file="pairwisecorrelationsforTablesS7andS8.tsv", sep="\t", quote=F, row.names=F, col.names=T) 

setwd("../fieldvsgh_final/")