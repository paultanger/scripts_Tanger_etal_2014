# All code written by Paul Tanger. Please cite appropriately.

# disable scientific notation
options(scipen=999)

# load stuff
#install.packages("lme4")
library(lme4)
library(plyr)
library(reshape2)

# get variance components..
varcompsdf = function(model){
  varcomps = as.data.frame(print(VarCorr(model),comp=c("Std.Dev.","Variance")))
  varcomps = varcomps[,-c(2,3)]
  varcomps.component = varcomps[,1]
  varcomps.variance = varcomps[,2]
  varcomps.SD = varcomps[,3]
  varcompsdf = data.frame(varcomps.component, as.numeric(varcomps.variance), as.numeric(varcomps.SD))
  names(varcompsdf) = c("component", "variance", "SD")
  varcompsdf$varPCT = varcompsdf$variance/ sum(varcompsdf$variance)*100
  return(varcompsdf)
}

setwd("../output/")
alldata = read.delim("AllDataForLSMeansPdiff.tsv")

# split tissue..
leafdata = subset(alldata, tissue == "leaf")
stemdata = subset(alldata, tissue == "stem")

# create matrices of response variables...
leafresp  = as.matrix(cbind(leafdata[,c(4:20)]))
stemresp  = as.matrix(cbind(stemdata[,c(4:20)]))

varlist = names(alldata)[c(4:20)]

#############################
# overall
##############################
# or with line and enviro (no nesting)
leafmodels = lapply(varlist, function(x) { 
  lmer(substitute(i ~ + (1 | line) + (1 | enviro), list(i=as.name(x))), data=leafdata)
})

# name models in list..
leafnames = paste(varlist, "leaf", sep="_")
names(leafmodels) = leafnames

# or with line and enviro (no nesting)
stemmodels = lapply(varlist, function(x) { 
  lmer(substitute(i ~ + (1 | line) + (1 | enviro), list(i=as.name(x))), data=stemdata)
})

# name models in list..
stemnames = paste(varlist, "stem", sep="_")
names(stemmodels) = stemnames

# combine models into one big list
models = c(unlist(leafmodels), unlist(stemmodels))

# set names
modelnames = names(models)

# get variance components
varcomps = lapply(models, varcompsdf)
varcompsall = ldply(varcomps, data.frame)

# make new column for excel
varcompsall$varpctexcel = varcompsall$varPCT / 100
# rename col
colnames(varcompsall)[colnames(varcompsall)==".id"] = "parameter"
colnames(varcompsall)[colnames(varcompsall)=="varpctexcel"] = "H2"

# sort by parameter
varcompsall = varcompsall[with(varcompsall, order(parameter)), ]

# round values
varcompsall$H2 = formatC(round(varcompsall$H2, digits=2), 2, format="f")

# add separate columns for parameter and tissue
setwd("../input")
phenotypesforvarcomps = read.csv("phenotypesforVarCompsv4.csv")

# merge together
varcompsfinal = merge(varcompsall, phenotypesforvarcomps, all.x=T)
# remove extra cols
varcompsfinal = varcompsfinal[,c(7,8,2,3,6)]

# try to have columns for each component (make it wide)
varcompsfinalwide = dcast(varcompsfinal[,-4],phenotype + tissue ~ component, value.var="H2")
varcompsfinalwide = varcompsfinalwide[,c(1,2,4,3,5)]
colnames(varcompsfinalwide) = c("phenotype", "tissue", "H2", "% var environment", "% residual var")

# set custom levels
varcompsfinalwide$phenotype = factor(varcompsfinalwide$phenotype, c("Cellulose", "Lignin", "Hemicellulose", "Ash", 
                                                                    "Total Glucose", "Total Xylose", "Klason Lignin", "Klason Ash", 
                                                                    "glucose_base", "glucose_water", "pentose_base", "pentose_water", 
                                                                    "Glu Efficiency", "Pen Efficiency", "MLG",  "HRGP", "Density" ))

varcompsfinalwidesorted = varcompsfinalwide[order(varcompsfinalwide$phenotype),]
# save it
setwd("../output")
write.table(varcompsfinalwidesorted, file="Table2.tsv", sep="\t", quote=F, row.names=F, col.names=T) 

#############################
# by environment
##############################

leafdataGH = subset(leafdata, enviro == "GH")
stemdataGH = subset(stemdata, enviro == "GH")
leafdatafield = subset(leafdata, enviro == "field")
stemdatafield = subset(stemdata, enviro == "field")
leafrespGH  = as.matrix(cbind(leafdataGH[,c(4:20)]))
stemrespGH  = as.matrix(cbind(stemdataGH[,c(4:20)]))
leafrespF  = as.matrix(cbind(leafdatafield[,c(4:20)]))
stemrespF  = as.matrix(cbind(stemdatafield[,c(4:20)]))

leafmodelsGH = lapply(varlist, function(x) { lmer(substitute(i ~ + (1 | line), list(i=as.name(x))), data=leafdataGH) })
stemmodelsGH = lapply(varlist, function(x) { lmer(substitute(i ~ + (1 | line), list(i=as.name(x))), data=stemdataGH) })
leafmodelsF = lapply(varlist, function(x) { lmer(substitute(i ~ + (1 | line), list(i=as.name(x))), data=leafdatafield) })
stemmodelsF = lapply(varlist, function(x) { lmer(substitute(i ~ + (1 | line), list(i=as.name(x))), data=stemdatafield) })

leafGHnames = paste(varlist, "leafGH", sep="_")
stemGHnames = paste(varlist, "stemGH", sep="_")
leafFnames = paste(varlist, "leafF", sep="_")
stemFnames = paste(varlist, "stemF", sep="_")

names(leafmodelsGH) = leafGHnames
names(stemmodelsGH) = stemGHnames
names(leafmodelsF) = leafFnames
names(stemmodelsF) = stemFnames
models = c(unlist(leafmodelsGH), unlist(stemmodelsGH), unlist(leafmodelsF), unlist(stemmodelsF))
modelnames = names(models)
varcomps = lapply(models, varcompsdf)
varcompsall = ldply(varcomps, data.frame)

#do stuff like for below..
varcompsall$varpctexcel = varcompsall$varPCT / 100
colnames(varcompsall)[colnames(varcompsall)==".id"] = "parameter"
colnames(varcompsall)[colnames(varcompsall)=="varpctexcel"] = "H2"
varcompsall = varcompsall[with(varcompsall, order(parameter)), ]
varcompsall$H2 = formatC(round(varcompsall$H2, digits=2), 2, format="f")
setwd("../input")
phenotypesforvarcomps = read.csv("phenotypesforVarCompsv4ENVIROSEP.csv")
varcompsfinal = merge(varcompsall, phenotypesforvarcomps, all.x=T)
varcompsfinal = varcompsfinal[,c(8,9,11,2,6)]
varcompsfinalwide = dcast(varcompsfinal, phenotype + tissue ~ enviro + component, value.var="H2")
varcompsfinalwide$phenotype = factor(varcompsfinalwide$phenotype, c("Cellulose", "Lignin", "Hemicellulose", "Ash", 
                                                                    "Total Glucose", "Total Xylose", "Klason Lignin", "Klason Ash", 
                                                                    "glucose_base", "glucose_water", "pentose_base", "pentose_water", 
                                                                    "Glu Efficiency", "Pen Efficiency", "MLG",  "HRGP", "Density" ))


varcompsfinalwidesorted = varcompsfinalwide[order(varcompsfinalwide$phenotype),]
setwd("../output")
write.table(varcompsfinalwidesorted, file="Table3.tsv", sep="\t", quote=F, row.names=F, col.names=T) 

setwd("../fieldvsgh_final/")