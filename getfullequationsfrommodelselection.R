# All code written by Paul Tanger. Please cite appropriately.

setwd("../output/")
forJMPmodelselection = read.delim("DataforModelSelectionJMP.tsv")

# split into two groups of data: forage and decon:
forage = forJMPmodelselection[,c(11,4,6,8,9,19,25,32)]
decon = forJMPmodelselection[,c(11,27,20,21,23,19,25,32)]

#install.packages("glmulti")
library(glmulti)
# create model with all parameters
forage.global = glm(Glucose_base ~ ., data=forage)
decon.global = glm(Glucose_base ~ ., data=decon)
# sort all models
# how many models?
maxmodels = 100
forage.model = glmulti(forage.global, level=1, crit="aicc", confsetsize=maxmodels, plotty=F)
decon.model = glmulti(decon.global, level=1, crit="aicc", confsetsize=maxmodels, plotty=F)

# check all
foragemodels = weightable(forage.model)
deconmodels = weightable(decon.model)

# limit to models < 10 from best
foragemax = foragemodels$aicc[1] + 10
deconmax = deconmodels$aicc[1] + 10

foragemodels = foragemodels[foragemodels$aicc < foragemax,]
deconmodels = deconmodels[deconmodels$aicc < deconmax,]

# figure out how many models to keep
maxforagemodels = length(foragemodels$aicc)
maxdeconmodels = length(deconmodels$aicc)

# rerun
forage.model = glmulti(forage.global, level=1, crit="aicc", confsetsize=maxforagemodels, plotty=F)
decon.model = glmulti(decon.global, level=1, crit="aicc", confsetsize=maxdeconmodels, plotty=F)

# disable sci notation
options(scipen=999)

# put everything in a list to access AICc inside loop
modellist = summary(forage.model)
library(reshape2)

foragetable = data.frame()

# for each model
for (i in 1:length(forage.model@objects)) {
  # get coeff
  coeff_df = as.data.frame(coef(summary(forage.model@objects[[i]])))
  # keep what we need
  coeff_df = as.data.frame(cbind(rownames(coeff_df),coeff_df[,1]))
  # add model name
  coeff_df$model = i
  # add AICc
  coeff_df$AICc = modellist$icvalues[i]
  # make numbers
  coeff_df$V2 = as.numeric(as.character(coeff_df$V2))
  # add to big table
  foragetable = rbind(foragetable, coeff_df)
}

# add ash model
ash = glm(Glucose_base ~ Ash, data=forage)
# get AICc
# install.packages("AICcmodavg")
library(AICcmodavg)
# summary(ash)
# AICc(ash)
# add this stuff manually

# get best model without ash
bestnoash = glm(Glucose_base ~ Cellulose + Lignin + Density, data=forage)
# summary(bestnoash)
# AICc(bestnoash)
# add this stuff manually

# make big table wide
foragewide = dcast(foragetable, model ~ V1, value.var="V2", mean, drop=F)

# add AICc
foragewide$AICc = modellist$icvalues

# calculate delta AICc
foragewide$deltaAICc = foragewide$AICc - foragewide$AICc[1]

# scale some columns.. use max and min..
# summary(foragewide)
foragewide$`(Intercept)` = round(foragewide$`(Intercept)`, 2)
foragewide$Ash = round(foragewide$Ash, 2)
foragewide$Hemicellulose = round(foragewide$Hemicellulose, 3)
foragewide$Hemicellulose = round(foragewide$Hemicellulose, 3)
foragewide$MLG = round(foragewide$MLG, 3)
foragewide$Density = round(foragewide$Density, 2)
foragewide$HRGP = round(foragewide$HRGP, 2)
foragewide$Cellulose = round(foragewide$Cellulose, 4)
foragewide$Lignin = round(foragewide$Lignin, 3)
foragewide$AICc = round(foragewide$AICc, 2)
foragewide$deltaAICc = round(foragewide$deltaAICc, 2)

fileprefix = "TableS10.tsv"
write.table(foragewide, file=fileprefix, sep="\t", na="", quote=T, row.names=F, col.names=T) 

#########################
# repeat for decon
# put everything in a list to access AICc inside loop
modellist = summary(decon.model)

decontable = data.frame()

# for each model
for (i in 1:length(decon.model@objects)) {
  # get coeff
  coeff_df = as.data.frame(coef(summary(decon.model@objects[[i]])))
  # keep what we need
  coeff_df = as.data.frame(cbind(rownames(coeff_df),coeff_df[,1]))
  # add model name
  coeff_df$model = i
  # add AICc
  coeff_df$AICc = modellist$icvalues[i]
  # make numbers
  coeff_df$V2 = as.numeric(as.character(coeff_df$V2))
  # add to big table
  decontable = rbind(decontable, coeff_df)
}

# add ash model
Kash = glm(Glucose_base ~ Klason_Ash, data=decon)
# summary(Kash)
# get AICc
# AICc(Kash)
# add this stuff manually

# get best model without ash
bestnoash = glm(Glucose_base ~ Total_Glucose + MLG + HRGP, data=decon)
# summary(bestnoash)
# AICc(bestnoash)
# add this stuff manually

# make big table wide
deconwide = dcast(decontable, model ~ V1, value.var="V2", mean, drop=F)

# add AICc
deconwide$AICc = modellist$icvalues
# calculate delta AICc
deconwide$deltaAICc = deconwide$AICc - deconwide$AICc[1]

# scale some columns.. use max and min..
# summary(deconwide)
deconwide$`(Intercept)` = round(deconwide$`(Intercept)`, 2)
deconwide$Density = round(deconwide$Density, 2)
deconwide$HRGP = round(deconwide$HRGP, 2)
deconwide$Klason_Ash = round(deconwide$Klason_Ash, 2)
deconwide$Total_Glucose = round(deconwide$Total_Glucose, 2)
deconwide$Total_Xylose = round(deconwide$Total_Xylose, 3)
deconwide$MLG = round(deconwide$MLG, 3)
deconwide$Klason_Lignin = round(deconwide$Klason_Lignin, 5)
deconwide$AICc = round(deconwide$AICc, 2)
deconwide$deltaAICc = round(deconwide$deltaAICc, 2)

# export it
fileprefix = "TableS11.tsv"
write.table(deconwide, file=fileprefix, sep="\t", na="", quote=T, row.names=F, col.names=T) 

setwd("../fieldvsgh_final/")