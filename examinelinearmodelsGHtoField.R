# All code written by Paul Tanger. Please cite appropriately.

source("getmodeldf.R")

setwd("../output/")
data = read.delim("ForLinearModelstoPredictGlucoseField.tsv")
vars = as.data.frame(colnames(data))

# fit models
dens = lm(`glucose_base..0.4_field` ~ `density..2.2_GH`, data = data)
klign = lm(`glucose_base..0.4_field` ~ `klason_lig_GH`, data = data)
totglu = lm(`glucose_base..0.4_field` ~ `totgluC..1_GH`, data = data)
HRGP = lm(`glucose_base..0.4_field` ~ `HRGP_GH`, data = data)
kash = lm(`glucose_base..0.4_field` ~ `klason_ash.0.1_GH`, data = data)

# combine into a big list to extract stuff
models = list(dens, klign, totglu, HRGP, kash)

# load functions to get resp name
getrespname = function(model){
  return (all.vars(terms(model))[1])
}

# get formula
getformula = function(model){
  return (formula(model))
}

# another way to do qqplots
# pdf('GHmodelstoFieldqqplots.pdf', width=10, height=8)
# lapply(models, function(x) {
#   plot(x, 2, id.n=0, add.smooth=F, pch=19)
# })
# dev.off()

# scaled resid vs predict
# pdf('GHmodelstoFieldresidplots.pdf', width=10, height=8)
# lapply(models, function(x) {
#   plot(x, 1, id.n=0, add.smooth=F, pch=19)
# })
# dev.off()

# now build a table with important data:
# create empty df
modeldata <- data.frame(equation = character(), 
                        R2 = integer(), 
                        pval = integer(),
                        residnormalpval = integer(),
                        stringsAsFactors=F
                        )

# loop through models
for (i in models) {
  modeldata = rbind(modeldata, as.data.frame(getmodelvec(i), stringsAsFactors=F))
}

# fix colnames
colnames(modeldata) = c("equation", "R2", "pval", "residnormalpval")

# export it
fileprefix = "TableS7.tsv"
write.table(modeldata, file=fileprefix, sep="\t", quote=F, row.names=F, col.names=T) 

setwd("../fieldvsgh_final/")
