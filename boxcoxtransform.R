# All code written by Paul Tanger. Please cite appropriately.

# make list of phenotypes
varlist = names(alldata)[5:27]

# run models to check residuals..the model actually doesn't matter..
models = lapply(varlist, function(x) { 
  lm(substitute(i ~ 1, list(i=as.name(x))), data=alldata)
})

# name models in list..
names(models) = varlist

# return the response var from a model
getrespname = function(model){
  return (all.vars(terms(model
  ))[1])
}

# # qqplot of scaled residuals
# lapply(models, function(x) {
#   qqnorm(resid(x, scaled=T), main=paste("qqplot of ", getrespname(x), " scaled residuals"))
#   qqline(resid(x, scaled=T))
# })
# 
# # get histograms of scaled residuals
# lapply(models, function(x) {
#   hist(resid(x, scaled=T), breaks=10, main=paste("histogram of ", getrespname(x), " scaled residuals"), xlab="residuals")
# })

# maybe this works better
library(car)

# check normality
normaltests = apply(alldata[5:27], 2, shapiro.test)
normaltestssummary <- sapply(normaltests, `[`, c("statistic","p.value"))
normaltestsfinal = as.data.frame(t(normaltestssummary))

# setup colnumbers to process
normaltestsfinal$colnum = seq(5, nrow(normaltestsfinal)+4)

# skip AcBr, missing values..
normaltestsfinal = normaltestsfinal[-11,]

# if p < 0.05 then transform
normaltestsfinal$doit = normaltestsfinal$p.value < 0.05
normaltestsfinal = normaltestsfinal[normaltestsfinal$doit,]

for (i in normaltestsfinal$colnum) {
  # create model formula
  formula <- reformulate(" 1", response=as.name(colnames(alldata[i])))
  # calculate lambdas
  model = powerTransform(formula, data=alldata)
  # get max lambda
  maxlambda = coef(model, round=F)
  # round to nearest .1
  maxlambda = round(maxlambda, 1)
  # make new column name to contain lambda
  newcol = paste(colnames(alldata[i]),maxlambda, sep="_")
  # transform col as new col
  alldata = cbind(alldata, bcPower(alldata[i], maxlambda))
}

# check after transformation
normaltestsafter = apply(alldata[28:43], 2, shapiro.test)
normaltestsaftersummary <- sapply(normaltestsafter, `[`, c("statistic","p.value"))
normaltestsafterfinal = as.data.frame(t(normaltestsaftersummary))
