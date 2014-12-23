# All code written by Paul Tanger. Please cite appropriately.

getmodelvec = function(model, digit=2) {
  
  # get parameters
  intercept = as.numeric(format(coef(model)[1], digits=digit))
  coefficient = as.numeric(format(coef(model)[2], digits=digit))
  depvar = as.name(all.vars(terms(model))[2])
  response = as.name(all.vars(terms(model))[1])
  
  # make equation
  equation = paste0(response, " = ", coefficient, " * ", depvar,
                    paste(sprintf(" %.1+f", intercept, collapse=""))
  )
  
  # get other stuff
  r2 = as.numeric(format(summary(model)$r.squared, digits=digit))
  f = summary(model)$fstatistic
  p = as.numeric(format(pf(f[1],f[2],f[3],lower.tail=F), digits=digit))
  
  # and test that residuals are normal
  resid = residuals(model)
  normaltestpval = format(shapiro.test(resid)$p.value, digits=digit)
  
  # create vector
  vectortoadd = c(equation, r2, p, normaltestpval)
  vectortoadd = t(vectortoadd)
  return(vectortoadd)
}