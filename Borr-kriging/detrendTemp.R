# detrendTemp.R
# 
# Author: Michael
###############################################################################
transdf <- function(rhs, df, initparams){
	tempfit <- nls(y ~ rhs(x, intercept, power), data = ds, start = list(intercept = 0,
					+ power = 2), trace = T)
	
	
	df$temperature_avgmin_resid <- apply(df, 1, transtemp)
}


