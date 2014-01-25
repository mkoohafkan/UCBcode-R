#######################################################################
# name: borr-modelfuncs.r
# author: Michael Koohafkan
# depends: 
# purpose: using a specific table in the database, fit models to climate
# data using specified predictors and glmulti package. Convert AICc to
# Akaike weights and select best model. Additional functions to define
# breakpoint for elevation.
#######################################################################
#
# TODO:

require(segmented)
require(glmulti)
require(ggplot2)

# helper function to get breakpoint for z-only models
solar_mnthavg <- function(solardat, month, prefix){
	# averages solar radiation data by month
	# prefix determines radiation or insolation
	# month specifies the month to average for
	# columns of solardat are individual days
	# radiation data is named like RADDOY9, RADDOY23, RADDOY365 etc.
	#  then prefix = 'RADDOY'
	# insolation data is names liked INSDOY9, etc
	# names(radiationdat) = c('deviceid', 'RADDOY9', ...)
	# names(insolationdat) = c('deviceid', 'INSDOY9', ...)
	# helper dataframe
	doyconv <- data.frame(doy=seq(1,365), month=c(rep(1, 31), rep(2, 28), 
	                      rep(3, 31), rep(4, 30), rep(5, 31), rep(6, 30), 
						  rep(7, 31), rep(8, 31), rep(9, 30), rep(10, 31), 
						  rep(11, 30), rep(12, 31)))
	
	# generate labels
	 solarlabels <- paste(prefix, doyconv[doyconv[['month']]==month, 'doy'], 
	                      sep='')
	return(rowMeans(as.matrix(solardat[, solarlabels])))
}

get_bp_model <- function(dframe, rvar, pvar){
	#psi <- mean(dframe[[pvar]])
	psi = seq(575, 750, 5)
	# linfit first
	linfitfrm <- as.formula(paste(rvar, '~', pvar))
	linfit <- lm(linfitfrm, dframe)
	# segmented fit
	segfit <- vector('list', length=length(psi))
	for(i in seq(along=segfit)){
		segfit[[i]] <- tryCatch({
			segmented.lm(linfit, seg.Z=as.formula(paste("~", pvar)), 
		                            psi=psi[i])
		}, error = function(err){return(NA)})
	}
	# remove NULLs from list (happens if bp model cannot be fitted)
	for(i in seq(length(segfit), 1)){
		if(is.na(segfit[[i]])) 
			segfit[[i]] <- NULL
			warning('breakpoint model not fitted\n')
	}
	# if no bp models could be fitted, return NULL
	if(is.null(segfit)) 
		return(NULL)
	# otherwise get the breakpoints terms
	newfit <- vector('list', length=length(segfit))
	for(i in seq(along=segfit)){
		# reformat the formula (screw you segmented!)
		bpformula <- as.formula(paste(rvar, '~', pvar, '+ I(pmax(', pvar, '-', 
		                              segfit[[i]]$psi[2], ', 0))'))
		# refit the models using the new formula
		newfit[[i]] <- lm(bpformula, dframe)
	}
	# use glmulti to pick best model
	bpm <- glmulti(newfit, method='h', crit='bic')
	# refit model to return model object (screw you glmulti!)
	return(lm(bpm@formulas[[1]], dframe))
}

# helper function convert aicc to Akaike weights 
# following Wagenmakers and Farrell 2004
aic_to_weights <- function(AIC){
	rAIC <- AIC - min(AIC)
	rAICtrans <- exp(-0.5*rAIC)
	aw <- rAICtrans/sum(rAICtrans)
	return(aw)
}

# glmulti with specific parameters
myglm <- function(..., lev=1, meth='h', ic='bic', marg=TRUE, makeplot=FALSE){
	glmulti(..., level=lev, method=meth, crit=ic, marginality=marg, plotty=makeplot)
}	

pick_model <- function(dataset, response, predictors, exc=c()){
	# fit the models using glmulti
	models <- myglm(response, predictors, dataset, exclude=exc)
	# fit the z model with bp term and add to model list
	# get the elevation breakpoint model
	#bpm <- get_bp_model(dataset, r, 'z')
	modelobjs <- models@objects
	#nm <- length(modelobjs) + 1
	#modelobjs[[nm]] <- bpm
	# recheck model weights with bp model
	allmodels <- myglm(modelobjs, confsetsize=length(modelobjs), meth='h')
	return(allmodels)
}

plot_model <- function(model, dat){
	# model is output of e.g. lm
	# dat is output of get_all_vars(model, somedataset)
	# column 1 is response
	# column 2, ... are predictors
	# assumes rownames(dat) as id field
	new.data <- data.frame(a=seq(min(dat[, 2]), max(dat[, 2]), length=1000))
	names(new.data) <- names(dat)[2]
	preds <- predict(model, newdata=new.data, type='response', se=TRUE)
	new.data$pred.full <- preds$fit
	# confidence interval
	new.data$ymin <- new.data$pred.full - 2*preds$se.fit
	new.data$ymax <- new.data$pred.full + 2*preds$se.fit
	# visuals
	dat['id'] <- rownames(dat)
	pnts <- geom_text(aes(label=id), size=2, color="black")
	fit <- geom_line(data=new.data, aes(y=pred.full), color="red", size=1)
	cnf <- geom_ribbon(data=new.data,
						   aes(y=pred.full, ymin=ymin, ymax=ymax), alpha=0.2)
	ggplot(dat, aes_string(y=names(dat)[1], x=names(dat)[2])) + cnf + fit + pnts
}