select_model <- function(df, pv, rv, n=2, psi=NULL){
# Identify best model from set of:
#	no predictors (rv ~ 1)
#	linear response to pv
#	segmented linear response to pv
# df = a data frame containing at minimum the columns rv, pv
	# expects na.omit(df)
# pv = predictor variable, e.g. z
# response variable, e.g. temperature
# n = number of observations to leave out for leave-n-out cross-validation
# psi = initial guess for segmented fit

	numobs <- nrow(df)
	setsize <- numobs - n
	sets <- combn(seq(1, numobs, 1), setsize)
	numsets <- dim(sets)[2]
	models <- vector("list", length=numsets)
	formulas <- models
	# iterate through the sets and fit models
	for(i in seq(1, numsets, 1)){
		# take a subset of the data for cross-validation
		subdf <- df[sets[,i],]
		# find the segmented fit
		tmp <- fit_models(subdf, pv, rv, psi)
		models[[i]] <- tmp[['models']]
		# all unique formulas 
		formulas[[i]] <- tmp[['formulas']]
	}
	
	# get the p-values of the models fitted to each subset
	# HARDCODED: pvals columns are nofit, linfit, segfit	
	pvals <- data.frame(nofit=rep(NA, length(models)))
	pvals['linfit'] <- pvals$nofit
	pvals['segfit'] <- pvals$nofit
	# boolean matrix indicating which model had lowest p-value
	isbest <- matrix(rep(FALSE, length(models[[1]])*length(models)), ncol=3)
	# get the p-values
	for(i in seq(along=models)){ # for each subset		
		for(m in seq(along=models[[i]])){ # for each model type
			thism <- models[[i]][[m]]
			# formula to compute p-value of model (see summary() for lm)
			pvals[i, m] <- pf(thism$fstatistic[1], thism$fstatistic[2], 
							   thism$fstatistic[3], lower.tail=FALSE)
		}
		# identify which model type fits this data subset best
		isbest[i, ] <- pvals[i,]==min(pvals[i,])
	}
	# total the number of times each model type was selected
	totals <- rep(NA, length(models[[1]]))
	for(i in seq(along=totals))
		totals[i] <- sum(isbest[i,])
	# select the best model based on cross-validation results
	for(t in seq(along=totals)){
		if(min(totals)==totals[t])
			bestmodel <- lm(formulas[[1]][[t]], df)
	}
	return(bestmodel)
}


fit_models <- function(df, pv, rv, psi=NULL){
	require(segmented)
# get fits for: 
# 	no predictor (rv ~ 1) 
#   linear function (rv ~ pv)
#   piecewise linear with breakpoint fit (rv ~ pv)
# df = data.frame. names(df) must include rv, pv
# pv = name of predictor variable
# rv = name of response variable
# psi = initial guess for breakpoint

	# helper function
	piece.formula <- function(var.name, knots) {
		formula.sign <- rep(" - ", length(knots))
		formula.sign[knots < 0] <- " + "
		paste(var.name, "+",
		paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
		collapse = " + ", sep=""))
	}
	# if initial breakpoint not provided, make a guess
	if(is.null(psi)) 
		psi <- mean(df[[pv]])
	# no fit
	nofitfrm <- as.formula(paste(rv, '~ 1'))
	nofit <- lm(nofitfrm, df)
	# linear fit
	linfitfrm <- as.formula(paste(rv, '~', pv))
	linfit <- lm(linfitfrm, df)
	# segmented fit
	segfit <- segmented.lm(linfit, seg.Z=as.formula(paste("~", pv)), psi=psi)
	bp <- segfit$psi[2]
	# redo segmented fit
	segfitfrm <- as.formula(paste(rv, "~", piece.formula(pv, bp)))
	segfit <- lm(segfitfrm, df)
	# put all the models together
	models <- list(nofit=nofit, linfit=linfit, segfit=segfit)
	formulas <- list(deparse(nofitfrm), deparse(linfitfrm), deparse(segfitfrm))
	return(list(models=models, formulas=formulas))
}


plot_fit <- function(df, pv, rv, models, formulas, id=NULL){
require(ggplot2)
# plot the available models and confidence intervals
# df = data.frame. names(df) must include rv, pv
# pv = name of predictor variable
# rv = name of response variable
# models = output of fit_models
# formulas = output of fit_models
# id = optional column name to use as point labels

	# initialize list of plots
	g <- vector("list", length=length(models))
	# iterate through available models
	for(i in seq(along=models)){
		# if id tag was not provided
		if(is.null(id)){
			# plot points if id not provided
			pnts <- geom_point()
		} else {
			# plot id numbers instead of points if provided
			pnts <- geom_text(aes_string(label=id), size=2, color="black")
		}
		# fit and confidence interval
		new.data <- data.frame(a=seq(min(df[[pv]]), max(df[[pv]]), length=10000))
		names(new.data) <- c(pv)
		# fit
		preds <- predict(models[[i]], newdata=new.data, type='response', se=TRUE)
		new.data$pred.full <- preds$fit
		# confidence interval
		new.data$ymin <- new.data$pred.full - 2*preds$se.fit
		new.data$ymax <- new.data$pred.full + 2*preds$se.fit 
		# graphics
		fit <- geom_line(data=new.data, aes(y=pred.full), color="red", size=1)
		cnf <- geom_ribbon(data=new.data,
						   aes(y=pred.full, ymin=ymin, ymax=ymax), alpha=0.2)
		# labels
		eqn <- geom_text(label=formulas[[i]], size=2,
						 x=psi, y=0.99*max(df[[rv]]))
		rval <- geom_text(x=psi, y=0.98*max(df[[rv]]), size=2,
						  label=paste('r-squared ~', 
						  format(summary(models[[i]])$r.squared, 
								 digits=3)))
		# plot it
		g[[i]] <- ggplot(data=as.data.frame(df), aes_string(x=pv, y=rv)) + 
				  cnf + fit + pnts + eqn + rval
		rm(preds)
	}
	# return ggplot objects
	return(g)
}