# fit_fogrun.r
#
###
  # helper functions
  filter_rundata <- function(dl){
    # collection of filters determined by visually inspecting the data
	# designed to improve fittings and cope with process error
	for(i in seq(along=dl))
	  dl[[i]]$rundata['runfilter'] <- 1
	# add filters; 0 means data should be ignored
	# bay
	dl[['bay3']]$rundata[dl[['bay3']]$rundata$record > 216, 'runfilter'] <- 0
	dl[['bay7']]$rundata[dl[['bay7']]$rundata$record > 177, 'runfilter'] <- 0
	# blackoak
	dl[['blackoak1']]$rundata[dl[['blackoak1']]$rundata$record > 202, 'runfilter'] <- 0
	dl[['blackoak3']]$rundata[dl[['blackoak3']]$rundata$record > 194, 'runfilter'] <- 0
	dl[['blackoak5']]$rundata[dl[['blackoak5']]$rundata$record > 181 |
	                          dl[['blackoak5']]$rundata$record < 87, 'runfilter'] <- 0
	dl[['blackoak6']]$rundata[dl[['blackoak6']]$rundata$record > 214, 'runfilter'] <- 0
	dl[['blackoak6rep2']]$rundata[dl[['blackoak6rep2']]$rundata$record > 202, 'runfilter'] <- 0
	dl[['blackoak7']]$rundata[dl[['blackoak7']]$rundata$record > 201, 'runfilter'] <- 0
	dl[['blackoak7rep2']]$rundata[dl[['blackoak7rep2']]$rundata$record > 210, 'runfilter'] <- 0
	dl[['blackoak7rep3']]$rundata[dl[['blackoak7rep3']]$rundata$record > 152, 'runfilter'] <- 0
	# coastliveoak
	dl[['coastliveoak1']]$rundata[dl[['coastliveoak1']]$rundata$record > 155, 'runfilter'] <- 0
	dl[['coastliveoak2']]$rundata[dl[['coastliveoak2']]$rundata$record < 69 |
	                              dl[['coastliveoak2']]$rundata$record > 198, 'runfilter'] <- 0
	dl[['coastliveoak3rep2']]$rundata[dl[['coastliveoak3rep2']]$rundata$record > 202, 'runfilter'] <- 0	
	dl[['coastliveoak4']]$rundata[dl[['coastliveoak4']]$rundata$record > 223, 'runfilter'] <- 0
	dl[['coastliveoak7']]$rundata[dl[['coastliveoak7']]$rundata$record > 204, 'runfilter'] <- 0
	# dougfir
	dl[['dougfir5']]$rundata[dl[['dougfir5']]$rundata$record > 190, 'runfilter'] <- 0
	# garryoak
	dl[['garryoak1']]$rundata[dl[['garryoak1']]$rundata$record > 207, 'runfilter'] <- 0
	dl[['garryoak4']]$rundata[dl[['garryoak4']]$rundata$record > 213, 'runfilter'] <- 0
	dl[['garryoak5']]$rundata[dl[['garryoak5']]$rundata$record > 269, 'runfilter'] <- 0
	dl[['garryoak7']]$rundata[dl[['garryoak7']]$rundata$record > 182, 'runfilter'] <- 0
	# redwood
	dl[['redwood2']]$rundata[dl[['redwood2']]$rundata$record > 241, 'runfilter'] <- 0
	dl[['redwood4']]$rundata[dl[['redwood4']]$rundata$record > 200, 'runfilter'] <- 0	
	dl[['redwood5']]$rundata[dl[['redwood5']]$rundata$record > 200, 'runfilter'] <- 0
	# valleyoak
	dl[['valleyoak2']]$rundata[dl[['valleyoak2']]$rundata$record > 216, 'runfilter'] <- 0
	dl[['valleyoak5']]$rundata[dl[['valleyoak5']]$rundata$record > 203, 'runfilter'] <- 0
	dl[['valleyoak8']]$rundata[dl[['valleyoak8']]$rundata$record > 203, 'runfilter'] <- 0
	# blueoak
    dl[['blueoak2']]$rundata[dl[['blueoak2']]$rundata$record > 186, 'runfilter'] <- 0
	dl[['blueoak3']]$rundata[dl[['blueoak3']]$rundata$record < 35, 'runfilter'] <- 0
	dl[['blueoak5']]$rundata[dl[['blueoak5']]$rundata$record > 206, 'runfilter'] <- 0
	dl[['blueoak6']]$rundata[dl[['blueoak6']]$rundata$record < 44 | 
	                         dl[['blueoak6']]$rundata$record > 210, 'runfilter'] <- 0	
	# filter LWS voltage < 274
	for(i in seq(along=dl)){
	  dl[[i]]$rundata[dl[[i]]$rundata$LWSmV < 274, 'runfilter'] <- 0
	# filter minimum mass measurement < variability in dry leaf mass
	  dl[[i]]$rundata['runfilter'] <- dl[[i]]$rundata$runfilter == 1 & 
	                                  dl[[i]]$rundata$watermass - 
									   1.96*dl[[i]]$rundata$watermass.sd >
									   1.96*dl[[i]]$dryleafmass$stdev
	  # and ignore NA data
	  dl[[i]]$rundata[is.na(dl[[i]]$rundata$runfilter), 'runfilter'] <- FALSE
	  # factors for plotting
	  dl[[i]]$rundata['runfilter'] <- factor(as.numeric(dl[[i]]$rundata$runfilter))
	}
	return(dl)
  }
  
  fit_lwscurve <- function(datalist){
  # fit watermass to LWS voltage
    # calculate summary data
	  add_run_lws <- function(sumdat, datalist){
    sumdat['runLWS'] <- NA
	for(n in sumdat$name){
	  sumdat[n, 'runLWS'] <- max(datalist[[n]]$rundata$LWSmV[datalist[[n]]$rundata$runfilter])
	}
	return(sumdat)
  }
	
    fogsummary <- data.frame(name=sapply(datalist, function(x) x$name),
	  					     species=sapply(datalist, function(x) x$species),
						     trial=sapply(datalist, function(x) x$trial),
						     isvertical=sapply(datalist, function(x) x$isvertical),
						     replicate=sapply(datalist, function(x) x$replicate),
							 dryleafmass.avg=sapply(datalist, function(x) x$dryleafmass$avg),
							 dryleafmass.stdev=sapply(datalist, function(x) x$dryleafmass$stdev),
							 leafarea=sapply(datalist, function(x) x$leafarea),
							 runtime=sapply(datalist, function(x) x$runtime),
						     LWSwater.avg=sapply(datalist, function(x) x$LWSwatermass$avg),
						     LWSwater.stdev=sapply(datalist, function(x) x$LWSwatermass$stdev),
						     LWSvolt.max=sapply(datalist, function(x) x$maxLWSvolt),
						     LWSvolt.last=sapply(datalist, function(x) x$lastLWSvolt),
							 LWSvolt.first=sapply(datalist, function(x) x$rundata$LWSmV[1]),
						     runLWSvolt=sapply(datalist, function(x) max(x$rundata[x$rundata$runfilter == 1,'LWSmV'])),
							 conversionfactor.avg=sapply(datalist, function(x) x$conversionfactor$avg),
						     conversionfactor.stdev=sapply(datalist, function(x) x$conversionfactor$stdev))
	# add zero data
	dupdata <- fogsummary
	dupdata['LWSwater.avg'] <- 0
	dupdata['LWSvolt.last'] <- sapply(datalist, function(x) mean(x$dryleafvolt$LWSmV))
	dupdata <- rbind(fogsummary, dupdata)
	# fit an unweighted linear model using the LWSvolt.last values
    unweightedmodel <- lm(LWSwater.avg ~ LWSvolt.last, dupdata)
	# fit a weighted model
	# DEPRECATED: weighting is insignificant
	#fitfunc <- function(testrms){
	#  thisweights <-  abs(1/(testrms + fogsummary$LWSwater.stdev^2))
	#  if(any(thisweights != abs(thisweights)))
	#    return(NA)
	#  thismod <- lm(LWSwater.avg ~ LWSvolt.last, fogsummary, weights=thisweights)
	#  return(abs(testrms - 1))
	#}
	#rms <- optim(summary(unweightedmodel)$sigma - mean(fogsummary$LWSwater.stdev^2), 
	#             fitfunc, method='Brent', lower = -30, upper = 30)$value
	#modweights <- 1/(rms + fogsummary$LWSwater.stdev^2)
    #lastmodel <- lm(LWSwater.avg ~ LWSvolt.last, fogsummary, weights=modweights)
	#fogsummary['fitresiduals'] <- lastmodel$residuals
	# return the summary data, weighted model, and unweighted model
	return(list(data=fogsummary, lwscurve=unweightedmodel))
  }
  calc_lwswater <- function(rd, modfit, LWSarea=32.54){
  # calculate the LWS water mass and surface density based on fitted curve
  # rd is the rundata of a given trial
  # modfit is the LWS curve fitted from fit_lwscurve()
    # extract data for curve
    nd <- data.frame(LWSvolt.last=rd$LWSmV)
    # predict the water mass on the LWS using curve
    preds <- predict(modfit, newdata=nd, type='response', se=TRUE)
    nd['LWSwater'] <- preds$fit
    nd['LWSwater.stdev'] <- preds$se.fit
    # normalize by LWS area to get surface density
    nd['LWSsurfacedensity'] <- nd['LWSwater']/LWSarea
    nd['LWSsurfacedensity.stdev'] <- nd['LWSwater.stdev']/LWSarea
    return(cbind(rd, nd[, c('LWSwater', 'LWSwater.stdev', 
	                        'LWSsurfacedensity', 'LWSsurfacedensity.stdev')]))
  }
  fit_curves <- function(rd){
  # fit linear curves to LWS voltage and leaf water surface density
    fitlws <- lm(LWSsurfacedensity ~ record, rd[rd$runfilter == 1,])
    fitleaf <- lm(surfacedensity ~ record, rd[rd$runfilter == 1,])
    return(list(fitlws, fitleaf))
  }
  compare_curves <- function(lwsmod, leafmod){
    # get the leaf:lws slope ratio of the fitted curves
	d <- data.frame(leafcoef=c(summary(leafmod)$coefficients[2, 1],
	                           summary(leafmod)$coefficients[2, 2]),
					lwscoef=c(summary(lwsmod)$coefficients[2, 1],
	                          summary(lwsmod)$coefficients[2, 2]))
    expr <- expression(leafcoef/lwscoef)
	r <- calc_with_uncertainty(expr, d)
	d['fitratio'] <- c(r$avg, r$stdev)
	return(d)
  }
  merge_replicates <- function(summarydata){
    # merge the replicates by averaging
	alldat <- NULL
	namelist <- c('species', 'trial', 'fitratio.avg', 'fitratio.stdev')
	for(s in unique(summarydata$species)){
	  for(tr in unique(as.character(summarydata[summarydata$species == s, 'trial']))){
	    entries <- summarydata$species == s & summarydata$trial == tr &
		           !summarydata$isvert
		trdat <- summarydata[entries, namelist]
		if(nrow(trdat) < 2){
		  mergedat <- trdat
		}else{
		  mergedat <- cbind(s, tr, stats_with_uncertainty(trdat[, c('fitratio.avg', 
							                                        'fitratio.stdev')]))
	      names(mergedat) <- namelist
		}
		alldat <- rbind(alldat, mergedat)
	  }
    }
    row.names(alldat) <- paste(alldat$species, alldat$trial, sep='')
	return(alldat)
  }
  average_by_species <- function(sumdat){
    # assumes fitratio filter already applied
	alldat <- NULL
	for(s in unique(sumdat$species)){
	  speciesratio <- stats_with_uncertainty(sumdat[sumdat$species == s, 
	                                         c('fitratio.avg', 'fitratio.stdev')])
	  speciesratio['species'] <- s
	  speciesratio['fitratio.min'] <- min(sumdat[sumdat$species == s, 
	                                             'fitratio.avg'])
	  speciesratio['fitratio.max'] <- max(sumdat[sumdat$species == s, 
	                                             'fitratio.avg'])
	  alldat <- rbind(alldat, speciesratio)
	}
	names(alldat) <- c('fitratio.avg', 'fitratio.stdev', 'species', 
	                   'fitratio.min', 'fitratio.max')
	return(alldat)
  }
###
#
fit_fogrun <- function(datalist){
# performs a variety of fitting procedures on the analyzed fog data
# dl is the full datalist of outputs from analyze_fogrun()
  # add filter to rundata
  datalist <- filter_rundata(datalist)
  # create lws curve
  summarydata <- fit_lwscurve(datalist)
  # expand add LWS water mass to all rundata
  for(i in seq(along=datalist)){
    datalist[[i]][['rundata']] <- calc_lwswater(datalist[[i]]$rundata, 
	                                            summarydata$lwscurve)
	curves <- fit_curves(datalist[[i]]$rundata)
	# attach models to the trial data
	datalist[[i]][['lwsmodel']] <- curves[[1]]
	datalist[[i]][['leafmodel']] <- curves[[2]]
  }
  # add fitratio to summarydata
  fitratio.avg <- rep(NA, nrow(summarydata$data))
  fitratio.stdev <- fitratio.avg
  leafcoefficient.avg <- fitratio.avg
  leafcoefficient.stdev <- fitratio.avg
  lwscoefficient.avg <- fitratio.avg
  lwscoefficient.stdev <- fitratio.avg
  leafr2 <- fitratio.avg
  lwsr2 <- fitratio.avg 
  for(i in seq(along=fitratio.avg)){
    ratios <- compare_curves(datalist[[summarydata$data$name[i]]]$lwsmodel, 
	                         datalist[[summarydata$data$name[i]]]$leafmodel)
	fitratio.avg[i] <- ratios[1, 'fitratio']
	fitratio.stdev[i] <- ratios[2, 'fitratio']
	leafcoefficient.avg[i] <- ratios[1, 'leafcoef']
	leafcoefficient.stdev[i] <- ratios[2, 'leafcoef']
	lwscoefficient.avg[i] <- ratios[1, 'lwscoef']
	lwscoefficient.stdev[i] <- ratios[2, 'lwscoef']
	leafr2[i] <- summary(datalist[[summarydata$data$name[i]]]$leafmodel)$r.squared
	lwsr2[i] <- summary(datalist[[summarydata$data$name[i]]]$lwsmodel)$r.squared
  }
  summarydata$data['leafcoefficient.avg'] <- leafcoefficient.avg
  summarydata$data['leafcoefficient.stdev'] <- leafcoefficient.stdev
  summarydata$data['lwscoefficient.avg'] <- lwscoefficient.avg
  summarydata$data['lwscoefficient.stdev'] <- lwscoefficient.stdev
  summarydata$data['fitratio.avg'] <- fitratio.avg
  summarydata$data['fitratio.stdev'] <- fitratio.stdev  
  summarydata$data['leafr2'] <- leafr2
  summarydata$data['lwsr2'] <- lwsr2
  # get prediction of maximum leaf water and LWS water of curve-fitted region
  lwsmodelwatermass.avg <- rep(NA, nrow(summarydata$data))
  lwsmodelwatermass.stdev <- lwsmodelwatermass.avg
  leafmodelwatermass.avg <- lwsmodelwatermass.avg
  leafmodelwatermass.stdev <- lwsmodelwatermass.avg
  for(i in seq(along=lwsmodelwatermass.avg)){
    thisrd <- datalist[[summarydata$data$name[i]]]$rundata
  	thisrd <- thisrd[thisrd$runfilter == 1,]
	recindx <- thisrd[nrow(thisrd), 'record']
	nd <- data.frame(record=recindx)
	lwsmodelvals <- predict(datalist[[summarydata$data$name[i]]]$lwsmodel, 
	                        newdata=nd, se.fit=TRUE)
    leafmodelvals <- predict(datalist[[summarydata$data$name[i]]]$leafmodel, 
	                         newdata=nd, se.fit=TRUE)
	lwsmodelwatermass.avg[i] <- lwsmodelvals$fit
	lwsmodelwatermass.stdev[i] <- lwsmodelvals$se.fit
	leafmodelwatermass.avg[i] <- leafmodelvals$fit
	leafmodelwatermass.stdev[i] <- leafmodelvals$se.fit
  }
  summarydata$data['LWSmodelwater.avg'] <- lwsmodelwatermass.avg
  summarydata$data['LWSmodelwater.stdev'] <- lwsmodelwatermass.stdev
  summarydata$data['leafmodelwater.avg'] <- leafmodelwatermass.avg
  summarydata$data['leafmodelwater.stdev'] <- lwsmodelwatermass.stdev
  # add the ratiofilter
  summarydata$data['ratiofilter'] <- (summarydata$data$leafmodelwater.avg - 
                                 1.96*summarydata$data$leafmodelwater.stdev) > 
								 1.96*summarydata$data$dryleafmass.stdev
  summarydata[['repavg']] <- merge_replicates(summarydata$data[summarydata$data$ratiofilter,])
  summarydata[['speciesavg']] <- average_by_species(summarydata$data)
  summarydata[['speciesrepavg']] <- average_by_species(summarydata$repavg)
    
  # add summarydata to datalist
  datalist[['summary']] <- summarydata
  return(datalist)
}