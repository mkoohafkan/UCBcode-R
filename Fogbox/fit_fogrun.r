# fit_fogrun.r
fit_fogrun <- function(datalist){
# performs a variety of fitting procedures on the analyzed fog data
# dl is the full datalist of outputs from analyze_fogrun()
#
###
  # helper functions
  filter_rundata <- function(dl){
    # collection of filters determined by visually inspecting the data
	# designed to improve fittings and cope with process error
	for(i in seq(along=dl))
	  dl[[i]]$rundata['runfilter'] <- 1
	# add filters; 0 means data should be ignored
	dl[['bay3']]$rundata[dl[['bay3']]$rundata$record > 216, 'runfilter'] <- 0
	dl[['bay7']]$rundata[dl[['bay7']]$rundata$record > 177, 'runfilter'] <- 0
	dl[['blackoak1']]$rundata[dl[['blackoak1']]$rundata$record > 202, 'runfilter'] <- 0
	dl[['blackoak3']]$rundata[dl[['blackoak3']]$rundata$record > 194, 'runfilter'] <- 0
	dl[['blackoak5']]$rundata[dl[['blackoak5']]$rundata$record > 181 &
	                          dl[['blackoak5']]$rundata$record < 87, 'runfilter'] <- 0
	dl[['blackoak6']]$rundata[dl[['blackoak6']]$rundata$record > 214, 'runfilter'] <- 0
	dl[['blackoak6rep2']]$rundata[dl[['blackoak6rep2']]$rundata$record > 202, 'runfilter'] <- 0
	dl[['blackoak7']]$rundata[dl[['blackoak7']]$rundata$record > 201, 'runfilter'] <- 0
	dl[['blackoak7rep2']]$rundata[dl[['blackoak7rep2']]$rundata$record > 210, 'runfilter'] <- 0
	dl[['blackoak7rep3']]$rundata[dl[['blackoak7rep3']]$rundata$record > 152, 'runfilter'] <- 0
	dl[['coastliveoak1']]$rundata[dl[['coastliveoak1']]$rundata$record > 155, 'runfilter'] <- 0
	dl[['coastliveoak2']]$rundata[dl[['coastliveoak2']]$rundata$record < 69 &
	                              dl[['coastliveoak2']]$rundata$record > 198, 'runfilter'] <- 0
	dl[['coastliveoak3rep2']]$rundata[dl[['coastliveoak3rep2']]$rundata$record > 202, 'runfilter'] <- 0	
	dl[['coastliveoak4']]$rundata[dl[['coastliveoak4']]$rundata$record > 223, 'runfilter'] <- 0
	dl[['coastliveoak7']]$rundata[dl[['coastliveoak7']]$rundata$record > 204, 'runfilter'] <- 0
	dl[['dougfir5']]$rundata[dl[['dougfir5']]$rundata$record > 190, 'runfilter'] <- 0
	dl[['garryoak1']]$rundata[dl[['garryoak1']]$rundata$record > 207, 'runfilter'] <- 0
	dl[['garryoak4']]$rundata[dl[['garryoak4']]$rundata$record > 213, 'runfilter'] <- 0
	dl[['garryoak5']]$rundata[dl[['garryoak5']]$rundata$record > 269, 'runfilter'] <- 0
	dl[['garryoak7']]$rundata[dl[['garryoak7']]$rundata$record > 182, 'runfilter'] <- 0
	dl[['redwood2']]$rundata[dl[['redwood2']]$rundata$record > 241, 'runfilter'] <- 0
	dl[['redwood4']]$rundata[dl[['redwood4']]$rundata$record > 200, 'runfilter'] <- 0	
	dl[['redwood5']]$rundata[dl[['redwood5']]$rundata$record > 200, 'runfilter'] <- 0
	dl[['valleyoak2']]$rundata[dl[['valleyoak2']]$rundata$record > 216, 'runfilter'] <- 0
	dl[['valleyoak5']]$rundata[dl[['valleyoak5']]$rundata$record > 203, 'runfilter'] <- 0
	dl[['valleyoak8']]$rundata[dl[['valleyoak8']]$rundata$record > 203, 'runfilter'] <- 0
	# also filter LWS voltage < 274
	for(i in seq(along=dl))
	  dl[[i]]$rundata[dl[[i]]$rundata$LWSmV < 274, 'runfilter'] <- 0
	# factors for plotting
	for(i in seq(along=dl))
	  dl[[i]]$rundata['runfilter'] <- factor(dl[[i]]$rundata$runfilter)
	return(dl)
  }
  
  fit_lwscurve <- function(datalist){
  # fit watermass to LWS voltage
    # calculate summary data
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
						     conversionfactor.avg=sapply(datalist, function(x) x$conversionfactor$avg),
						     conversionfactor.stdev=sapply(datalist, function(x) x$conversionfactor$stdev))
    # fit a linear model using the LWSvolt.last values
    lastmodel <- lm(LWSwater.avg ~ LWSvolt.last, fogsummary, weights=1/LWSwater.stdev)
	fogsummary['fitresiduals'] <- lastmodel$residuals
    return(list(data=fogsummary, lwscurve=lastmodel))
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
    leafcoef <- leafmod$coefficients[[2]]
    lwsmod <- lwsmod$coefficients[[2]]
    return(leafcoef/lwsmod)
  }
###
  # add filter to rundata
  datalist <- filter_rundata(datalist)
  # get summary data and lws curve
  summarydata <- fit_lwscurve(datalist)
  for(i in seq(along=datalist)){
    datalist[[i]][['rundata']] <- calc_lwswater(datalist[[i]]$rundata, 
	                                            summarydata$lwscurve)
	curves <- fit_curves(datalist[[i]]$rundata)
	datalist[[i]][['lwsmodel']] <- curves[[1]]
	datalist[[i]][['leafmodel']] <- curves[[2]]
	datalist[[i]][['fitratio']] <- compare_curves(curves[[1]], curves[[2]])
  }
  # add fitratio to summarydata
  fitratios <- rep(NA, nrow(summarydata$data))
  leafcoefficients <- fitratios
  lwscoefficients <- fitratios
  leafr2 <- fitratios
  lwsr2 <- fitratios
  for(i in seq(along=fitratios)){
    leafcoefficients[i] <- datalist[[summarydata$data$name[i]]]$leafmodel$coefficients[[2]]
	lwscoefficients[i] <- datalist[[summarydata$data$name[i]]]$lwsmodel$coefficients[[2]]
    fitratios[i] <- datalist[[summarydata$data$name[i]]]$fitratio
	leafr2[i] <- summary(datalist[[summarydata$data$name[i]]]$leafmodel)$r.squared
	lwsr2[i] <- summary(datalist[[summarydata$data$name[i]]]$lwsmodel)$r.squared
  }
  summarydata$data['leafcoefficient'] <- leafcoefficients
  summarydata$data['lwscoefficient'] <- lwscoefficients
  summarydata$data['fitratio'] <- fitratios
  summarydata$data['leafr2'] <- leafr2
  summarydata$data['lwsr2'] <- lwsr2
  # add summarydata to datalist
  datalist[['summary']] <- summarydata
  return(datalist)
}

