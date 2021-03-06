# plot_fogsummary.r
# functions used in process wrapper to plot data
require(ggplot2)
require(reshape2)
require(gridExtra)

lm_eqn = function(m) {
# get the equation and r-squared of a linear model
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  if(is.na(coef(m)[2])){
    eq <- substitute(italic(y) == a %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }
  else if(coef(m)[2] >= 0){
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }else{
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  as.character(as.expression(eq));                 
}

lwscurveplot <- function(fogsummary, fitmod){
# plot the LWS water mass vs. voltage curve
# fogsummary is the summary data
# fitmod is the fitted LWS curve
    # build the curve
	nd <- data.frame(LWSvolt.last=seq(264, 600))
	preds <- predict(fitmod, newdata=nd, type='response', se=TRUE)
	nd['LWSwater.avg'] <- preds$fit
	nd['LWSwater.stdev'] <- preds$se.fit
	# add a label to fogsummary for plotting
    fogsummary['plab'] <- paste('  ', fogsummary$trial, 'rep', fogsummary$replicate, sep='')
	# also plot curve from Cobos 2013
	cobosmodel <- lm(log(LWSwater.avg) ~ LWSvolt.last, fogsummary, 
	                 weights=1/LWSwater.stdev)
	cobospreds <- predict(cobosmodel, newdata=nd, type='response', se=TRUE)
	nd['cobosline.avg'] <- cobospreds$fit
	nd['cobosline.stdev'] <- cobospreds$se.fit
	# plot
	ggplot(fogsummary, aes(x=LWSvolt.last, y=LWSwater.avg)) + 
	geom_point(size=1.5) +
	geom_errorbar(aes(ymin=LWSwater.avg - 1.96*LWSwater.stdev,
	                  ymax=LWSwater.avg + 1.96*LWSwater.stdev), width=1) +
    geom_ribbon(data=nd, aes(ymin=LWSwater.avg - 1.96*LWSwater.stdev,
	                         ymax=LWSwater.avg + 1.96*LWSwater.stdev,
							 x=LWSvolt.last), alpha=0.2) +
    geom_line(data=nd, aes(x=LWSvolt.last, y=LWSwater.avg), color='blue', size=1, alpha=0.7) +
	#geom_line(data=nd, aes(y=exp(cobosline.avg)), color='red', linetype='dashed') +
	#geom_text(aes(label=plab), size=2, hjust=0, alpha=0.5, position='dodge') + 	
	annotate("text", label=lm_eqn(fitmod), x=375, y=0.275, parse=TRUE) +
	scale_y_continuous('water mass on LWS (g)') + 
    scale_x_continuous('LWS voltage (mV)', limits=c(264, 600))
}

allrunsplot <- function(dl){
# plot the rundata
	# merge the rundata from each trial
	alldat <-NULL
	for(i in seq(along=dl)){
	    # add tags to rundata component
		dl[[i]]$rundata['name'] <- dl[[i]]$name
		dl[[i]]$rundata['species'] <- dl[[i]]$species
		dl[[i]]$rundata['trial'] <- dl[[i]]$trial
		dl[[i]]$rundata['replicate'] <- dl[[i]]$replicate
		# convert timestamp to 'seconds since start' for plotting
		dl[[i]]$rundata['seconds'] <- 15*seq(0, nrow(dl[[i]]$rundata) - 1)
		# bind this rundata to alldat
		alldat <- rbind(alldat, dl[[i]]$rundata[c('species', 'surfacedensity',
		                                          'LWSmV', 'surfacedensity.sd',
												  'seconds', 'trial', 'name',
												  'replicate')])
	}
	# plot it
	p1 <- ggplot(alldat, aes(x=seconds, y=surfacedensity, color=trial)) + 
	      geom_point() + facet_wrap(~species)
	p2 <- ggplot(alldat, aes(x=seconds, y=LWSmV, color=trial, shape=replicate)) + 
	      geom_point() + facet_wrap(~species)
	return(list(p1, p2))
}

surfacedensityplots <- function(datalist){
  # plot leaf surfacedensity for each run
  wps <- vector('list', length=length(datalist))
  for(i in seq(along=datalist)){
    recindx <- datalist[[i]]$rundata[datalist[[i]]$rundata$LWSmV >= 274, 'record'][1]
    nd <- data.frame(record=datalist[[i]]$rundata$record, runfilter=1)
	preds <- predict(datalist[[i]]$leafmodel, newdata=nd, type='response', se=TRUE)
	nd['surfacedensity'] <- preds$fit
	nd['surfacedensity.stdev'] <- preds$se.fit
    t2 <- datalist[[i]]$rundata[,c('record', 'surfacedensity', 'runfilter')]
    wps[[i]] <- ggplot(t2, aes(x=record, y=surfacedensity)) + 
	            geom_point(aes(shape=runfilter, color=runfilter)) +
	            scale_shape_manual(values=c(4, 16)) + 
				scale_color_manual(values=c('red', 'black')) +				
	            geom_vline(xintercept=recindx, alpha=0.5, linetype='longdash') +
                geom_ribbon(data=nd, 
				            aes(ymin=surfacedensity - 1.96*surfacedensity.stdev,
							    ymax=surfacedensity + 1.96*surfacedensity.stdev),
							alpha=0.3) +
				geom_line(data=nd) + ggtitle(datalist[[i]]$name) +
				xlab('record') + ylab('surface density of water on leaf') +
                opts(legend.position="bottom")				
	names(wps)[i] <- datalist[[i]]$name
  }
  return(wps)
}
lwssurfdensplots <- function(datalist){
  # plot LWS surfacedensity for each run
  wps <- vector('list', length=length(datalist))
  for(i in seq(along=datalist)){
    # get the LWSmV cutoff location
	recindx <- datalist[[i]]$rundata[datalist[[i]]$rundata$LWSmV >= 274, 'record'][1]
    nd <- data.frame(record=datalist[[i]]$rundata$record, runfilter=1)
	preds <- predict(datalist[[i]]$lwsmodel, newdata=nd, type='response', se=TRUE)
	nd['LWSsurfacedensity'] <- preds$fit
	nd['LWSsurfacedensity.stdev'] <- preds$se.fit
    t2 <- datalist[[i]]$rundata[,c('record', 'LWSsurfacedensity', 'runfilter')]
    wps[[i]] <- ggplot(t2, aes(x=record, y=LWSsurfacedensity)) + 
	            geom_point(aes(shape=runfilter, color=runfilter)) +
		        scale_shape_manual(values=c(4, 16)) + 
				scale_color_manual(values=c('red', 'black')) +
	            geom_vline(xintercept=recindx, alpha=0.5, linetype='longdash') +
                geom_ribbon(data=nd, 
				            aes(ymin=LWSsurfacedensity - 1.96*LWSsurfacedensity.stdev,
							    ymax=LWSsurfacedensity + 1.96*LWSsurfacedensity.stdev),
							alpha=0.3) +
				geom_line(data=nd) + ggtitle(datalist[[i]]$name)+
				xlab('record') + ylab('surface density of water on LWS')
	names(wps)[i] <- datalist[[i]]$name
  }
  return(wps)
}

lwsmvplots <- function(datalist){
  # plot LWSmV for each run
  lps <- vector('list', length=length(datalist))
  for(i in seq(along=datalist)){
	recindx <- datalist[[i]]$rundata[datalist[[i]]$rundata$LWSmV > 274, 'record'][1]
    t2 <- datalist[[i]]$rundata[,c('record', 'LWSmV')]
    lps[[i]] <- ggplot(t2) + geom_point(aes(x=record, y=LWSmV)) +
            	geom_vline(xintercept=recindx, alpha=0.5, linetype='longdash') +
                ggtitle(datalist[[i]]$name) + ylab('sensor voltage (mV)')
    names(lps)[i] <- datalist[[i]]$name
  }
  return(lps)	
}

surfdensandlwsplots <- function(datalist){
  # plot loadmV and LWSmV as facets for each run
  lps <- vector('list', length=length(datalist))
  for(i in seq(along=datalist)){
    t <- datalist[[i]]$rundata[,c('record', 'surfacedensity', 'LWSmV')]
    t2 <- melt(t, id.vars=c('record'))
    lps[[i]] <- ggplot(t2) + geom_point(aes(x=record, y=value)) + 
                facet_wrap(~variable, ncol=1, scales="free_y") +
                ggtitle(datalist[[i]]$name)
    names(lps)[i] <- datalist[[i]]$name
  }
  return(lps)
}

loadplots <- function(datalist){
# plot loadmV and change in loadmV as facets
  rps <- vector('list', length=length(datalist))
  for(i in seq(along=datalist)){
    t <- datalist[[i]]$rundata[,c('record', 'loadmV')]
    t['loadiff'] <- rep(0, nrow(t))
    for(j in 2:nrow(t))
      t$loadiff[j] <- t$loadmV[j] - t$loadmV[j-1]
    t2 <- melt(t, id.vars=c('record'))
    rps[[i]] <- ggplot(t2) + geom_point(aes(x=record, y=value)) + 
                facet_wrap(~variable, ncol=1, scales="free_y") +
                ggtitle(datalist[[i]]$name)
	names(rps)[i] <- datalist[[i]]$name
  }
  return(rps)
}

ratioplot <- function(summarydata){
  # plot the fitratios as dots
  summarydata['plab'] <- paste('  ', summarydata$trial, 'rep', summarydata$replicate, sep='')
  #summarydata['plab'] <- paste(' ', summarydata$leafarea)
  #summarydata['plab'] <- paste(' ', summarydata$LWSvolt.last)
  #summarydata['plab'] <- paste(' ', summarydata$LWSwater.avg)
  #summarydata['plab'] <- paste('  ', format(summarydata$leafr2, digits=2), 
  #                             '/', format(summarydata$lwsr2, digits=2), sep='')
  #summarydata['plab'] <- paste(' ', summarydata$leafmodelwater.avg)
  ggplot(summarydata, aes(x=species, y=fitratio.avg)) + 
  geom_errorbar(data=summarydata[summarydata$ratiofilter,], 
                aes(ymin=fitratio.avg - 1.96*fitratio.stdev,
				    ymax=fitratio.avg + 1.96*fitratio.stdev), 
			    width=0.1, position='dodge') +
  geom_point(aes(shape=ratiofilter)) + 
  geom_hline(yintercept=1, linetype='longdash', alpha=0.5) +
  scale_shape_manual(values=c(4, 16)) +
  geom_text(aes(label=plab), size=2, hjust=0, alpha=0.5) +
  ggtitle('slope ratio of leaf wetting to LWS wetting by species') +
  stat_summary(data=summarydata[summarydata$ratiofilter,], fun.y=mean, 
               geom="point", color='blue')
}
ratiomeans <- function(speciesdat, sumdat){
  ggplot(speciesdat, aes(x=species, y=fitratio.avg, 
                 ymin=fitratio.avg - 1.96*fitratio.stdev,
                 ymax=fitratio.avg + 1.96*fitratio.stdev)) + 
  geom_point() + geom_errorbar(width=0.2)  + geom_point(data=sumdat, alpha=0.5) + 
  stat_summary(data=sumdat,fun.y=median, geom='point', color='red')
}

ratiobox <- function(sumdat){
  ggplot(sumdat, aes(x=species, y=fitratio.avg)) + geom_boxplot(notch=TRUE)
}

bigratiobox <- function(dl){
  # pull rundata from each trial
  alldat <- NULL
  for(i in seq(along=dl)){
    # pull this trial's rundata
    thisdat <- dl[[i]]$rundata
	# append trial identifiers
	thisdat['name'] <- dl[[i]]$name
	thisdat['species'] <- dl[[i]]$species
	thisdat['trial'] <- dl[[i]]$trial
	thisdat['replicate'] <- dl[[i]]$replicate
    # collate data 
	alldat <- rbind(alldat, thisdat)
  }
  # calculate massratio for plotting
  alldat['massratio'] <- alldat$surfacedensity/alldat$LWSsurfacedensity
  # filter the data again
  alldat[alldat$massratio < 0 | is.na(alldat$massratio), 'runfilter'] <- 0
  filtdat <-alldat[alldat$runfilter==1,]
  # plot it
  ylim1 = boxplot.stats(filtdat$massratio)$stats[c(1, 5)]
  ggplot(filtdat, aes(x=species, y=massratio)) + 
  geom_boxplot(notch=TRUE, outlier.shape=NA) + ylim(c(0,10))
  #coord_cartesian(ylim = ylim1*1.05)
}


voltrangeplot <- function(sumdat){
  # get the range of lws voltage for each species
  ggplot(sumdat[sumdat$ratiofilter,], aes(x=species, y=LWSvolt.last)) + geom_point()
}

fitratiobyvolt <- function(sumdat, speciesdat){
  #sumdat <- sumdat[sumdat$ratiofilter,]
  # add runLWS mins and maxes to speciesdat
  speciesdat['runLWSvolt.min'] <- NA
  speciesdat['runLWSvolt.max'] <- NA
  speciesdat['runLWSvolt'] <- NA
  for(s in speciesdat$species){
    speciesdat[speciesdat$species == s, 'runLWSvolt.min'] <- min(sumdat[sumdat$species == s, 'runLWSvolt'])
    speciesdat[speciesdat$species == s, 'runLWSvolt.max'] <- max(sumdat[sumdat$species == s, 'runLWSvolt'])
    speciesdat[speciesdat$species == s, 'runLWSvolt'] <- mean(sumdat[sumdat$species == s, 'runLWSvolt'])
  } 
  ggplot(sumdat, aes(x=runLWSvolt, y=fitratio.avg, color=species)) + 
  geom_errorbar(data=speciesdat, aes(ymin=fitratio.min, ymax=fitratio.max)) +
  geom_errorbarh(data=speciesdat, aes(xmin=runLWSvolt.min, xmax=runLWSvolt.max)) +
  geom_point() + facet_wrap(~species, ncol=2)
}

leafmodelvslwsmodel <- function(sumdat){
  ggplot(sumdat[sumdat$ratiofilter,], aes(x=LWSmodelwater.avg, y=leafmodelwater.avg)) + 
  geom_point() + facet_wrap(~species)
}
leafmodelvsvoltage <- function(sumdat){
  ggplot(sumdat[sumdat$ratiofilter,], aes(x=runLWSvolt, y=leafmodelwater.avg)) + 
  geom_point() + facet_wrap(~species)
}

redwoodcurve <- function(sumdat){
  redwood <- sumdat[sumdat$species == 'redwood' & sumdat$ratiofilter, 
                    c('LWSmodelwater.avg', 'LWSmodelwater.stdev', 
					  'leafmodelwater.avg', 'leafmodelwater.stdev')]
  redwoodcurve <- lm(leafmodelwater.avg ~ LWSmodelwater.avg + 0, redwood)
  ggplot(redwood, aes(y=leafmodelwater.avg, x=LWSmodelwater.avg)) + 
  stat_smooth(method = "lm") + geom_point() + 
  annotate("text", label=lm_eqn(redwoodcurve), x=0.0025, y=0.01, parse=TRUE) +
  xlab('water surface density on sensor (g cm-2)') +
  ylab('water surface density on leaf (g cm-2)')
}

allredwoodcurve <- function(dl){
  # extract redwood runs
  sumdat <- dl$summary$data
  rfnames <- sumdat[sumdat$species == 'redwood' & sumdat$ratiofilter, 'name']
  dl <- dl[rfnames]
  redwood <- NULL
  cols <- c("surfacedensity", "surfacedensity.sd", 
            "LWSsurfacedensity", "LWSsurfacedensity.stdev")
  # pull collate rundata from all redwood runs, filtered using runfilter
  for(n in names(dl)){
    thisdat <- dl[[n]]$rundata[dl[[n]]$rundata$runfilter == 1, cols]
	thisdat['name'] <- n
	redwood <- rbind(redwood, thisdat)
  }
  redwood <- redwood[redwood$surfacedensity >= 0 & redwood$LWSsurfacedensity >= 0,]
  # fit a curve of leaf water vs sensor water (normalized by surface areas)
  redwoodcurve <- lm(surfacedensity ~ LWSsurfacedensity + 0, redwood, weights=1/surfacedensity.sd)
  nd <- data.frame(LWSsurfacedensity=seq(0, max(redwood$LWSsurfacedensity), length=100))
  preds <- predict(redwoodcurve, newdata=nd, type='response', se=TRUE)
  nd['surfacedensity'] <- preds$fit 
  nd['surfacedensity.stdev'] <- preds$se.fit  
  # plot it
  ggplot(redwood, aes(y=surfacedensity, x=LWSsurfacedensity)) + 
  geom_ribbon(data=nd, aes(ymin=surfacedensity - 1.96*surfacedensity.stdev,
	                         ymax=surfacedensity + 1.96*surfacedensity.stdev,
							 x=LWSsurfacedensity), alpha=0.3) + 
  geom_point(aes(color=name)) +
  geom_line(data=nd, aes(x=LWSsurfacedensity, y=surfacedensity), color='black') +
  annotate("text", label=lm_eqn(redwoodcurve), x=0.0025, y=0.01, parse=TRUE) +
  xlab('water surface density on sensor (g cm-2)') +
  ylab('water surface density on leaf (g cm-2)')
}

massratios <- function(dl){
  rp <- vector('list', length=length(dl))
  llwsp <- rp
  cp <- rp
  for(i in seq(along=dl)){
    dl[[i]]$rundata['minutes'] <- seq(from=0, by = 0.25, length.out=nrow(dl[[i]]$rundata))
	dl[[i]]$rundata['modeldensity'] <- predict(dl[[i]]$leafmodel, newdata=dl[[i]]$rundata)
	dl[[i]]$rundata['LWSmodeldensity'] <- predict(dl[[i]]$lwsmodel, newdata=dl[[i]]$rundata)
	# whatever the measurement, if surfacedensity - 1.96*sigma < 0 toss it
	dmask <- dl[[i]]$rundata$LWSwater - 
	         1.96*dl[[i]]$dryleafmass$stdev < 0 
    dl[[i]]$rundata[dmask, 'runfilter'] <- 0
	startwindow <- dl[[i]]$rundata$minutes[match(1, dl[[i]]$rundata$runfilter)]
	endwindow <- dl[[i]]$rundata$minutes[nrow(dl[[i]]$rundata) + 1 - match(1, rev(dl[[i]]$rundata$runfilter))]
    rp[[i]] <- ggplot(dl[[i]]$rundata[dl[[i]]$rundata$runfilter == 1,], 
	                  aes(x=minutes, y=surfacedensity/LWSsurfacedensity)) + 
	           geom_point() +
			   geom_vline(xintercept=c(startwindow, endwindow), linetype='dashed') 
    llwsp[[i]] <- ggplot(dl[[i]]$rundata, aes(x=minutes)) + 
	         geom_point(aes(y=surfacedensity), color='green') + 
			 geom_point(aes(y=LWSsurfacedensity), color='blue') +
			 geom_vline(xintercept=c(startwindow, endwindow), linetype='dashed') 
	cp[[i]] <- arrangeGrob(llwsp[[i]], rp[[i]], ncol=1)
  }
  return(list(cp, llwsp, rp))
}

massplot <- function(dl){
  alldat <- NULL
  for(i in seq(along=dl)){
    # pull this trial's rundata
    thisdat <- dl[[i]]$rundata
	# append trial identifiers
	thisdat['name'] <- dl[[i]]$name
	thisdat['species'] <- dl[[i]]$species
	thisdat['trial'] <- dl[[i]]$trial
	thisdat['replicate'] <- dl[[i]]$replicate
	# whatever the measurement, if surfacedensity - 1.96*sigma < 0 toss it
	dmask <- thisdat$LWSwater - 
	         1.96*dl[[i]]$dryleafmass$stdev < 0 
    thisdat[dmask, 'runfilter'] <- 0
    # collate data 
	alldat <- rbind(alldat, thisdat)
  }
  # filter the data
  filtdat <- alldat#[alldat$runfilter==1,]
  # plot everything
  l <- vector('list', length=length(unique(filtdat$name)))
  names(l) <- unique(filtdat$name)
  for(n in names(l)){
    l[[n]] <- geom_smooth(data=filtdat[filtdat$name==n,], method='lm', color='black')
  }
  ap <- ggplot(filtdat, aes(x=LWSsurfacedensity, y=surfacedensity, color=species)) + 
        geom_point() + facet_wrap(~species) + geom_abline(intercept=0, slope=1, linetype='dashed') + 
		l + geom_smooth(method='lm', color='yellow')
  
  # do some fitting
  getcoeffs <- function(d){
  # helper function
    thisline <- summary(lm(surfacedensity ~ LWSsurfacedensity, d))
	intercept <- thisline$coefficients[1,1]
	intercept.sd <- thisline$coefficients[1,2]
	slope <- thisline$coefficients[2,1]
	slope.sd <- thisline$coefficients[2,2]
	rsquared <- signif(thisline$r.squared, 2)
	r <- c(intercept, intercept.sd, slope, slope.sd, rsquared)
	names(r) <- c('intercept', 'intercept.sd', 'slope', 'slope.sd', 'rsquared')
	return(r)
  }
  # fit lines to individual trials
  sumdat <- data.frame(name=unique(filtdat$name))
  sumdat['species'] <- NA
  for(i in seq(nrow(sumdat)))
    sumdat[i, 'species'] <- unique(filtdat[filtdat$name==sumdat$name[i], 'species'])
  tres <- NULL
  for(i in seq(nrow(sumdat)))
    tres <- rbind(tres, getcoeffs(filtdat[filtdat$name==sumdat$name[i],]))
  sumdat <- cbind(sumdat, tres)
  # fit lines to species
  speciesdat <- data.frame(species=unique(filtdat$species))
  sres <- NULL
  for(i in seq(nrow(speciesdat)))
    sres <- rbind(sres, getcoeffs(filtdat[filtdat$species==speciesdat$species[i],]))
  speciesdat <- cbind(speciesdat, sres)
  # plot it
  sp <- ggplot(sumdat, aes(x=species, y=slope)) + geom_point() + geom_point(data=speciesdat, color='red') + 
        geom_errorbar(data=speciesdat, aes(ymin=slope - 1.96*slope.sd, ymax=slope + 1.96*slope.sd), width=0.2) +
		geom_text(data=speciesdat, aes(label=rsquared), hjust=0, size=4)
  ip <- ggplot(sumdat, aes(x=species, y=intercept)) + geom_point() + geom_point(data=speciesdat, color='red') +
        geom_errorbar(data=speciesdat, aes(ymin=intercept - 1.96*intercept.sd, ymax=intercept + 1.96*intercept.sd), width=0.2)
  return(list(sp, ip, ap))
}
