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
  if(coef(m)[2] >= 0){
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
	nd <- data.frame(LWSvolt.last=seq(274, 500))
	preds <- predict(fitmod, newdata=nd, type='response', se=TRUE)
	nd['LWSwater.avg'] <- preds$fit
	nd['LWSwater.stdev'] <- preds$se.fit
	# add a label to fogsummary for plotting
    fogsummary['plab'] <- paste('  ', fogsummary$trial, 'rep', fogsummary$replicate, sep='')
	# plot
	ggplot(fogsummary, aes(x=LWSvolt.last, y=LWSwater.avg)) + 
	geom_point(aes(color=species), size=1.5) +
	geom_errorbar(aes(ymin=LWSwater.avg - 1.96*LWSwater.stdev,
	                  ymax=LWSwater.avg + 1.96*LWSwater.stdev,
		              color=species), width=0.1) +
    geom_ribbon(data=nd, aes(ymin=LWSwater.avg - 1.96*LWSwater.stdev,
	                         ymax=LWSwater.avg + 1.96*LWSwater.stdev,
							 x=LWSvolt.last), alpha=0.3) +
    geom_line(data=nd, aes(x=LWSvolt.last, y=LWSwater.avg), color='black') +
	geom_text(aes(label=plab), size=2, hjust=0, alpha=0.5, position='dodge') + 	
	annotate("text", label=lm_eqn(fitmod), x=375, y=0.225, parse=TRUE) +
	xlab('LWS voltage (mV)') + ylab('water mass on LWS (g)') +
    scale_x_continuous(limits=c(274, 500))
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
				xlab('record') + ylab('surface density of water on leaf')

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
  #summarydata['plab'] <- paste('  ', summarydata$trial, 'rep', summarydata$replicate, sep='')
  #summarydata['plab'] <- paste(' ', summarydata$leafarea)
  summarydata['plab'] <- paste('  ', format(summarydata$leafr2, digits=2), 
                               '/', format(summarydata$lwsr2, digits=2), sep='')
  ggplot(summarydata, aes(x=species, y=fitratio)) + geom_point() + 
  geom_hline(yintercept=1, linetype='longdash', alpha=0.5) +
  geom_text(aes(label=plab), size=2, hjust=0, alpha=0.5, position='dodge') + 
  ggtitle('slope ratio of leaf wetting to LWS wetting by species') +
  stat_summary(fun.y=mean, geom="point", color='red')
}
