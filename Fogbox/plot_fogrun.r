# plot_fogsummary.r
# functions used in process wrapper to plot data
require(ggplot2)
require(reshape2)
require(gridExtra)
LWScurve <- function(dl){
# plot the LWS water mass vs. voltage curve
	# summarize the data
	fogsummary <- data.frame(name=sapply(dl, function(x) x$name),
				             LWSwater.avg=sapply(dl, function(x) x$LWSwatermass$avg),
				             LWSwater.stdev=sapply(dl, function(x) x$LWSwatermass$stdev),
				             LWSvolt.max=sapply(dl, function(x) x$maxLWSvolt),
				             LWSvolt.last=sapply(dl, function(x) x$lastLWSvolt),
							 species=sapply(dl, function(x) x$species))
	# reformat the data for plotting
	d <- melt(fogsummary, meas.vars=c('LWSvolt.max', 'LWSvolt.last'),
	          id.vars=c('name', 'species', 'LWSwater.avg', 'LWSwater.stdev'))
	names(d)[names(d)=='variable'] <- 'measurement'
	names(d)[names(d)=='value'] <- 'LWSmV'
	return(ggplot(d, aes(x=LWSmV, y=LWSwater.avg, color=species)) + geom_point() +
	       geom_errorbar(aes(ymin=LWSwater.avg - 1.96*LWSwater.stdev,
	                         ymax=LWSwater.avg + 1.96*LWSwater.stdev,
							 xmin=LWSmV - 0.05, xmax=LWSmV + 0.05)) +
	       xlab('LWS voltage (mV)') + ylab('water mass on LWS (g)') +
           scale_x_continuous(limits=c(274, 500)) + facet_wrap(~measurement))
}
runplot <- function(dl){
# plot the rundata
	# merge the rundata from each trial
	alldat <-NULL
	for(i in seq(along=dl)){
		dl[[i]]$rundata['tag'] <- dl[[i]]$name
		dl[[i]]$rundata['seconds'] <- 15*seq(0, nrow(dl[[i]]$rundata) - 1)
		alldat <- rbind(alldat, dl[[i]]$rundata[c('tag', 'surfacedensity',
		                                          'LWSmV', 'surfacedensity.sd',
												  'seconds')])
	}
	# plot it
	p1 <- ggplot(alldat, aes(x=seconds, y=surfacedensity)) + 
	      geom_point(aes(color=tag)) +
          geom_errorbar(aes(ymin=surfacedensity - 1.96*surfacedensity.sd,
	                        ymax=surfacedensity + 1.96*surfacedensity.sd,
	                        color=tag))
	p2 <- ggplot(alldat, aes(x=seconds, y=LWSmV)) + geom_point(aes(color=tag)) +
	      geom_errorbar(aes(ymin=LWSmV - 0.05, ymax=LWSmV + 0.05, color=tag))
	# side by side
	grid.arrange(p1, p2, nrow=2)
}

