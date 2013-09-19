# plot_fogsummary.r
# functions used in process wrapper to plot data
require(ggplot2)
require(reshape2)
require(gridExtra)
LWScurve <- function(fogsummary){
# plot the LWS water mass vs. voltage curve
  # helper function
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
	lastmodel <- lm(LWSwater.avg ~ LWSvolt.last, fogsummary, weights=1/LWSwater.stdev)
	nd <- data.frame(LWSvolt.last=seq(274, 500))
	preds <- predict(lastmodel, newdata=nd, type = 'response',se = TRUE)
	nd['LWSwater.avg'] <- preds$fit
	nd['LWSwater.stdev'] <- preds$se.fit
	p <- ggplot(fogsummary, aes(x=LWSvolt.last, y=LWSwater.avg)) + 
	     geom_point(aes(color=species), size=1.5) +
	     geom_errorbar(aes(ymin=LWSwater.avg - 1.96*LWSwater.stdev,
	                       ymax=LWSwater.avg + 1.96*LWSwater.stdev,
		                   color=species), width=0.1) +
		 geom_ribbon(data=nd, aes(ymin=LWSwater.avg - 1.96*LWSwater.stdev,
	                              ymax=LWSwater.avg + 1.96*LWSwater.stdev,
								  x=LWSvolt.last), alpha=0.3) +
		 geom_line(data=nd, aes(x=LWSvolt.last, y=LWSwater.avg), color='black') +
	     annotate("text", label=lm_eqn(lastmodel), x=375, y=0.225, parse=TRUE) +
		 xlab('LWS voltage (mV)') + ylab('water mass on LWS (g)') +
         scale_x_continuous(limits=c(274, 500))
	return(list(lastmodel, p))
}
runplot <- function(dl){
# plot the rundata
	# merge the rundata from each trial
	alldat <-NULL
	for(i in seq(along=dl)){
		dl[[i]]$rundata['name'] <- dl[[i]]$name
		dl[[i]]$rundata['species'] <- dl[[i]]$species
		dl[[i]]$rundata['trial'] <- dl[[i]]$trial
		dl[[i]]$rundata['seconds'] <- 15*seq(0, nrow(dl[[i]]$rundata) - 1)
		alldat <- rbind(alldat, dl[[i]]$rundata[c('species', 'surfacedensity',
		                                          'LWSmV', 'surfacedensity.sd',
												  'seconds', 'trial', 'name')])
	}
	# plot it
	p1 <- ggplot(alldat, aes(x=seconds, y=surfacedensity, color=trial)) + 
	      geom_point() +
          geom_errorbar(aes(ymin=surfacedensity - 1.96*surfacedensity.sd,
	                        ymax=surfacedensity + 1.96*surfacedensity.sd)) +
		  facet_wrap(~species)
	
	p2 <- ggplot(alldat, aes(x=seconds, y=LWSmV, color=trial)) + geom_point() +
	      geom_errorbar(aes(ymin=LWSmV - 0.05, ymax=LWSmV + 0.05), width=0.1) +
		  facet_wrap(~species)
	return(list(p1, p2))
}

ratioplot <- function(dl){
  runsammary <- data.frame()
}

