# analyze_fogrun.r
analyze_fogrun <- function(procdata, calibrationstandard=data.frame(avg=5.33, stderr=0)){
# procdata is a list of objects produced by process_fogrun
# calibrationstandard = the mean and standard error of the
#   reference mass used for calibrating load cell
#   names(calibrationstandard) = c('avg', 'stderr')

###
	# helper functions
	summarize_load <- function(d, usecorrected=FALSE){
		if(usecorrected){ # tells whether to use CSEmV or SEmV 
			data.frame(timestamp=max(d$timestamp), avg=mean(d$CSEmV),
			                                       stdev=sd(d$CSEmV))
		} else {
			data.frame(timestamp=max(d$timestamp), avg=mean(d$SEmV),
			                                       stdev=sd(d$SEmV))
		}
	}
	calc_drift <- function(startvolt, endvolt){
		times <- seq(startvolt$timestamp, endvolt$timestamp, by=15)
		change <- c(0, seq(1, length(times) - 1)*(endvolt$avg - startvolt$avg)
		                                        /(length(times) - 1))
		return(data.frame(timestamp=times, driftmV=change))
	}
	correct_drift <- function(d, drift){
		# find segment of drift that corresponds to timestamps in d
		sidx <- match(d$timestamp[1], drift$timestamp)
		eidx <- match(d$timestamp[nrow(d)], drift$timestamp)
		d['driftmV'] <- drift$driftmV[seq(sidx, eidx)]
		d['CSEmV'] <- d$SEmV - d$driftmV
		return(d)
	}
	summarize_basevolt <- function(startvolt, endvolt){
		# combine the start and end baseline voltages to calculate
		# the error (variability) in measurements
		# we used separate averages for start and end baseline voltage
		# to calculate the drift. But now we combine all those baseline
		# measurements to understand the overall variation (error) in 
		# load cell readings
		list(avg=mean(c(startvolt$CSEmV, endvolt$CSEmV)),
		     stdev=sd(c(startvolt$CSEmV, endvolt$CSEmV))
		)
	}	
	strip_basevoltage <- function(d, basevoltstats){
		# adds a column representing the load cell measurement stripped of
		# the offset voltage when the cradle is attached (the 'baseline' or
		# 'background' voltage)
		d['loadmV.avg'] <- d$CSEmV - basevoltstats$avg
		d['loadmV.stderr'] <- 2*basevoltstats$stdev
		return(d)
	}
	calc_conversionfactor <- function(calibdat, refmass){
		meancalib <- mean(calibdat$loadmV.avg)
		stderrcalib <- mean(calibdat$loadmV.stderr)
		list(avg=refmass$avg/meancalib, stderr=stderrcalib)
	}
	calc_watermass <- function(wetdat, drydat, convdat){
		# wetdat is e.g. procdat$wetragvolt
		# drydat would then be procdat$dryragvolt
		# convdat is data for the conversion factor
		avgmass <- (mean(wetdat$loadmV.avg) - mean(drydat$loadmV.avg))*convdat$avg
		stderrmass <- (mean(wetdat$loadmV.stderr) + 
		               mean(drydat$loadmV.stderr))*convdat$loadmV.stderr
		list(avg=avgmass, stderr=stderrmass)
	}
####
	# get base voltage mean and stdev
	sbv <- summarize_load(procdata$startbasevolt)
	ebv <- summarize_load(procdata$endbasevolt)
	# calculate the base voltage drift assuming a linear change
	# (suggested by Campbell Scientific)
	measdrift <- calc_drift(sbv, ebv)
	# correct procdata for drift
	# this is a type B error analysis, correcting for systematic error
	# assumes startbasevolt is the earliest set of measurements
	# and endbasevolt is the latest set of measurements
	# could cause problems if endcalibvolt occurs after endbasevolt!
	# (which is why I'm not doing anything with endcalibvolt...)
	procdata$startcalibvolt <- correct_drift(procdata$startcalibvolt, measdrift)
	# NOT RUN: procdata$endcalibvolt <- correct_drift(procdata$endcalibvolt)
	procdata$dryragvolt <- correct_drift(procdata$dryragvolt, measdrift)
	procdata$wetragvolt <- correct_drift(procdata$wetragvolt, measdrift)
	procdata$dryleafvolt <- correct_drift(procdata$dryleafvolt, measdrift)
	procdata$wetleafvolt <- correct_drift(procdata$wetleafvolt, measdrift)
	procdata$rundata <- correct_drift(procdata$rundata, measdrift)
	# also correct endbasevolt for drift, and add a dummy 
	# column to startbasevolt
	procdata$endbasevolt <- correct_drift(procdata$endbasevolt, measdrift)
	procdata$startbasevolt['CSEmV'] <- procdata$startbasevolt$SEmV
	# get summarize basevolt to understand variability in baseline voltage
	# this is type A error analysis
	bvstats <- summarize_basevolt(procdata$startbasevolt, procdata$endbasevolt)
	# strip the baseline voltage from everything and propagate the type A
	# error analysis
	procdata$dryragvolt <- strip_basevoltage(procdata$dryragvolt, bvstats)
	procdata$wetragvolt <- strip_basevoltage(procdata$wetragvolt, bvstats)
	procdata$dryleafvolt <- strip_basevoltage(procdata$dryleafvolt, bvstats)
	procdata$wetleafvolt <- strip_basevoltage(procdata$wetleafvolt, bvstats)
	procdata$rundata <- strip_basevoltage(procdata$rundata, bvstats)
	procdata$startcalibvolt <- strip_basevoltage(procdata$startcalibvolt, bvstats)
	# get conversion factor from calibration data
	cf <- calc_conversionfactor(procdata$startcalibvolt, calibrationstandard)
	# calculate water mass on leaf and LWS
	lwswm <- calc_watermass(procdata$wetragvolt, procdata$dryragvolt, cf)
	lwm <- calc_watermass(procdata$wetleafvolt, procdata$dryleafvolt, cf)
	# add results of the analysis
	procdata[['LWSwater']] <- lwswm
	procdata[['leafwater']] <- lwm
	return(procdata)
}