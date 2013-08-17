# process_fogrun.r
process_fogrun <- function(path){
# function processes data from an individual run, formatted like:
# <trial name>
# STARTING BASE VOLTAGE
# rec 1
# rec 2
# rec 3
# ...
# STARTING CALIBRATION VOLTAGE
# rec 1
# rec 2
# ...
# DRY LEAF VOLTAGE
# rec 1
# rec 2...
# BEGIN RUN
# ...
# ...
# ...
# END RUN
# WET LEAF VOLTAGE
# rec 1
# ...
# DRY RAG VOLTAGE
# rec 1
# ...
# WET RAG VOLTAGE
# rec 1
# ...
# ENDING BASE VOLTAGE
# rec 1
# ...
# ENDING CALIBRATION VOLTAGE
# rec 1
# ...
# EOF
	# helper functions
	getmatch <- function(tag, dat, stopifmissing=TRUE){
	# get indices for data components
		idx <- match(tag, dat)
		if(is.na(idx)){
			if(stopifmissing){
				stop(paste(tag, 'is missing.'))
			} else {
				warning(paste(tag, 'is missing.'))
			}
		}
		return(idx)
	}
	scandata <- function(dat, idx){
	# pull numeric values 
		values <- NULL
		n <- idx
		while(!is.na(as.numeric(dat[n]))){
			values <- c(values, as.numeric(dat[n]))
			n <- n + 1
		}
		return(values)
	}	
	# read lines from file
	datalines <- readLines(path)
	# trial name should be first line
	name <- datalines[1]
	# indices for data processing
	startbaseidx <- getmatch('STARTING BASE VOLTAGE', datalines) + 1
	endbaseidx <- getmatch('ENDING BASE VOLTAGE', datalines) + 1
	startcalibrationidx <- getmatch('STARTING CALIBRATION VOLTAGE', datalines) + 1
	endcalibrationidx <- getmatch('ENDING CALIBRATION VOLTAGE', datalines, FALSE) + 1
	dryleafidx <- getmatch('DRY LEAF VOLTAGE', datalines) + 1
	wetleafidx <- getmatch('WET LEAF VOLTAGE', datalines) + 1
	dryragidx <- getmatch('DRY RAG VOLTAGE', datalines) + 1
	wetragidx <- getmatch('WET RAG VOLTAGE', datalines) + 1
	startrunidx <- getmatch('BEGIN RUN', datalines) + 1
	endrunidx <- getmatch('END RUN', datalines) - 1 
	# get start and end base voltages
	sbv <- scandata(datalines, startbaseidx)
	ebv <- scandata(datalines, endbaseidx)
	# get dry and wet rag voltages
	drv <- scandata(datalines, dryragidx)
	wrv <- scandata(datalines, wetragidx)
	# get dry and wet leaf voltages
	dlv <- scandata(datalines, dryleafidx)
	wlv <- scandata(datalines, wetleafidx)
	# get start (and end) calibration voltages
	scv <- scandata(datalines, startcalibrationidx)
	if(!is.na(endcalibrationidx)){ 
		ecv <- scandata(datalines, endcalibrationidx)
	} else {
		ecv <- NA
	}
	# get run data
	rd <- NULL
	for(l in seq(startrunidx, endrunidx)){
		rd <- rbind(rd, unlist(strsplit(datalines[l], '\t')))
	}
	rundata <- data.frame(timestamp=strptime(rd[, 1], "%I:%M:%S %p"), 
						  record=as.integer(rd[, 2]), LWSmV=as.numeric(rd[, 3]),
						  SEmV=as.numeric(rd[, 4]))
	return(list(name=name, startbasevolt=sbv, endbasevolt=ebv, dryragvolt=drv, 
	            wetragvolt=wrv, rundata=rundata, dryleafvolt=dlv, 
				wetleafvolt=wlv, startcalibvolt=scv, endcalibvolt=ecv))
}