## analyze the 1920s SFPUC fog record using event synchronization

# run parameters
fogpath <- paste(getwd(), '/Pilarcitos/fogrecord.csv', sep='')
# climate is either foggy 'f', rainy 'r' or clear 'c'
climate <- 'c' # 'c'
lastclimate <- 'f' 

## helper functions

# split record into list of dataframes, each containing one day's record
return_days <- function(d){
	# expects a dataframe d
	# names(d) = c('day', time, location1, location2)
	# 5 records a day
	# no missing data
	dayindex <- seq(1, nrow(d), 5)
	thedate <- rep(NA, length(dayindex))
	days <- vector('list', length=length(dayindex))
	wasrainy <- rep(0, length(days))
	for(i in seq(along=days)){
		records <- seq(dayindex[i], dayindex[i] + 4, 1)
		days[[i]] <- d[records,]
		thedate[[i]] <- days[[i]]['day'][1]
	}
	names(days) <- dayindex
	return(days)
}

event_sync <- function(d, condition, prior=c('f', 'r', 'c')){
	# expects ONE DAY's worth of records for two locations
	# 5 records per day
	# condition is transition to observe (clear 'c', foggy 'f')
	# use prior to filter out e.g. rain->fog transitions
	# d is a dataframe containing 3 columns
	# names(d) = c('date', time, location1, location2)	
	# Quiroga: location1 = x, location2 = y
	synced <- 0 # Quiroga: c(x|y)
	numevents <- 0 # Quiroga: mx
	for(i in seq(2, 5, 1)){ # 5 entries per day
		if(d[i, 1] == condition & any(d[i-1, 1] == prior) ){
			# is an event
			numevents <- numevents + 1
			if(d[i, 2] == condition & any(d[i-1, 2] == prior) ){
				# same thing happened at other site at same time
				synced <- synced + 0.5
			}
			# check if event at location2 preceded event at location1
			if(d[i-1, 2] == condition){
				if(i < 3){
				# it's the first entry of morning, just call it preceding
					#synced <- synced + 1
				} else {
					# late enough in the day that we can check for event
					if(any(d[i-2, 2] == prior))
						synced <- synced + 1 # happened at other site first
				}
			}
		}
	}			
	return(c(synced, numevents))
}

fix_direction <- function(d){
	# names(d) = c("x", "y", "sync", "delay")
	for(i in seq(1, nrow(d), 1)){
		if(d[['delay']][i] < 0){
			# swap locations so that delay is always positive
			tmp <- d[['y']][i]
			d[['y']][i] <- d[['x']][i]
			d[['x']][i] <- tmp
			d[['delay']][i] <- abs(d[['delay']][i])
		}
	}
	return(d)
}

## main program

# read the data
fogdata <- read.csv(fogpath, header=TRUE)
locations <- names(fogdata)[c(3,4,5)]
locationpairs <- t(combn(locations, 2))
pairsync <- rep(NA, nrow(locationpairs))
pairdelay <- pairsync

# for each pair of locations, check event sync
for(i in seq(1, nrow(locationpairs), 1)){
	# select data for locationpair
	locpair <- locationpairs[i,]
	subdat <- return_days(fogdata[, c('day', 'time', locpair)])
	iloc <- locpair[1]
	jloc <- locpair[2]
	numrecords <- length(subdat)*nrow(subdat[[1]])
	# Quiroga: 
	igivenj <- c(0, 0) # c(x|y)
	jgiveni <- c(0, 0) # c(y|x)
	# go through each day and calculate sync terms
	for(j in seq(along=subdat)){
			igivenj <- igivenj + event_sync(subdat[[j]][,c(iloc, jloc)], 
											climate, lastclimate)
			jgiveni <- jgiveni + event_sync(subdat[[j]][,c(jloc, iloc)], 
											climate, lastclimate)
	}
	# Quiroga:
	pairsync[i] <- (jgiveni[1] + igivenj[1])/sqrt(jgiveni[2]*igivenj[2])
	pairdelay[i] <- (jgiveni[1] - igivenj[1])/sqrt(jgiveni[2]*igivenj[2])
}
quirogasummary <- data.frame(x=locationpairs[,1], y=locationpairs[,2], 
							 sync=pairsync, delay=pairdelay, 
							 stringsAsFactors=FALSE)
quirogafixed <- fix_direction(quirogasummary)
# clarify the names
names(quirogafixed)[c(1, 2)] <- c('from', 'to')

## plotting
source(paste(getwd(), '/ggnetworks/network-plotfuncs.r', sep=''))
require(ggplot2)
v <- data.frame(id=locations, x=c(0, 0, 3), y=c(0, 3, 0))
opts <- vector('list', length=5)
opts[[1]] <- theme_bw()
opts[[2]] <- scale_color_gradient2(low="grey", mid="orange", high="red", 
								   limits=c(0, 1), midpoint=0.5)
opts[[3]] <- scale_size('q', range=c(0.1, 1))
fog <- ggplot(quirogafixed) + plot_graph(v, quirogafixed, TRUE, 'delay') + opts
