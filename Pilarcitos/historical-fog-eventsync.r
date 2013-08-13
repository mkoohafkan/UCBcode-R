## analyze the 1920s SFPUC fog record using event synchronization

# run parameters
fogpath <- paste(getwd(), '/Pilarcitos/fogrecord.csv', sep='')
# climate is either foggy 'f', rainy 'r' or clear 'c'
climate <- 'c' # 'c'
lastclimate <- 'f' 

## helper functions

# calculate probabilities
get_probabilities <- function(d, cond){
	#expects a dataframe d
	# names(d) = c('day', time, location1, location2)
	# no missing data
	numrecords <- nrow(d)
	probigivenj <- nrow(d[d[, 1] == cond,] & d[d[, 2] == cond,])/nrow(d[d[,2] == cond,])
	probi <- nrow(d[d[, 1],])
	
}


# DEPRECATED split record into list of dataframes, each containing one day's record
return_days <- function(d){
	# expects a dataframe d
	# names(d) = c(location1, location2)
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
	# this version considers 'overnight' as within the window
	synced <- 0
	numevents <- 0
	# at first, this will just record the change in cn
	timeresolved <- rep(0, nrow(d)) # TODO FIX THIS
	# check first two records for event sync
	if(d[2,1] == condition & any(d[1,1] == prior)){
		# an event!
		numevents <- numevents + 1
		if(d[2, 2] == condition & any(d[1,1] == prior)){
			# a synchronized event!
			synced <- synced + 0.5
		}
	}
	# check remaining records
	for(i in seq(3, nrow(d), 1)){
		if(d[i,1] == condition & any(d[i-1,1] == prior)){
			# an event!
			numevents <- numevents + 1
			if(d[i, 2] == condition & any(d[i-1,1] == prior)){
				# a synchronized event!
				synced <- synced + 0.5
			}
			else if(d[i-1, 2] == condition & any(d[i-2,1] == prior)){
				# a preceding event!
				synced <- synced + 1
				timeresolved[i] <- 1
			}
		}
	}
	
	return(list(synced=synced, numevents=numevents, 
	            timeresolved=cumsum(timeresolved)))
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
timeresolvedsync <- vector('list', length=length(pairsync))
timeresolveddelay <- timeresolvedsync
# Quiroga: time-resolved

# for each pair of locations, check event sync
for(i in seq(1, nrow(locationpairs), 1)){
	# select data for locationpair
	locpair <- locationpairs[i,]
	# by day
	# or including overnight
	subdat <- fogdata[, c('day', 'time', locpair)]
	iloc <- locpair[1]
	jloc <- locpair[2]
	# Quiroga: 
	# igivenj = c(x|y) mx cn(x|y)
	# jgiveni = c(y|x) my cn(y|x)
	# calculate sync terms
	igivenj <- event_sync(subdat[,c(iloc, jloc)], 
									climate, lastclimate)
	jgiveni <- event_sync(subdat[,c(jloc, iloc)], 
									climate, lastclimate)
	# Quiroga:
	pairsync[i] <- (jgiveni[[1]] + igivenj[[1]])/sqrt(jgiveni[[2]]*igivenj[[2]])
	pairdelay[i] <- (jgiveni[[1]] - igivenj[[1]])/sqrt(jgiveni[[2]]*igivenj[[2]])
	timeresolveddelay[[i]] <- jgiveni[[3]] - igivenj[[3]] # TODO fix event_sync
	timeresolvedsync[[i]] <- jgiveni[[3]] + igivenj[[3]]
}
quirogasummary <- data.frame(x=locationpairs[,1], y=locationpairs[,2], 
							 sync=pairsync, delay=pairdelay, 
							 stringsAsFactors=FALSE)
quirogafixed <- fix_direction(quirogasummary)
# clarify the names
names(quirogafixed)[c(1, 2)] <- c('from', 'to')

## plotting
require(grid)
source(paste(getwd(), '/ggnetworks/network-plotfuncs.r', sep=''))
require(ggplot2)
v <- data.frame(id=locations, x=c(0, 0, 3), y=c(0, 3, 0))
opts <- vector('list', length=5)
opts[[1]] <- theme_bw()
opts[[2]] <- scale_color_gradient2(low="grey", mid="orange", high="red", 
								   limits=c(0, 1), midpoint=0.5)
opts[[3]] <- scale_size('q', range=c(0.1, 1))
fog <- ggplot(fortify_edges(quirogafixed, v)) + opts + 
       geom_segment(aes(x=x, y=y, xend=xend, yend=yend, color=sync, 
						size=delay), arrow=arrow() )
						
## probabilities
