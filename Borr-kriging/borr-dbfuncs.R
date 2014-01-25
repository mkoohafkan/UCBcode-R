#######################################################################
# name: borr-dbfuncs.r
# author: Michael Koohafkan
# purpose: function definitions for database querying and data cleaning
#######################################################################
#
# TODO:

require(RPostgreSQL)

connectBORR <- function(){
	drv <- dbDriver("PostgreSQL")
	dbName <- 'borrmod' 
	dbUser <- 'postgres' 
	dbPassword <- 'borr'
	port <- 5433
	conn <- dbConnect(drv, dbname=dbName, port=port, user=dbUser, password=dbPassword)
	return(conn)
}

datafromDB <- function(query, fields, conn){
	raw <- dbSendQuery(conn, query) 
	sampleset <- as.data.frame(fetch(raw, n=-1))
	if(nrow(sampleset) < 1){
		warning('empty result set returned by query, returning NULL.\n')
		return(NULL)
	} else{
		names(sampleset) <- fields		
		return(cleandata(sampleset))
	}
}

getnodeproperties <- function(conn, tbl='xserv_join_map_info', id='deviceid'){
	query <- paste('SELECT DISTINCT', paste(tbl, '.', id, sep=""), 'AS nodeid,',
				   'projx, projy, z', 
				   'FROM', tbl, 
				   'LEFT OUTER JOIN node_properties',
				   'ON (', paste(tbl, '.',id, sep=""),
				   '= node_properties.nodeid)',
				   'ORDER BY nodeid', sep=' ')
	fields <- c('nodeid', 'projx', 'projy', 'z')
	return(datafromDB(query, fields, conn)[[1]])
}

gridtoDF <- function(fpath, predictvar='z'){
	rawgrid <- read.csv(fpath)
	unsampled_df <- data.frame(projx=rawgrid[['POINT_X']],
			                   projy=rawgrid[['POINT_Y']])
	unsampled_df[predictvar] <- rawgrid[['GRID_CODE']]						
	return(cleandata(unsampled_df))
}

cleandata <- function(dataset){
	# cleans the provided dataset according to label
	# processing of dataset depending on label  
	clean.z <- function(ds){ # convert feet to meters
		cleandata <- ds*0.3048
		cleandata[ds < 0] = NA
		return(cleandata)
	}
	clean.xy <- function(ds){ # convert feet to kilometers
		cleandata <- ds #*0.3048/1000
		return(cleandata)
	}
	clean.rh <- function(ds){ # remove negative rh values
		cleandata <- ds
		cleandata[ds < 0] <- NA
		return(cleandata)
	}
	clean.temp <- function(ds){ # Celsius 
		cleandata <- ds				# no conversion
		#cleandata <- ds + 273.15	# to Kelvin
		#cleandata <- ds*9/5 + 32	# to Fahrenheit
		return(cleandata)
	}
	clean.aspect <- function(ds){ # convert to 'degrees from 200'
		# aspect = 200 is south-facing slope
		# NA are bad data
		flatmask <- ds < 0
		cleandata <- abs(200 - ds)
		cleandata[flatmask] <- 0
		#cleandata <- rep(NA, length(ds))
		#cleandata[ds > 360] <- NA # bad data		
		#cleandata[ds < 0] <- "F" # assumes negative values are placeholders for flat
		#cleandata[(ds > 0 && (ds < 45 || ds >= 315) )] <- "N" # north facing slopes
		#cleandata[ds >= 45 && ds < 135] <- "E" # east facing slopes
		#cleandata[ds >= 135 && ds < 225] <- "S" # south facing slopes
		#cleandata[ds >= 225 && ds < 315] <- "W" # west facing slopes
		return(cleandata)
	}
	clean_windspeed <- function(ds){ # remove negative values
		cleandata <- ds
		cleandata[ds < 0] = NA
		return(cleandata)
	}
	clean.slope <- function(ds){ #convert low slopes to 0
		#  also flag negative slopes
		cleandata <- ds
		cleandata[ds < 0] <- NA
		return(cleandata)
	}
	clean_canopy <- function(ds){}
	
	# clean the data
	for(label in sort(names(dataset), decreasing=TRUE) ){ 
		vartype <- unlist(strsplit(paste(label, collapse=""), "_"))[1]
		if(vartype == 'temperature'){
			dataset[label] <- clean.temp(dataset[[label]])
		} else if(vartype == 'humidity'){
			dataset[label] <- clean.rh(dataset[[label]])
		} else if(vartype == "aspect"){
			dataset[label] <- clean.aspect(dataset[[label]])
		} else if(vartype == "slope"){
			dataset[label] <- clean.slope(dataset[[label]])
		} else if(any(vartype == c("projx", "projy"))){
			dataset[label] <- clean.xy(dataset[[label]])
		} else if(any(vartype == c("z", 'relz1500ft', 'relz500ft'))){
			dataset[label] <- clean.z(dataset[[label]])
		}else if(vartype == "windspeed"){
			dataset[label]<- clean_windspeed(dataset[[label]])
		}else if(vartype == "winddir"){
			#dataset[label]<- clean_aspect(dataset[[label]])
		}
	}
	return(dataset)
}

replacexy <- function(ds, neards){
	# function to modify node xy to match nearest raster cell
	# expects df to have columns "projx", "projy"
	# expects neardf to have columns "projx", projy", "nearx", "neary"
	# neardf "projx", "projy" columns should include all "projx", "projy" in df
	for(i in seq(1, nrow(ds), 1)){
		ds[["projx"]][i] <- neards[["nearx"]][which(neards[["nodeid"]] == ds[["nodeid"]][i])]
		ds[["projy"]][i] <- neards[["neary"]][which(neards[["nodeid"]] == ds[["nodeid"]][i])]
	}
return(ds)
}