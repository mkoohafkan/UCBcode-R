scratchlabels <- function(rv, basepath){
	# rv = response variable
	#months <- seq(1,12)
	months <- 4
	years <- 2011
	#days <- NA
	days <- c(1, 2, 3)
	#hours <- seq(0, 23)
	hours <- NA
	cnt <- 0
	if(!any(is.na(days))){
		if(!any(is.na(hours))){
			scratchlabels <- vector("list", length(years)*length(months)*length(days)*length(hours))
		} else{
			scratchlabels <- vector("list", length(years)*length(months)*length(days))
		}
	} else{
		scratchlabels <- vector("list", length(years)*length(months))
	}
	for(year in years){
		for(month in months){
			if(!any(is.na(days))){
				for(day in days){
					if(!any(is.na(hours))){
						for(hour in hours){
							cnt=cnt + 1
							labelbits <- c(rv, '/', year, '-', month, '-', day, '-', hour, '/', rv, '_', year, '-', month, '-', day, '-', hour)
							scratchlabels[[cnt]] <- paste(labelbits, collapse="")
						}
					}else{
						cnt = cnt + 1			
						labelbits <- c(rv, '/', year, '-', month, '-', day, '/', rv, '_', year, '-', month, '-', day)
						scratchlabels[[cnt]] <- paste(labelbits, collapse="")
					}
				}
			} else{
				cnt = cnt + 1			
				labelbits <- c(rv, '/', year, '-', month, '/', rv, '_', year, '-', month)
				scratchlabels[[cnt]] <- paste(labelbits, collapse="")	
			}
		}
	}
	return(scratchlabels)
}

scratchqueries <- function(rv, dbtable){
	# rv = response variable
	# assumes you want projx, projy, z, rv
	#months <- seq(1,12)
	months <- 4
	years <- 2011
	#days <- NA
	days <- c(1, 2, 3)
	#hours <- seq(0, 23)
	hours <- NA
	cnt <- 0 
	if(!any(is.na(days))){
		scratchqueries <- vector("list", length(years)*length(months)*length(days))
	} else{
		scratchqueries <- vector("list", length(years)*length(months))
	}
	for(year in years){
		for(month in months){
			if(!any(is.na(days))){
				for(day in days){
					if(!any(is.na(hours))){
						for(hour in hours){
							cnt = cnt + 1
							querybits <- c('SELECT projx, projy, nodeid, z, ', rv, ' FROM ', dbtable, 
								' WHERE year=', year, ' AND month=', month, ' AND day=', day,
								' AND hour=', hour)
							scratchqueries[[cnt]] <- paste(querybits, collapse="")
						}
					} else{
						cnt <- cnt + 1
						querybits <- c('SELECT projx, projy, nodeid, z, ', rv, ' FROM ', dbtable, 
							' WHERE year=', year, ' AND month=', month, ' AND day=', day)
						scratchqueries[[cnt]] <- paste(querybits, collapse="")
					}
				}
			}else{
				cnt <- cnt + 1
				querybits <- c('SELECT projx, projy, nodeid, z, ', rv, ' FROM ', dbtable, 
						' WHERE year=', year, ' AND month=', month)
				scratchqueries[[cnt]] <- paste(querybits, collapse="")
			}
		}
	}
	return(scratchqueries)
}