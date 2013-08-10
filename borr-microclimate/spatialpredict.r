#######################################################################
# name: spatialpredict.r
# author: Michael Koohafkan
# depends: borr-modelfuncs.r, borr-dbfuncs.r, borr-etfuncs.r, pointsplot.r
# purpose: using a specific table in the database and a specific set of
# solar radiation raster layers, predict ET at each point location for
# each day specified. Multiple ET models are used and the results are plotted
# as Voronoi polygons (plotting defined in pointspredict.r) and saved to
# pdf files.
#######################################################################
#
# TODO: 

source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-dbfuncs.r")
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-etfuncs.r")
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-modelfuncs.r")
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-microclimate/pointsplot.r")

# path to solar radiation data
radpath <- "C:/repositories/codeRepo/sol/nodesglobrad.csv"
insolpath <- "C:/repositories/codeRepo/sol/nodesinsol.csv"

# Query database to get table mnthlydat
# names(mnthlydat) = c(deviceid, projx, projy, z, slope, aspect, canopyheight, relz1500ft, result_time)
# choice of basetable determines monthly, daily!

# get the climate data and predictors
responses <- c('temperature_min')
#responses <- c('temperature_avg', 'temperature_min', 'temperature_max', 
#               'humidity_avg', 'humidity_min', 'humidity_max')
predictors <- c('z', 'slope', 'aspect', 'canopyheight', 'relz1500ft')
fields <- c('deviceid', 'result_time', 'projx', 'projy', responses, predictors)
basetable <- 'michael.krige_trh_monthly'
whereclause <- "where result_time between '2011-01-01' and '2011-12-31'"
#whereclause <- "where result_time = '2012-08-01'"
query <- paste('select', paste(fields, collapse=', '), 'from', basetable, 
               whereclause, 'order by result_time, deviceid', sep=' ')
conn <- connectBORR()
climatedat <- datafromDB(query, fields, conn)

# add a 'period' column
# helper function to allow easy modification of data splitting
format_periods <- function(d, pcol){
	periods <- rep(NA, nrow(d))
	for(i in seq(1, nrow(d))){
	  periods[i] <- format(d[i, pcol], "%Y-%m")
	}	
	return(periods)
}
climatedat['period'] <- format_periods(climatedat, 'result_time')
periods <- unique(climatedat[['period']])

# get the solar data
radiationdat <- read.csv(radpath, header=TRUE)
insolationdat <- read.csv(insolpath, header=TRUE)
datperiod <- vector("list", length=length(periods))
names(datperiod) <- periods
for(p in periods){
	r <- radiationdat
	s <- insolationdat
	r['radavg'] <- 0.0036*solar_mnthavg(r, as.integer(substr(p, 6, 8)), 'RADDOY')
	s['insolavg'] <- solar_mnthavg(s, as.integer(substr(p, 6, 8)), 'INSDOY')
	solardat <- merge(r[, c('deviceid', 'radavg')], 
	                  s[, c('deviceid', 'insolavg')],
					  by = 'deviceid')
	datperiod[[p]] <- merge(climatedat[climatedat[['period']] == p,], 
	                               solardat, by = 'deviceid')
	rm(r, s, solardat)
}
predictors <- c(predictors, 'radavg', 'insolavg')

# get model fitting summary data for each period
# initialize structure: list of lists, all periods for each response
modelresponse <- vector("list", length=length(responses))
names(modelresponse) <- responses
for(i in seq(along=modelresponse)){
	modelresponse[[i]] <- vector("list", length=length(datperiod))
	names(modelresponse[[i]]) <- periods
}
# intialize these ones too
modelsummary <- modelresponse

# fit model for each period and response
for(p in periods){
	for(r in responses){
		# run glmulti, first with relz500
		firstmodel <- pick_model(datperiod[[p]], r, predictors, exc=c('relz1500ft'))
		# then with relz1500
		secondmodel <- pick_model(datperiod[[p]], r, predictors, exc=c('relz500ft'))
		# use consensus to combine them and remove duplicate models
		modelresponse[[r]][[p]] <- consensus(list(firstmodel, secondmodel))
		# get summary table for 10 best models
		modelsummary[[r]][[p]] <- weightable(modelresponse[[r]][[p]])[1:10,]
	}
}
# develop bestmodels table
# bestmodel columns are period, formula, modelweight
bestmodels <- vector('list', length=length(responses))
names(bestmodels) <- responses
for(r in responses){
	for(p in periods){
		bestmodels[[r]] <- rbind(bestmodels[[r]], 
         		   cbind(p, modelsummary[[r]][[p]][1, c('model', 'weights')]))
	}
	names(bestmodels[[r]]) <- c('period', 'model', 'weight')
}

# helper function to check 500ft vs 1500ft buffer
compare_buffers <- function(first, second, container){
	for(r in names(first)){
		for(p in names(first[[1]])){
			container[[r]][[p]] <- glmulti(c(first[[r]][[p]]@objects, 
			                                 second[[r]][[p]]@objects),
										   crit='bic', method='h')
		}
	}
	return(container)
}
