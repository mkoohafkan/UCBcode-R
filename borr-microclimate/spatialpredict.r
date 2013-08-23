#######################################################################
# name: spatialpredict.r
# author: Michael Koohafkan
# depends: borr-modelfuncs.r, borr-dbfuncs.r, borr-etfuncs.r, more-plot-funcs.r
# purpose: using a specific table in the database and a specific set of
# solar radiation raster layers, predict ET at each point location for
# each day specified. Multiple ET models are used and the results are plotted
# as Voronoi polygons (plotting defined in pointspredict.r) and saved to
# pdf files.
#######################################################################
#
# TODO: 
require(gridExtra)
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-dbfuncs.r")
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-modelfuncs.r")
# path to solar radiation data
radpath <- "C:/repositories/codeRepo/UCBcode-GIS/trunk/data/BORR-solar/nodesglobrad.csv"
insolpath <- "C:/repositories/codeRepo/UCBcode-GIS/trunk/data/BORR-solar/nodesinsol.csv"

# helper function to initialize structure: 
# creates list of lists, all periods for each response
model_struct <- function(topnames, subnames){
	model_struct <- vector('list', length=length(topnames))
	names(model_struct) <- topnames	
	for(n in topnames){
		model_struct[[n]] <- vector('list', length=length(subnames))
		names(model_struct[[n]]) <- subnames
	}
	return(model_struct)
}

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
	# use deviceid as rownames, drop redundant column
	rownames(datperiod[[p]]) <- datperiod[[p]][['deviceid']]
	datperiod[[p]][['deviceid']] <- NULL
	rm(r, s, solardat)
}
predictors <- c(predictors, 'radavg', 'insolavg')
# get model fitting summary data for each period
#initialize containers
modelresponse <- model_struct(responses, periods)
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
# develop bestable table
# bestable columns are period, formula, modelweight
bestable <- vector('list', length=length(responses))
names(bestable) <- responses
for(r in responses){
	for(p in periods){
		bestable[[r]] <- rbind(bestable[[r]], 
      	 cbind(p, modelsummary[[r]][[p]][1, c('model', 'weights')]))
	}
	names(bestable[[r]]) <- c('period', 'model', 'weight')
}
# develop best models
bestmodel <- model_struct(responses, periods)
for(r in responses){
	for(p in periods){
		bestmodel[[r]][[p]] <- lm(modelresponse[[r]][[p]]@formulas[[1]],
		                          datperiod[[p]])
	}
}
# plot everything
modelplot <- model_struct(responses, periods)
for(r in responses){
	for(p in periods){
		modelplot[[r]][[p]] <- tryCatch({
			plot_model(bestmodel[[r]][[p]], get_all_vars(bestmodel[[r]][[p]], 
			                                             datperiod[[p]])) + 
			ggtitle(p)
		}, error = function(err){
			warning(paste('there was a problem plotting', p))
			return(NA)
		})
	}
}
# Code to override clipping
for(r in responses){
	for(p in periods){
		# create formula annotation
		# Create the textGrobs
		frmgrob <- textGrob(modelresponse[[r]][[p]]@formulas[[1]])
		frmx <- min(datperiod[[p]][,'z']) + 0.5*(max(datperiod[[p]][,'z']) - 
		                                       min(datperiod[[p]][,'z']))
		frmy <- min(datperiod[[p]][,r]) - 0.5*(max(datperiod[[p]][,r]) - 
		                                      min(datperiod[[p]][,r]))
		frmannotate <- annotation_custom(grob=frmgrob, xmin=frmx, xmax=frmx, 
		                                 ymin=frmy, ymax=frmy)
		gt <- ggplot_gtable(ggplot_build(modelplot[[r]][[p]] + frmannotate))
		gt$layout$clip[gt$layout$name=="panel"] <- "off"
		grid.draw(gt)
	}
}