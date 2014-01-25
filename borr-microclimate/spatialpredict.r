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
require(intamap)
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-dbfuncs.r")
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-modelfuncs.r")
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-autofitmod.r")
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-plotfuncs.r")
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
predictors <- c('z')
#predictors <- c('z', 'slope', 'aspect', 'relz1500ft')
#predictors <- c('z', 'slope', 'aspfrom200', 'relz500ft', 'ndvi')
fields <- c('deviceid', 'result_time', 'projx', 'projy', responses, predictors)
basetable <- 'michael.krige_trh_monthly'
whereclause <- "where result_time between '2011-05-01' and '2011-08-31'"
#whereclause <- "where result_time = '2011-08-01'"
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

# principle component analysis
pcaresults <- vector('list', length=length(responses))
names(pcaresults) <- responses
for(r in responses){
  pform <- as.formula(paste('~', paste(predictors, collapse='+')))
  pcaresults[[r]] <- princomp(pform, data=datperiod[[p]], scale=TRUE)
}

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
		                          datperiod[[p]], na.action=na.exclude)
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

setwd('./GISproj/')
# save plots
for(r in responses){
  for(p in periods){
    ggsave(paste(r, '_', p, '_trend.png', sep=''), plot=modelplot[[r]][[p]], 
	       width=6.5, height=2.25, dpi=300)
  }
}



# plots for temperature_max, humidity_min
modelplot <- model_struct(responses, periods)
for(r in responses){
  for(p in periods){
    modelplot[[r]][[p]] <- ggplot(datperiod[[p]], aes_string(x='z', y=r)) +
                           geom_text(aes(label=rownames(datperiod[[p]])), size=2)
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

# make the data spatial
# helper function
spatialize <- function(model, modeldata){
    # trim and convert data to SpatialPointsDataFrame
	# add residuals
	modeldata['residual'] <- resid(model)
	# identify relevant columns
	dnames <- c('result_time', 'period', 'projx', 'projy', 
		        names(get_all_vars(formula(model), modeldata)), 'residual')
	# pull relevant data 
	returndat <- na.omit(modeldata[dnames])
	# transform into spatial dataframe
	coordinates(returndat) <- ~ projx + projy
	rownames(returndat@coords) <- row.names(returndat)
	return(returndat)
}
# initialize container
spatialdat <- model_struct(responses, periods)
for(r in responses){
	for(p in periods){
		# subset and spatialize the data
		spatialdat[[r]][[p]] <- spatialize(bestmodel[[r]][[p]], datperiod[[p]])
	}
}

# get the variograms
# helper function
test_variograms <- function(modelformula, modeldata){
  # create a bunch of variograms
  nbins <- seq(10, 20)
  binwidths <- seq(100, 500, by=50)
  cress <- FALSE
  pr <- FALSE
  npopts <- vector('list', length=length(nbins))
  widthopts <- vector('list', length=length(binwidths))
  for(i in seq(along=nbins))
    npopts[[i]] <- list(orig.behavior=FALSE, equal.np.bins=TRUE, num.bins=nbins[i])
  for(i in seq(along=binwidths))
    widthopts[[i]] <- list(orig.behavior=FALSE, equal.width.bins=TRUE, init.width=nbins[i])
  opts <- c(npopts, widthopts)
  testvgms <- vector('list', length=length(opts))
  for(i in seq(along=opts))
    testvgms[[i]] <- afvmod(modelformula, modeldata, PR=pr,cressie=cress, 
						    miscFitOptions=opts[[i]])
  return(testvgms)
}
# initialize container
variograms <- model_struct(responses, periods)
# fit the variograms
for(r in responses){
	for(p in periods){
		# fit the variogram
		variograms[[r]][[p]] <- test_variograms(formula(bestmodel[[r]][[p]]),
									            spatialdat[[r]][[p]])
	}
}
# plot the variograms
variogramplots <- model_struct(responses, periods)
for(r in responses){
	for(p in periods){
		variogramplots[[r]][[p]] <- annotatedplot(variograms[[r]][[p]])
		#dev.new()
		#print(variogramplots[[r]][[p]])
	}
}

# load the grid
gridpath <- 'C:/Users/Michael/Desktop/KrigLayers/pureR/daily/basegridxyz.txt'
basegrid <- gridtoDF(gridpath)
coordinates(basegrid) <- ~ projx + projy

# interpolate using intamap and idw
idp <- model_struct(responses, periods)
for(r in responses){
	for(p in periods){
	    # get the  mask
        gridmask <- basegrid[['z']] >= min(spatialdat[[r]][[p]][['z']]) & 
	                basegrid[['z']] <= max(spatialdat[[r]][[p]][['z']])
		interpdat <- createIntamapObject(observations=spatialdat[[r]][[p]], 
		                                 predictionLocations=basegrid,
										 formulaString=residual ~ 1)
		interpdat <- estimateParameters(interpdat)
		idp[[r]][[p]] <- interpdat$inverseDistancePower
		interpdat <- spatialPredict(interpdat)
		interpdat <- postProcess(interpdat)
		# retrend the data
		trendat <- as.data.frame(interpdat$predictions)
		names(trendat)[1] <- 'residual'
		trendat[r] <- trendat[['residual']] + predict(bestmodel[[r]][[p]], newdata=basegrid)
		# mask this shit
		trendat <- trendat[gridmask,]
		# plot this shit
		prange <- c(min(trendat[[r]]), mean(trendat[[r]]), max(trendat[[r]]))
		krigeraster <- krigeplot(ggplot(data=trendat, aes(x=projx, y=projy)), 
					             r, prange) + ylab('y (feet)') + xlab('x (feet)')
			       #geom_point(color="black", alpha=0.5, data=sampleobj)
	    ggsave(filename=paste(p, '.', r, ".krige.png", sep=""), 
		       plot=krigeraster, width=6, height=4.5, dpi=300)
	}
}

# write the spatial data to files
for(r in responses){
	for(p in periods){
		write.csv(spatialdat[[r]][[p]], paste(p, '.', r, '.csv', sep=''))
		save('spatialdat', 'datperiod', 'modelresponse', 'bestable', 
		     'bestmodel', 'idp', file=paste(p, '.', r, '.Rdata', sep=''))
	}
}





testdat <- as.data.frame(datperiod[[p]])
testdat['id'] <- rownames(testdat)

ggplot(testdat, aes(x=z, y=humidity_min, label=id)) + geom_text() + ggtitle(p)