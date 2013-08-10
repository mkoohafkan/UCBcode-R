#######################################################################
# name: pointspredict.r
# author: Michael Koohafkan
# depends: pointsplot.r, borr-dbfuncs.r, borr-etfuncs.r
# purpose: using a specific table in the database and a specific set of
# solar radiation raster layers, predict ET at each point location for
# each day specified. Multiple ET models are used and the results are plotted
# as Voronoi polygons (plotting defined in pointspredict.r) and saved to
# pdf files.
#######################################################################
#
# TODO: check all etfuncs, values for copais are whack

source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-dbfuncs.r")
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-kriging/borr-etfuncs.r")
source("C:/repositories/codeRepo/UCBcode-R/trunk/borr-microclimate/pointsplot.r")

# path to solar radiation data
radpath <- "C:/repositories/codeRepo/sol/noderad.csv"

# Query database to get table mnthlydat
# names(mnthlydat) = c(deviceid, projx, projy, z, slope, aspect, canopyheight, result_time)
# choice of basetable determines monthly, daily!

# get the climate data and predictors
responsevars <- c('temperature_avg', 'temperature_min', 'temperature_max',
				  'humidity_avg', 'humidity_min', 'humidity_max')
fields <- c('deviceid', 'result_time', 'projx', 'projy', 'z', responsevars)
basetable <- 'michael.krige_trh_monthly'
query <- paste('select', paste(fields, collapse=', '), 'from', basetable, 
               "where result_time between '2011-01-01' and '2011-12-31'", 
               'order by result_time, deviceid', sep=' ')
conn <- connectBORR()
climatedat <- datafromDB(query, fields, conn)
# add a 'period' column
periods <- rep(NA, nrow(climatedat))
for(i in seq(1, nrow(climatedat))){
  periods[i] <- as.integer(format(climatedat[i, 'result_time'], "%m"))
}
climatedat['period'] <- periods

# get the solar radiation data
# solar data is named like RADDOY#9, RADDOY23, RADDOY365 etc.
# names(solardat) = c('deviceid', 'RADDOY9', ...)
solardat <- read.csv(radpath, header=TRUE)

# helper data frame for splitting by solar radiation days
doyconv <- data.frame(doy=c(9, 23, 37, 51, 65, 79, 93, 107, 121, 135, 149, 161, 
                            172, 182, 191, 205, 219, 233, 247, 257, 266, 278, 
							289, 303, 317, 331, 344, 356, 365),
					  month=c(1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6, 7, 7, 
					          8, 8, 9, 9, 9, 10, 10, 10, 11, 11, 12, 12, 12))
# loop through the solar radiation days
etdat <- vector('list', length=nrow(doyconv))
for(i in seq(1, nrow(doyconv))){
  # get the month of this solar day
	themonth <- doyconv[i, 'month']
	# the solar day (corresponds to solardat)
	doy <- doyconv[i, 'doy']
	# the label to select the column
	radlabel <- paste('RADDOY', doy, sep='')
	# select nodes data for this month and merge solar data
	pdat <- merge(climatedat[climatedat[['period']]==themonth,], 
	                   solardat[c('deviceid', radlabel)], by='deviceid')
	# convert solar radiation from Wh/m2/day to MJ/m2/day
	pdat[radlabel] <- 0.0036*pdat[[radlabel]]
	# run the calculations
	pdat['hargreaves'] <- hargreaves(pdat[['temperature_min']], 
										  pdat[['temperature_max']], doy)
	pdat['copais'] <- hargreaves(pdat[['temperature_avg']], 
									  pdat[['humidity_avg']], 
									  pdat[[radlabel]])
	pdat['turc'] <- turc(pdat[['temperature_avg']], 
							  pdat[['humidity_avg']], 
							  pdat[[radlabel]])
	pdat['ivanov'] <- ivanov(pdat[['temperature_avg']], 
							  pdat[['humidity_avg']])
	etdat[[i]] <- pdat
	names(etdat)[i] <- radlabel
}

# plotting!
# load the DEM
dempath <- "C:/repositories/codeRepo/UCBcode-GIS/trunk/data/BORR/TABLE/BORR_DEM16ft_pointgrid.txt"
dem <- gridtoDF(dempath)
coordinates(dem) <- ~ projx + projy
# load the boundary
boundary <- readOGR(dsn="C:/repositories/codeRepo/UCBcode-GIS/trunk/data/BORR/FEATURE/BORR_bound_noroad", 
               layer="BORRboundnoroad")
imgtype <- '.pdf'
# start plotting
for(i in seq(along=etdat)){
	for(et in c('hargreaves', 'copais', 'turc', 'ivanov')){
		etpolys <- data_to_Thiessen(etdat[[i]], boundary)
		etplot <- plot_ET_polygons(etpolys, et, dem)
		ggsave(filename=paste('C:/repositories/codeRepo/UCBcode-R/trunk/', 
							  names(etdat)[i], '_', et, imgtype, sep=''),
			   plot=etplot, width=8, height=8, dpi=300)
	}
}