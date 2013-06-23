source("C:/repositories/codeRepo/R/trunk/borr-kriging/more-plot-funcs.r")

responsevars <- c('temperature_avg', 'temperature_avgmin', 'temperature_avgmax',
				  'humidity_avg', 'humidity_avgmin', 'humidity_avgmax')

# get the data
query <- paste('select basetable.nodeid, result_time::char(7) AS result_time,', 
			   responsevar, 'from ', basetbl,
	           'left outer join node_properties on (',
			   paste(basetbl, '.nodeid', sep=""), '= node_properties.nodeid)',
			   'order by result_time, nodeid', sep=' ')
fields <- c('nodeid', 'result_time', 'projx', 'projy', 'z', responsevars)
conn <- connectBORR()
mnthlydat <- datafromDB(query, fields, conn) 

# set up
periods <- unique(mnthlydat[['period']])

# expects names(solardat) = c('date', nodeid, 'radiation)' 
solardat <- read.csv() 

for(i in seq(along=periods)){
		thisdat <- mnthlydat[mnthlydat[['result_time']]==periods[i],]
		thissolar < solardat[substr(solardat[['date']], 1, 7)==periods[i],]
		# trim solar data to nodes in record
		thissolar <- thissolar[thisdat[['nodeid']] %IN% thissolar[['nodeid']],]
		# join solar radiation to dataframe
		thisdat <- thisdat[with(thisdat, order(nodeid)), ]
		thissolar <- thissolar[with(thissolar, order(nodeid)), ]
		thisdat['radiation'] <- thissolar[['radiation']]
		
		
		rm(thissolar, thisdat)
	}
}