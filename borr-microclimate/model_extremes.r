# fit models to extreme temperatures
require(segmented)
require(glmulti)
setwd('C:/repositories/codeRepo/UCBcode-R/trunk/')
source('borr-queries/extremes_queries.r')
source('Borr-kriging/borr-dbfuncs.r')

# setup
conn <- connectBORR()
srctable <- 'temphumidityhourly'
orderdesc <- TRUE 
rv <- 'temperature_avg'
pvs <- c('z', 'slope', 'aspect', 'canopyheight', 'projx', 'projy') #cad
# nodeid, result_time, pc come from get_summer_ranks()
fields <- c('nodeid', 'result_time', rv, 'pc', pvs)

# get extremes data
query <- paste('WITH tmp AS (', get_summer_ranks(rv, srctable, 0.05, orderdesc), '),',
			   'props AS (', get_nodeprops(c('nodeid', pvs)), ')',
			   'SELECT tmp.*,', paste('props.', pvs, sep='', collapse=', '),
			   'FROM tmp, props WHERE tmp.nodeid = props.nodeid',
			   sep=' ')
extremes <- datafromDB(query, fields, conn)[[1]]
names(extremes) <- fields

get_subsets <- function(df, nodeprops){
	# select subsets of data for model selection
	# monte carlo method
	# returns a list of subset frames
	nodes <- unique(df$nodeid)
	numsubsets <- 100
	dfs <- vector("list", length=numsubsets)
	for(i in seq(along=dfs)){
		# generate numsubset number of dataframes of extreme values
		# measured at each node
		sampledf <- NULL
		for(n in seq(along=nodes)){
			# get all entries for node n
			subdf <- df[df['nodeid']==nodes[n],]
			# random sampling, choose 1 row and add to sampledf
			sampledf <- rbind(sampledf, subdf[sample(nrow(subdf), 1), ])
			rm(subdf)
		}
		# results in sampledf containing one entry for each node
		dfs[[i]] <- sampledf
		rm(sampledf)
	}
	return(dfs)
}
extremesets <- get_subsets(extremes)

# helper function to get breakpoint for z-only models
get_bp_term <- function(dframe, rvar, pvar){
	psi <- mean(dframe[[pvar]])
	# linfit first
	linfitfrm <- as.formula(paste(rvar, '~', pvar))
	linfit <- lm(linfitfrm, dframe)
	# segmented fit
	segfit <- segmented.lm(linfit, seg.Z=as.formula(paste("~", pvar)), psi=psi)
	bp <- segfit$psi[2]
	return(paste('I(pmax(z -', bp, ', 0))'))
}

# use glmulti to fit models
bestmodels <- vector('list', length=length(extremesets))
bpterms <- bestmodels
exc <- c('projx', 'projy') # excluded terms
# get models
for(s in seq(along=extremesets)){
# get breakpoint that may represent marine layer height
	bpterms[[s]] <- get_bp_term(extremesets[[s]], rv, 'z')
	# add this bp term to predictor variables and get models
	models <- glmulti(rv, pvs, extremesets[[s]], exclude=exc,
					  level=2, method='g', crit='aicc')
}