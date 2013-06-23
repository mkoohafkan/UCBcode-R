# cross-correlation!
require(igraph)
source("C:/repositories/codeRepo/R/trunk/borr-queries/extremes_queries.r")
source("C:/repositories/codeRepo/R/trunk/Borr-kriging/borr-dbfuncs.R")
source("C:/repositories/codeRepo/R/trunk/ggnetworks/network-funcs.R")

responsevar <- 'temperature_avg'
basetbl <- 'temphumidityhourly'
dbconn <- connectBORR()

# get graph template
vertices <- getnodeproperties(dbconn, basetbl, 'nodeid')
edges <- as.data.frame(cbind(t(combn(vertices[['nodeid']], 2))))
names(edges) <- c('from', 'to')

# get list of query modifiers
# helper function for compactness
whereclause <- function(){
	# return unique yyyy-mm for yyyy > 2010 and 5 < mm < 9 
	mnthyrs <- datafromDB(paste("SELECT", 
								"date_trunc('month', result_time)::date::char(7)", 
								"AS mnthyr FROM", basetbl, 'WHERE', 
								"date_part('month', result_time) > 5 AND",
								"date_part('month', result_time) < 9 AND",
								"date_part('year', result_time) > 2010",
								'ORDER BY mnthyr', 
								sep=' '), 
						  c('mnthyr'), dbconn)[[1]]
	mnthyrs <- unique(mnthyrs[['mnthyr']])
	wherestring <- vector("list", length=length(mnthyrs))
	for(i in seq(along=wherestring)){
		wherestring[[i]] <- paste("WHERE date_trunc('month',",
								  "result_time)::date::char(7) =", 
								  paste("'", mnthyrs[[i]], "'", sep=''),
								  sep=' ')
	}
	return(list(wherestring, mnthyrs))
}
tmp <- whereclause()
periods <- tmp[[2]]
wheres <- tmp[[1]]
rm(tmp) # clean up 

# check covariances
correlations <- vector("list", length=length(periods))
fields <- c('nodeid', 'result_time', responsevar, 'pc')
# partition by yyyy-mm
for(q in seq(along=correlations)){
	# initialize some stuff
	crosscorr <- rep(NA, nrow(edges))
	samplesize <- crosscorr
	bestlag <- crosscorr
	# for each pair of nodes
	for(i in seq(1, nrow(edges), 1)){
		inode <- edges[['from']][i]
		jnode <- edges[['to']][i]
		# go ahead and use sync_query, ignore pc column
		query <- sync_query(inode, jnode, basetbl, responsevar, wheres[[q]])
		thedf <- datafromDB(query, fields, dbconn)[[1]]
		if(!is.null(thedf)){ # preserve NAs if nodes don't overlap
			# query should order data
			idata <- thedf[thedf[['nodeid']]==inode,]
			jdata <- thedf[thedf[['nodeid']]==jnode,]
			samplesize[i] <- nrow(idata)
			if(samplesize[i] < 2){
				warning('sample size too small. cross-correlation not computed.')
			} else{
				# cross-correlation
				its <- as.ts(idata[[responsevar]])
				jts <- as.ts(jdata[[responsevar]])
				ccor <- ccf(its, jts)
				crosscorr[i] <- ccor$acf[abs(ccor$acf)==max(abs(ccor$acf))]
				bestlag[i] <-  ccor$lag[abs(ccor$acf)==max(abs(ccor$acf))]
				# clean up
				rm(its, jts, ccor)
			}
		rm(query, thedf, inode, jnode, idata, jdata)
		}
	}
	# clean up from last time just in case
	edges$crosscorrelation <- NULL
	edges$samplesize <- NULL
	edges$lag <- NULL
	# update edges with covariance data
	edges['crosscorrelation'] <- crosscorr
	edges['lag'] <- bestlag
	edges['samplesize'] <- samplesize
	edges['period'] <- periods[[q]]
	correlations[[q]] <- edges
}
names(correlations) <- periods

# save the data
save(correlations, file=paste(basetbl, '_', responsevar, 
						      '_crosscor.RData', sep=""))