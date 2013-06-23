# get synchronization of nodes
require(igraph)
source("C:/repositories/codeRepo/R/trunk/borr-queries/extremes_queries.r")
source("C:/repositories/codeRepo/R/trunk/Borr-kriging/borr-dbfuncs.R")
source("C:/repositories/codeRepo/R/trunk/ggnetworks/network-funcs.R")

responsevar <- 'temperature_avgmax'
basetbl <- 'temphumiditydaily'
dbconn <- connectBORR()

# get graph template
vertices <- getnodeproperties(dbconn, basetbl, 'nodeid')
edges <- as.data.frame(cbind(t(combn(vertices[['nodeid']], 2))))
names(edges) <- c('from', 'to')
syncrate <- rep(NA, nrow(edges))
samplesize <- syncrate

# helper function for syncing
howsynced <- function(df1, df2){
	return(sum(df1[['pc']] == df2[['pc']])/nrow(df1))
}

# check synchronization
fields <- c('nodeid', 'result_time', responsevar, 'pc')
for(i in seq(1, nrow(edges), 1)){
	inode <- edges[['from']][i]
	jnode <- edges[['to']][i]
	query <- sync_query(inode, jnode, basetbl, responsevar)
	thedf <- datafromDB(query, fields, dbconn)[[1]]
	if(!is.null(thedf)){
		# query should order data
		idata <- thedf[thedf[['nodeid']]==inode,]
		jdata <- thedf[thedf[['nodeid']]==jnode,]
		samplesize[i] <- nrow(idata)
		syncrate[i] <- howsynced(idata, jdata)
		rm(query, thedf, inode, jnode, idata, jdata)
	}
}
edges['syncrate'] <- syncrate
edges['samplesize'] <- samplesize