# get covariance of nodes
require(igraph)
require(ggplot2)
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
rankings <- vector("list", length=length(periods))
fields <- c('nodeid', 'result_time', responsevar, 'pc')
# partition by yyyy-mm
for(q in seq(along=rankings)){
	# initialize some stuff
	covariance <- rep(NA, nrow(edges))
	samplesize <- covariance
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
			covariance[i] <- cov(idata[[responsevar]], jdata[[responsevar]])
			rm(query, thedf, inode, jnode, idata, jdata)
		}
	}
	# clean up from last time just in case
	edges$covariance <- NULL
	edges$samplesize <- NULL
	edges$rank <- NULL
	# update edges with covariance data
	edges['covariance'] <- covariance
	edges['samplesize'] <- samplesize
	edges['rank'] <- rank(covariance)
	edges['period'] <- periods[[q]]
	rankings[[q]] <- edges
}
names(rankings) <- periods

# save the data
save(rankings, file=paste(basetbl, '_', responsevar, 
						  '_covrankings.RData', sep=""))

# melt it all together for ggplotting
covdf <- do.call("rbind", rankings)

# ggplotting
baseplot <- ggplot(na.omit(covdf), aes(x=factor(period))) + 
			geom_point(aes(y=covariance, size=samplesize)) +
			facet_grid(from~to) + theme_bw() + 
			theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename=paste(basetbl, "_", responsevar, "_covplot.pdf", sep=""), 
	   plot=baseplot, width=100, height=100, dpi=300, 
	   limitsize=FALSE)
	   
# igraph plotting
graphs <- vector("list", length=length(rankings))
edgesegs <- graphs
for(i in seq(along=graphs)){
thedf <- rankings[[i]]
names(thedf)[]
	graphs[[i]] <- graph.data.frame(rankings[[i]], directed=FALSE, vertices=vertices)
	# for ggplotting
	edgesegs[[i]] <- get.data.frame(graphs[[i]], what="edges")
}
names(graphs) <- periods

