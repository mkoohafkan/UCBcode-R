# R function to do network plots
require(igraph)
source("C:/repositories/codeRepo/R/trunk/borr-queries/extremes_queries.r")
source("C:/repositories/codeRepo/R/trunk/Borr-kriging/borr-dbfuncs.R")
source("C:/repositories/codeRepo/R/trunk/ggnetworks/network-funcs.R")


# get data from DB
fields <- c('nodeid', 'result_time', 'temperature_avg', 'pc')
query <- get_summer_ranks(fields[3], 'temphumidityhourly', 0.1, FALSE)
# returns data.frame
df <- datafromDB(query, fields, connectBORR())[[1]]

# define synchronization
# helper function
are_synchronized <- function(a, b, connectthresh){
	# SCRATCH FUNCTION FOR BORR
	# a, b are (ordered) vectors of timestamps
	# function computes if fraction of entries in a and b
	# greater than connectthresh are synchronized
	commons <- intersect(a, b)
	if(round(length(commons)/min(length(a), length(b)), 2) < connectthresh){ 
		return(FALSE)
	} else{
		return(TRUE)
	}
}

# define vertices
vertices <- unique(df['nodeid'])
### CAN'T DO THIS UNTIL WE UPDATE THE NODE LOCATIONS
# SQL query
#piece <- paste('nodeid =', nodes[1], sep=' ')
#for(i in seq(2, numnodes, 1)) piece <- paste(piece, 'OR nodeid =', nodes[i], sep=' ')
#vquery <- paste('SELECT nodeid, projx, projy, z FROM "node_properties" WHERE', 
#				piece, 'ORDER BY nodeid', sep=' ')
#vfields <- c('nodeid', 'projx', 'projy', 'z')
#vertices <- datafromDB(vquery, vfields, connectBORR())
###

# define edges
edges <- as.data.frame(cbind(t(combn(vertices[['nodeid']], 2))))
names(edges) <- c('from', 'to')

# helper function to calculate distances
calc_dists <- function(df1, df2){
	#df1, df2 are dataframes containing single row (the node)
	# fields passes must included projx, projy
	return(sqrt( (df1[['projx']] - df2[['projx']])^2 + (df1[['projy']] - df2[['projy']])^2 ) )
}

# initialization for getting synchronization levels
connectionthreshold <- rep(NA, nrow(edges))
samplesize <- connectionthreshold
edgedist <- connectionthreshold
# threshold definitions
synclevels <- seq(0, 1.0, 0.01) # percent synced levels

# get thresholds
for(i in seq(1, nrow(edges), 1)){
	# get data for specified nodes
	idata <- df[df[['nodeid']]==edges[['from']][i],][['result_time']]
	jdata <- df[df[['nodeid']]==edges[['to']][i],][['result_time']]
	# get the number of records to be compared
	samplesize[i] <- min(length(idata), length(jdata))
	# get the distance between nodes
	#edgedist[[i]] <- calc_dist(vertices[vertices[['nodeid']]==edges[['from']][i],],
	#						   vertices[vertices[['nodeid']]==edges[['to']][i],])
	# iterate through thresholds
	for(slev in synclevels){
		if(are_synchronized(idata, jdata, slev)){
			# take the highest threshold
			connectionthreshold[i] <- slev
		}
	}
}
# add threshold attribute to edges
edges['threshold'] <- connectionthreshold
edges['samplesize'] <- samplesize

# create igraph graph object
graphs <- vector("list", length=length(synclevels))
edgesegs <- graphs
#graphs[] <- graph.empty()
for(i in seq(along=graphs)){
	# take only edges with thresholds > threshold[i]
	graphs[[i]] <- graph.data.frame(edges[edges[['threshold']] >= synclevels[i],],
									directed=FALSE, vertices=vertices)
	# for ggplotting
	edgesegs[[i]] <- get.data.frame(graphs[[i]], what="edges")
}
names(graphs) <- synclevels
# to play
tkplot(graphs[["1"]])
somecliques <- maximal.cliques(graphs[["0.8"]])
someclosenessvals <- closeness(graphs[["0.9"]])
somedegreevals <-degree(graphs[["0.96"]])
somekcores <- graph.coreness(graphs[["0.95"]])
somedensity <- graph.density(graphs[["0.9"]])
nearestneighbor <- graph.knn(graphs[["0.85"]])
