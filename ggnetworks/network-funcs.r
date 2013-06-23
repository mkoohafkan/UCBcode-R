# network functions

adjacency_by_function <- function(x, fun){
	# define the adjacency matrix of a network
	# x = a data frame of points. Each point contains properties
	#	there are no constraints on the fields of x
	#
	# fun = a function that returns either TRUE or FALSE given 
	#       two points from x
	#	if TRUE, nodes are connected by a segment
	#	if FALSE, nodes are not connected
	#
	# returns a boolean square matrix of length(x)
	
	# construct empty matrix
	len <- NROW(x)
	adj <- matrix(data=NA, nrow=len, ncol=len)
	# construct adjacency matrix
	for(i in seq(1, len, 1) ){
		for(j in seq(1, len, 1) ){
			# evaluate if i and j are connected according to fun 
			if(fun(x[i,], x[j,]) ){
				adj[i,j] <- TRUE
			} else{
				adj[i,j] <- FALSE
			}
		}
	}
	return(adj)
}

