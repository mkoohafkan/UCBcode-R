adjacency_to_df <- function(adjmat){
	# takes the matrix adjmat and converts it to the data.frame adjframe
	# names(adjframe) = c('from', 'to', 'value')
	adjframe <- expand.grid(from=seq(1, nrow(adjmat)), to=seq(1, ncol(adjmat)))
	adjframe['value'] <- expand.grid(adjmat)
	return(adjframe)
}

graph_to_dataframe <- function(graphobj){
	# split graphobj into vertices and edges
	vertices <- get.data.frame(graphobj, what="vertices")
	edges <- get.data.frame(graphobj, what="edges")
	return(list(edges=edges, vertices=vertices))
}

fortify_edges <- function(edges, vertices){
	xs <- rep(NA, nrow(edges))
	ys <- xs
	xends <- xs
	yends <- xs
	for(i in seq(along=xs)){
		xs[i] <- vertices[['x']][vertices[['id']]==edges[['from']][i] ]
		ys[i] <- vertices[['y']][vertices[['id']]==edges[['from']][i] ]
		xends[i] <- vertices[['x']][vertices[['id']]==edges[['to']][i] ]
		yends[i] <- vertices[['y']][vertices[['id']]==edges[['to']][i] ]
	}
		return(data.frame(edges, x=xs, y=ys, xend=xends, yend=yends))
}

plot_graph <- function(vertices, edges, directional=FALSE, weightcol=NULL, threshold=0, ...){
# vertices = vertices with location information
#   names(vertices) = c('id', 'x', 'y')
# edges = edge dataframe
#   names(edges) = c('from', 'to', ...)
#  weightcol = name of column (if any) to use for edge weights
# threshold = minimum edge weight required to be plotted. 
#			  Ignored if weightcol = NULL
# ... = optional parameters for arrow(). 
#       ignored if directional = FALSE
	
	# get coordinates of edge start and end points
	segdat <- data.frame(x=xs, y=ys, xend=xends, yend=yends)
	if(!is.null(weightcol)){
		segdat['weight'] <- edges[[weightcol]]
		segdat <- segdat[!(segdat$weight < threshold),]
	} else {
		segdat['weight'] <- 1
	}
	segdat['weight2'] <- edges[['sync']]
	if(directional){
		require(grid)
		segs <- geom_segment(data=segdat, aes(x=x, y=y, xend=xend, yend=yend, 
											  color=weight), 
							 arrow=arrow(...))		
	} else {
		segs <- geom_segment(data=segdat, aes(x=x, y=y, xend=xend, yend=yend, 
										  color=weight, size=sync))
	}
	return(segs)
}