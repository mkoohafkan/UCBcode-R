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
	segdat <- data.frame(x=xs, y=ys, xend=xends, yend=yends)
	if(!is.null(weightcol)){
		segdat['weight'] <- edges[[weightcol]]
		segdat <- segdat[!(segdat$weight < threshold),]
	} else {
		segdat['weight'] <- 1
	}
	if(directional){
		require(grid)
		segs <- geom_segment(data=segdat, aes(x=x, y=y, xend=xend, yend=yend, 
											  color=weight), 
							 arrow=arrow(...))		
	} else {
		segs <- geom_segment(data=segdat, aes(x=x, y=y, xend=xend, yend=yend, 
										  color=weight))
	}
	return(segs)
}