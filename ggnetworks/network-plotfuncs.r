connect_adjacent <- function(locs, adjacency, threshold=0, directional=FALSE, ...){
	# takes the adjacency matrix and the node coordinates to plot line segments
	# adjacency = square floating point matrix of length(locs) 
	# 	          element values are strength of connection
	# locs = data.frame(id, x, y)
	# threshold = 
	# ... are arguments for arrow() function
	require(grid)
	require(ggplot2)
	
	xs <- rep(NA, sum(adjacency))
	ys <- xs
	xends <- xs
	yends <- xs
	strengths <- xs
	segs<- vector("list", sum(adjacency) )
	ind <- 0	# loop index
	for(i in seq(1, dim(adjacency)[1], 1)){
		for(j in seq(1, dim(adjacency)[2], 1)){
			if(adjacency[i, j] > threshold){
				ind <- ind + 1
				xs[[ind]] <- locs[['projx']][i]
				ys[[ind]] <- locs[['projy']][i]
				xends[[ind]] <- locs[['projx']][j]
				yends[[ind]] <- locs[['projy']][j]
				strengths[[ind]] <- adjacency[i, j]
			}
		}
	}
	segdat <- data.frame(x=xs, y=ys, xend=xends, yend=yends, strength=strengths)
	if(directional){
		segs <- geom_segment(data=segdat, aes(x=x, y=y, 
		                                      xend=xend, yend=yend, 
							 				  size=factor(strength)), 
							 arrow=arrow(...))		
	} else {
		segs <- geom_segment(data=segdat, aes(x=x, y=y, 
		                                      xend=xend, yend=yend, 
											  size=factor(strength)))
	}
	return(segs)
}