#######################################################################
# name: borr-dbfuncs.r
# author: Michael Koohafkan
# reverse-depends: pointspredict.r
# purpose: function definitions for plotting Thiessen maps of 
# ET predictions (see pointspredict.r)
#######################################################################
#
# TODO:

require(ggplot2)
require(RColorBrewer)
require(deldir)
require(sp)
require(rgeos)
require(rgdal)

# function to create voronoi polygons
voronoipolygons <- function(x, poly, xcoords=~projx+projy) {
  # x is the point locations dataframe
  # names(x) = c('id', 'projx', 'projy')
  # poly is a clip boundary (assumed to match projection of x)
  if(missing(poly)) poly <- x
  if (.hasSlot(x, 'coords')) { # x is already class Spatial
    crds <- x@coords  
  } else if (!is.null(xcoords)){ # coordinates formula is provided
    coordinates(x) <- xcoords
    crds <- x@coords
  } else { #  assume names(x) = c('id', 'projx', 'projy'), same projection as poly
    crds <- x[, c(2,3)]
  }
  rw = as.numeric(t(bbox(poly)))
  z <- deldir(crds[,1], crds[,2], rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  ids <- x[['id']]
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(ids[i]))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, 
				data=data.frame(x=crds[,1],y=crds[,2], 
					row.names=sapply(slot(SP, 'polygons'), 
					function(x) slot(x, 'ID'))))
	return(voronoi)
}

data_to_Thiessen <- function (pointdat, boundary, idfield='deviceid', 
                      locfields=c('projx', 'projy'), coords=~projx+projy){
	# make voronoi polygons
	# split pointdat into location data and other data
	otherdat <- pointdat[, !(names(pointdat) %in% locfields)]
	locdat <- pointdat[, c(idfield, locfields)]
	# avoid id snafu with voronoi polygons
	names(locdat)[1] <- 'id'
	names(otherdat)[1] <- 'id'
	voronoinodes <- voronoipolygons(locdat, boundary, coords)
	# clip to boundary
	clipvn <- fortify(gIntersection(boundary, voronoinodes, byid=TRUE))
	# correct id messup (screw you gIntersection!)
	clipvn$id <- substr(clipvn$id, start=3, stop=5)
	# merge with centrality data for plotting
	vn <- merge(otherdat, clipvn, by=c('id'))
	names(vn) <- c(names(otherdat), locfields, 'order', 'hole', 'piece', 'group')
	names(vn)[1] <- idfield
	return(vn)
}

plot_ET_polygons <- function(polydat, ETtype, dem){
	kmscale <- function(offset=0){
		function(x) format((x - offset)*0.3048/1000, digits=1) 
	}
	miny <- min(as.data.frame(dem)$projy)
	minx <- min(as.data.frame(dem)$projx)

	opts <- vector("list", length=6)
	opts[[1]] <- coord_fixed()
	opts[[2]] <- theme_bw()
	opts[[3]] <- scale_y_continuous(name="y (km)", labels=kmscale(offset=miny))
	opts[[4]] <- scale_x_continuous(name="x (km)", labels=kmscale(offset=minx))
	opts[[5]] <- scale_fill_gradient(low="blue", mid="yellow", high="red")
	opts[[6]] <- scale_size('elevation', range=c(01, 0.1))

	voronoiplot <- ggplot(polydat, aes(x=projx, y=projy) ) + 
				   geom_polygon(aes_string(fill=ETtype, group='group')) + 
				   geom_contour(data=as.data.frame(dem), aes(z=z, size=..level..), 
								binwidth=25, color='black') + opts
	return(voronoiplot)
}