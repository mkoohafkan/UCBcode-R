require(ggplot2)
require(RColorBrewer)
require(scales)
require(R.matlab)
require(deldir)
require(sp)
require(rgeos)
require(rgdal)

source("C:/repositories/codeRepo/UCBcode-R/trunk/Borr-kriging/thefuncs.R")

# function to create voronoi polygons
voronoipolygons <- function(x, poly) {
  if(missing(poly)) poly <- x
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  rw = as.numeric(t(bbox(poly)))
  z <- deldir(crds[,1], crds[,2],rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  ids <- x[['id']]
  cents <- x[['cent']]
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
}

# dem
# load the grid
dempath <- "C:/repositories/codeRepo/UCBcode-GIS/trunk/data/BORR/TABLE/BORR_DEM16ft_pointgrid.txt"
dem <- gridtoDF(dempath)
coordinates(dem) <- ~ projx + projy
# load the boundary
shp <- readOGR(dsn="C:/repositories/codeRepo/UCBcode-GIS/trunk/data/BORR/FEATURE/BORR_bound_noroad", 
               layer="BORRboundnoroad")

# get network data (from MATLAB)
# coords includes: Coords, NodesOut
coordspath <- "C:/repositories/codeRepo/UCBcode-matlab/trunk/Sally-networkStuff/new/CoordNodes.mat"
# metrics includes: CDj
metricspath <- paste("C:/repositories/codeRepo/UCBcode-matlab/trunk/Sally-networkStuff/new/", 
					 "Metrics_Iterated_Tmin_90.mat", sep="")
# load the data
coordsdat <- readMat(coordspath)
metricsdat <- readMat(metricspath)
nodes <- cleandata(data.frame(id=coordsdat$NodesOut[,], 
					projx=coordsdat$Coords[,1], 
					projy=coordsdat$Coords[,2],
					z=coordsdat$Coords[,3], 
					centrality=c(metricsdat$CDj[,1], NA, NA)))
nodes <- na.omit(nodes)
coordinates(nodes) <- ~ projx + projy
nodes <- remove.duplicates(nodes)
centrality <- data.frame(id=as.character(nodes[['id']]), 
                         value=nodes[['centrality']])

# make voronoi polygons
voronoinodes <- voronoipolygons(nodes, shp)
# clip to boundary
clipvn <- fortify(gIntersection(shp, voronoinodes, byid=TRUE))
# correct id messup (screw you gIntersection!)
clipvn$id <- substr(clipvn$id, start=3, stop=5)
# merge with centrality data for plotting
vn <- merge(centrality, clipvn, by=c('id'))
names(vn) <- c('nodeid', 'centrality', 'projx', 'projy', 'order', 'hole', 'piece', 'group')

# make the plot
# axis transformation
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
opts[[5]] <- scale_fill_gradient(low="blue", high="red")
opts[[6]] <- scale_size('elevation', range=c(01, 0.1))

voronoiplot <- ggplot(vn, aes(x=projx, y=projy) ) + 
			   geom_polygon(aes(fill=centrality, group=group)) + 
			   geom_contour(data=as.data.frame(dem), aes(z=z, size=..level..), 
			                binwidth=25, color='black') +
			   geom_text(data=as.data.frame(nodes), 
			             aes(x=projx, y=projy, label=id), 
						 size=2, color="white") + 
			   opts
ggsave(filename='voronoi.png', 
	   plot=voronoiplot, width=8, height=8, dpi=300)