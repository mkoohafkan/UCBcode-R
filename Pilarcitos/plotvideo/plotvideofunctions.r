## functions for producing plot videos
require(ggplot2)
require(reshape2)

# get a slice of a dataset based on named column, start value, end value
# assumes numeric values of named column
slice_data <- function(dat, colname, startval, endval){
  datwindow <- dat[[colname]] >= startval & dat[[colname]] <= endval
  return(dat[datwindow,])
}

# sets axis limits on facets
# assumes user wants mins and maxes of data
# can override with custom function
# user supplies custom function for melting data for plotting
# melting procedure must make data workable with baseplot
set_facetlims <- function(dat, meltfun){
  blankdata <- as.data.frame(matrix(rep(0, 2*ncol(dat)), ncol=ncol(dat)))  
  names(blankdata) <- names(dat)
  for(n in names(blankdata))
    blankdata[n] <- c(min(dat[[n]]), max(dat[[n]]))
  return(geom_blank(data=meltfun(blankdata)))
}

# combines lists of layers (e.g. generated from plot_sequence) 
# for adding to a ggplot via save_plotsequence()
reshape_layers <- function(...){
  layerlist <- list(...)
  numlayers <- length(layerlist)
  sequencelength <- length(layerlist[[1]])
  # confirm that lists are of matching lengths
  for(i in seq(2, numlayers))
    if(length(layerlist[[i]]) != sequencelength)
	  stop('supplied layers are not of equal lengths. Check input list lengths.')
  # create new container
  newlist <- vector('list', length=sequencelength)
  for(i in seq(sequencelength)){
    newlist[[i]] <- vector('list', length=numlayers)
	for(n in seq(numlayers))
	  newlist[[i]][[n]] <- layerlist[[n]][[i]]
  }
  return(newlist)
}

# get a sequence of layers for a timelapse plot
# expects a baseplot to already be defined
# user supplies custom function for melting data for plotting
# melting procedure must make data workable with baseplot
plot_sequence <- function(dat, melt_fun, gg_fun, ...){
  # gg_fun is the ggplot function to use
  # ... is other arguments to pass to gg_fun
  layers <- vector('list', length=nrow(dat))
  for(i in seq(along=layers))
    layers[[i]] <- gg_fun(data=melt_fun(dat[seq(i),]), ...)
  return(layers)
} 

# save sequence of plots
save_plotsequence <- function(p, l, plotsuffix='', plotext='tiff', ...){
  # p is the starting plot and all options
  # l is a list of individual layers, e.g. geom_point()
  for(i in seq(along=l)){
    thisplot <- p + l[[i]]
	imagename <- paste(plotsuffix, i, '.', plotext, sep='')
    ggsave(imagename, plot=thisplot, ...)
  }
  return(0)
}