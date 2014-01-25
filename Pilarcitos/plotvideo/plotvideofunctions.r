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

theme_black=function(base_size=12,base_family="") {
require(grid)
  theme_grey(base_size=base_size,base_family=base_family) %+replace%
    theme(
      # Specify axis options
      axis.line=element_blank(), 
      axis.text.x=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,vjust=1), 
      axis.text.y=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,hjust=1), 
      axis.ticks=element_line(color="white",size = 0.2), 
      axis.title.x=element_text(size=base_size,color="white",vjust=1), 
      axis.title.y=element_text(size=base_size,color="white",angle=90,
                                vjust=0.5), 
      axis.ticks.length=unit(0.3,"lines"), 
      axis.ticks.margin=unit(0.5,"lines"),
      # Specify legend options
      legend.background=element_rect(color=NA,fill="black"), 
      legend.key=element_rect(color="white", fill="black"), 
      legend.key.size=unit(1.2,"lines"), 
      legend.key.height=NULL, 
      legend.key.width=NULL,     
      legend.text=element_text(size=base_size*0.8,color="white"), 
      legend.title=element_text(size=base_size*0.8,face="bold",hjust=0,
                                color="white"), 
      legend.position="right", 
      legend.text.align=NULL, 
      legend.title.align=NULL, 
      legend.direction="vertical", 
      legend.box=NULL,
      # Specify panel options
      panel.background=element_rect(fill="black",color = NA), 
      panel.border=element_rect(fill=NA,color="white"), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.margin=unit(0.25,"lines"),  
      # Specify facetting options
      strip.background=element_rect(fill="grey30",color="grey10"), 
      strip.text.x=element_text(size=base_size*0.8,color="white"), 
      strip.text.y=element_text(size=base_size*0.8,color="white",
                                angle=-90), 
      # Specify plot options
      plot.background=element_rect(color="black",fill="black"), 
      plot.title=element_text(size=base_size*1.2,color="white"), 
      plot.margin=unit(c(1,1,0.5,0.5),"lines")
    )
}