## CUSTOM FUNCTION
source("C:/repositories/codeRepo/UCBcode-R/trunk/Pilarcitos/plotvideo/plotvideofunctions.r")
melt_data <- function(dat){
  # melt the data
  # group = 'surface', 'deep', 'lws'
  meltdat <- melt(dat, measure.vars=c('surface.moisture', 'depth.moisture', 'lws.counts'), 
			     id.vars=c('result_time'))
  # names(meltdat) <- c('result_time, 'variable', 'value')
  # add tags
  meltdat['sensortype'] <- 'soil moisture (VWC)'
  meltdat[grep('lws.', meltdat$variable), 'sensortype'] <- 'leaf wetness (counts)'
  meltdat['group'] <- 'surface'
  meltdat[grep('depth.', meltdat$variable), 'group'] <- 'depth'
  return(meltdat)
}

# get the data
thedata <- read.csv("SP-SM-1 21Nov13-1257.csv", skip=3, stringsAsFactors=FALSE)
names(thedata) <- c('result_time', 'surface.moisture', 'surface.temp', 
                    'depth.moisture', 'depth.temp', 
			  'lws.minuteswet.450', 'lws.minuteswet.460', 
			  'lws.counts')
# make POSIX
thedata[,1] <- as.POSIXct(thedata[,1], format = '%m/%d/%Y%t%H:%M')

# get the period of data corresponding to the video 
tstart <- as.POSIXct("09/23/2013 14:29", format='%m/%d/%Y %H:%M')
tend <- as.POSIXct("11/16/2013 00:00", format='%m/%d/%Y %H:%M')
datslice <- slice_data(thedata, 'result_time', tstart, tend)

# define the baseplot
baseplot <- ggplot(NULL, aes(x=result_time, y=value, color=group)) + 
            facet_wrap(~ sensortype, ncol=2, scale='free_y') + 
			#set_facetlims(thedata, melt_data) + scale_x_datetime('') +
			theme_black() #+
			#scale_y_continuous('sensor readout') +
			#scale_color_gradient(name='sensor location') 
			
### maybe this way?
plot_linesequence <- function(dat, melt_fun, ...){
  # gg_fun is the ggplot function to use
  # ... is other arguments to pass to gg_fun
  layers <- vector('list', length=nrow(dat))
  layers[[1]]
  for(i in seq(2, length(layers)))
    layers[[i]] <- geom_line(data=melt_fun(dat[i-1:i,]), ...)
  return(layers)
} 
plotlayers <- plot_linesequence(thedata[1:200,], melt_data)

# get the plot segment for each timestep
plotlayers <- plot_sequence(thedata, melt_data, geom_line)

# get a vertical line at each timestep
vertlayers <- vector('list', length=length(plotlayers))
for(i in seq(along=vertlayers))
  vertlayers[[i]] <- geom_vline(xintercept=as.numeric(thedata[i, 'result_time']), color='white')

# combine the layers for adding to baseplot
newlayers <- reshape_layers(plotlayers, vertlayers)

# save the plots
## CRITICAL: use ratio of movie frame size as plot size
## otherwise Matlab write-to-video will break!
#save_plotsequence(baseplot, newlayers, 'pilarcitos', width=4.8, height=2.4)
#save_plotsequence(baseplot, newlayers, 'pilarcitos', width=19.2, height=3.68, dpi=100)
save_plotsequence(baseplot, newlayers, 'pilarcitos', width=9.6, height=1.84, dpi=200)
## try to get 368 x 1280 or 368 x 1920 
