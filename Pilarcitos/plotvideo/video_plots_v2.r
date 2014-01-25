# videoplots v2

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
thedata[,1] <- as.POSIXct(thedata[,1], format = '%m/%d/%Y%t%H:%M', tz='GMT')

# get the period of data corresponding to the video 
tstart <- as.POSIXct("09/30/2013 11:00", format='%m/%d/%Y %H:%M', tz='GMT')
tend <- as.POSIXct("11/13/2013 19:01", format='%m/%d/%Y %H:%M', tz='GMT')
datslice <- slice_data(thedata, 'result_time', tstart, tend)

# lws plot
lwsplot <- ggplot(NULL, aes(x=result_time, y=lws.counts)) + theme_black() +
           scale_x_datetime(limits=c(min(datslice$result_time), 
		                                 max(datslice$result_time))) +
		   scale_y_continuous('leaf wetness counts',
		                      limits=c(min(datslice$lws.counts), 
		                               max(datslice$lws.counts))) +
		   theme(axis.title.x=element_blank())
for(i in seq(nrow(datslice))){
  thisplot <- lwsplot + geom_line(data=datslice[seq(i),], color='green')
  ggsave(sprintf('lws%d.tiff', i), plot=thisplot, width=9.6, height=1.84, dpi=200)
}


# get the plot segment for each timestep

# now for soil
soilmelt <- function(d){
  dnew <- d[c('result_time', 'surface.moisture', 'depth.moisture')]
  rd <- melt(dnew, id.vars=c('result_time'))
  names(rd) <- c('result_time', 'sensor_location', 'vwc')
  return(rd)
  }
soilplot <- ggplot(NULL, aes(x=result_time, y=vwc, color=sensor_location)) +
            scale_x_datetime() +
		    scale_y_continuous('volumetric water content') +
			scale_color_manual(values=c('red', 'yellow')) + theme_black() +
			theme(axis.title.x=element_blank(), legend.position="top", 
			      legend.direction = "horizontal")
thisplot <- soilplot + geom_line(data=soilmelt(thedata))
ggsave('soilmoisture.png', plot=thisplot, height=8.5, width=11, dpi=300)
			
			
# temperature
soilmelt <- function(d){
  dnew <- d[c('result_time', 'surface.temp', 'depth.temp')]
  rd <- melt(dnew, id.vars=c('result_time'))
  names(rd) <- c('result_time', 'sensor_location', 'temp')
  return(rd)
  }
soilplot <- ggplot(NULL, aes(x=result_time)) +
            scale_x_datetime(limits=c(min(datslice$result_time), 
		                                 max(datslice$result_time))) +
		    scale_y_continuous('temperature (deg.C)', 
			                   limits=c(min(min(datslice$surface.temp), 
							                min(datslice$surface.temp)), 
		                               max(max(datslice$surface.temp), 
							               max(datslice$surface.temp)))) +
			scale_color_manual(values=c('red', 'yellow')) + theme_black() +
			theme(axis.title.x=element_blank(), legend.title=element_blank())

for(i in seq(nrow(datslice))){	  
	thisplot <- soilplot + 
                geom_line(aes(y=surface.temp, color='surface'), data=datslice[seq(i),]) +
			    geom_line(aes(y=depth.temp, color='depth'), data=datslice[seq(i),]) 
    ggsave(sprintf('soiltemp%d.tiff', i), plot=thisplot, width=9.6, height=1.84, dpi=200)
}


			
thisplot <- soilplot + geom_line(data=soilmelt(datslice[1,]))
ggsave('soilmoisture1.tiff', plot=thisplot, width=9.6, height=1.84, dpi=200)
for(i in seq(2, nrow(datslice))){
  thisplot <- thisplot + geom_line(data=soilmelt(datslice[seq(i-1, i),]))
  ggsave(sprintf('soilmoisture%d.tiff', i), plot=thisplot, width=9.6, height=1.84, dpi=200)  
}





soilayers <- plot_sequence(datslice, soilmelt, geom_smooth, se=FALSE)

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
