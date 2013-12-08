## CUSTOM FUNCTION
source("C:\repositories\codeRepo\UCBcode-R\trunk\Pilarcitos\plotvideo\plotvideofunctions.r")
melt_data <- function(dat){
  # melt the data
  # group = 'surface', 'deep', 'lws'
  meltdat <- melt(dat, meas.vars=c('sm1', 'sm2', 'lws'), id.vars='result_time')
  # names(meltdat) <- c('result_time, 'variable', 'value')
  # add tags
  meltdat['sensortype'] <- 'soil moisture'
  meltdat[meltdat$variable == 'lws', 'sensortype'] <- 'leaf wetness'
  meltdat['group'] <- 'lws'
  meltdat[meltdat$variable=='sm1', 'group'] <- 'surface'
  meltdat[meltdat$variable=='sm2', 'group'] <- 'depth'
  return(meltdat)
}

# get the data
thedata <- read.csv('testdat.csv')
# make POSIX
thedata[,1] <- as.POSIXct(thedata[,1],format = '%m/%d/%Y%t%H:%M:%S')
# not run: data slicing example 
#tstart <- '12/30/2013 14:36:00'
#tend <- '01/15/2013 08:51:00'
#datslice <- slice_data(thedata, 'result_time', tstart, tend)

# define the baseplot
baseplot <- ggplot(NULL, aes(x=result_time, y=value, color=group)) + 
            facet_wrap(~ sensortype, ncol=2, scale='free_y') + 
			set_facetlims(thedata, melt_data) + scale_x_datetime()

# get the plot segment for each timestep
plotlayers <- plot_sequence(thedata, melt_data, geom_line)

# get a vertical line at each timestep
vertlayers <- vector('list', length=length(plotlayers))
for(i in seq(along=vertlayers))
  vertlayers[[i]] <- geom_vline(xintercept=as.numeric(thedata[i, 'result_time']))

# combine the layers for adding to baseplot
newlayers <- reshape_layers(plotlayers, vertlayers)

# save the plots
## CRITICAL: use ratio of movie frame size as plot size
## otherwise Matlab write-to-video will break!
#save_plotsequence(baseplot, newlayers, 'pilarcitos', width=4.8, height=2.4)
#save_plotsequence(baseplot, newlayers, 'pilarcitos', width=19.2, height=3.68, dpi=100)
save_plotsequence(baseplot, newlayers, 'pilarcitos', width=9.6, height=1.84, dpi=200)
## try to get 368 x 1280 or 368 x 1920 
