# _freqplots.r
# plotting bit of probanalysis.r
require(ggplot2)
# plot
cbPalette <- scale_fill_discrete(drop=FALSE)
# relative frequency by location
relfreqloc <- function(dat, loc){
	ggplot(dat[dat[['location']]==loc,], aes(hour, fill=condition)) + 
	geom_bar(aes(y=..count../sapply(PANEL, FUN=function(x) sum(count[PANEL == x])/5)), 
			 width=0.5) + 
	facet_grid(year~month) + theme_bw() + xlab('location') + ylab('frequency') + 
	cbPalette + ggtitle(paste('conditions at', loc))
}
relfreqplotud <- relfreqloc(fogdat, 'upper dam')
relfreqplothmbr <- relfreqloc(fogdat, 'half moon bay road')
relfreqplotcot <- relfreqloc(fogdat, 'cottage')
ggsave(filename='img/freqplotud.pdf', plot=relfreqplotud, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='img/freqplothmbr.pdf', plot=relfreqplothmbr, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='img/freqplotcot.pdf', plot=relfreqplotcot, dpi=300, height=15, 
       width=15, units='in')
# relative frequency by hour
relfreqhour <- function(dat, hour, am=TRUE){
  fhour <- hour
  if(am){
		tod <- 'am'
	} else{
		tod <- 'pm'
		if(hour > 12) fhour <- hour - 12
	}
	ggplot(dat[dat[['hour']]==hour,], aes(location, fill=condition)) +
	geom_bar(aes(y=..count../sapply(PANEL, FUN=function(x) sum(count[PANEL == x])/3)), 
			 width=0.5) + 
	facet_grid(year~month) + theme_bw() + xlab('location') + ylab('frequency') + 
	theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) + cbPalette + 
	ggtitle(paste('conditions at ', fhour, tod, sep='')) 
}
relfreqplot6 <- relfreqhour(fogdat, 6)
relfreqplot9 <- relfreqhour(fogdat, 9)
relfreqplot12 <- relfreqhour(fogdat, 12, am=FALSE)
relfreqplot15 <- relfreqhour(fogdat, 15, am=FALSE)
relfreqplot18 <- relfreqhour(fogdat, 18, am=FALSE)
ggsave(filename='img/relfreqplot6.pdf', plot=relfreqplot6, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='img/relfreqplot9.pdf', plot=relfreqplot9, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='img/relfreqplot12.pdf', plot=relfreqplot12, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='img/relfreqplot15.pdf', plot=relfreqplot15, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='img/relfreqplot18.pdf', plot=relfreqplot18, dpi=300, height=15, 
       width=15, units='in')
# overall relative frequency
relfreqoverall <- function(dat){
	ggplot(fogdat, aes(location, fill=condition)) + 
	geom_bar(aes(y=..count../sapply(PANEL, FUN=function(x) sum(count[PANEL == x])/3)), 
	         width=0.5) +
	facet_grid(year~month) + xlab('location') + theme_bw() + 
	ylab('relative frequency') + 
	theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) + cbPalette + 
	ggtitle('relative frequency of weather conditions')
}
relfreqplot <- relfreqoverall(fogdat)
ggsave(filename='img/relfreqplot.pdf', plot=relfreqplot, dpi=300, height=15, 
       width=15, units='in')
# absolute frequency of weather conditions
absfreqcond <- function(dat, cond){
	ggplot(dat[dat[['condition']]==cond,], aes(location, fill=hour)) + 
	geom_bar(width=0.5) + facet_grid(year~month) + theme_bw() + 
	xlab('location') + ylab('frequency') +
	theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) + 
	cbPalette + ggtitle(paste('frequency of', cond, 'conditions')) + 
	scale_y_continuous(limits = c(0, 150))
}
fogfreqplot <- absfreqcond(fogdat, 'foggy')
rainfreqplot <- absfreqcond(fogdat, 'rainy')
clearfreqplot <- absfreqcond(fogdat, 'clear')
ggsave(filename='img/freqplotfog.pdf', plot=fogfreqplot, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='img/freqplotrain.pdf', plot=rainfreqplot, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='img/freqplotclear.pdf', plot=clearfreqplot, dpi=300, height=15, 
       width=15, units='in')
# all-day events
freqallday <- function(dat, cond){
  newdat <- NULL
  for(d in unique(dat$date)){
    m <- as.character(dat[dat$date==d, 'month'])[1]
    y <- as.character(dat[dat$date==d, 'year'][1])
    for(l in unique(dat$location)){
      if(all(dat[dat$date==d & dat$location==l, 'condition'] == cond)){
        newdat <- rbind(newdat, c(as.character(d), l, cond, y, m))
      }
    }
  }
  newdat <- data.frame(date=newdat[,1], location=factor(newdat[,2], 
                        levels=c('half moon bay road', 'upper dam', 'cottage')), 
                       condition=newdat[,3], year=factor(newdat[,4], levels=seq(1921, 1925)), 
                       month=factor(newdat[,5], levels=month.name))
  dayplot <- ggplot(newdat, aes(location)) + geom_bar() + facet_grid(year~month) +
             theme_bw() + ggtitle(paste('frequency of all-day', cond, 'conditions')) +
             xlab('location') + ylab('frequency') +  cbPalette +
             theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) 
}
foggyallday <- freqallday(fogdat, 'foggy')
ggsave(file='img/foggyalldayplot.pdf', plot=foggyallday, dpi=300, height=15, 
       width=15, units='in')
clearallday <- freqallday(fogdat, 'clear')
ggsave(file='img/clearalldayplot.pdf', plot=clearallday, dpi=300, height=15, 
       width=15, units='in')
rainyallday <- freqallday(fogdat, 'rainy')
ggsave(file='img/rainyalldayplot.pdf', plot=rainyallday, dpi=300, height=15, 
       width=15, units='in')