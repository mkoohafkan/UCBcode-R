# column 1 is date
# column 2 is Half Moon Bay road, 6am
# column 3 is Half Moon Bay road, 9am
# column 4 is Half Moon Bay road, 12pm
# column 5 is Half Moon Bay road, 3pm
# column 6 is Half Moon Bay road, 6pm
# column 7 is Upper Dam, 6am
# column 8 is Upper Dam, 9am
# column 9 is Upper Dam, 12pm
# column 10 is Upper Dam, 3pm
# column 11 is Upper Dam, 6pm
# column 12 is Cottage, 6am
# column 13 is Cottage, 9am
# column 14 is Cottage, 12pm
# column 15 is Cottage, 3pm
# column 16 is Cottage, 6pm
require(reshape2)
require(ggplot2)
path <- paste(getwd(), '/foggystuff', sep='')
setwd(path)
rawdat <- as.matrix(read.csv('master.csv', header=FALSE, sep=",", skip=2))
reclength <- nrow(rawdat)
# turn the data into something usable
otherdat <- as.data.frame(rbind(
	cbind(rawdat[, c(1, 2)], rep('hmbr', reclength), rep(6, reclength)),
	cbind(rawdat[, c(1, 3)], rep('hmbr', reclength), rep(9, reclength)),
	cbind(rawdat[, c(1, 4)], rep('hmbr', reclength), rep(12, reclength)),
	cbind(rawdat[, c(1, 5)], rep('hmbr', reclength), rep(15, reclength)),
	cbind(rawdat[, c(1, 6)], rep('hmbr', reclength), rep(18, reclength)),
	cbind(rawdat[, c(1, 7)], rep('ud', reclength), rep(6, reclength)),
	cbind(rawdat[, c(1, 8)], rep('ud', reclength), rep(9, reclength)),
	cbind(rawdat[, c(1, 9)], rep('ud', reclength), rep(12, reclength)),
	cbind(rawdat[, c(1, 10)], rep('ud', reclength), rep(15, reclength)),
	cbind(rawdat[, c(1, 11)], rep('ud', reclength), rep(18, reclength)),
	cbind(rawdat[, c(1, 12)], rep('cot', reclength), rep(6, reclength)),
	cbind(rawdat[, c(1, 13)], rep('cot', reclength), rep(9, reclength)),
	cbind(rawdat[, c(1, 14)], rep('cot', reclength), rep(12, reclength)),
	cbind(rawdat[, c(1, 15)], rep('cot', reclength), rep(15, reclength)),
	cbind(rawdat[, c(1, 16)], rep('cot', reclength), rep(18, reclength))),
	stringsAsFactors=FALSE)
names(otherdat) <- c('date', 'condition', 'location', 'hour')
# add the summer data you made earlier
summerdat <- read.csv('summer.csv', header=TRUE, stringsAsFactors=FALSE)
names(summerdat) <- c('date', 'hour', 'hmbr', 'ud', 'cot')
summerdat <- melt(summerdat, id.vars=c('date', 'hour'), 
                  measure.vars=c('hmbr', 'ud', 'cot'))
names(summerdat)[names(summerdat)=='variable'] <- 'location'
names(summerdat)[names(summerdat)=='value'] <- 'condition'
# combine to get all fog data in one dataframe
fogdat <- rbind(otherdat[, c('date', 'condition', 'location', 'hour')],
                summerdat[, c('date', 'condition', 'location', 'hour')])
# more cleanup
fogdat['date'] <- as.Date(fogdat[['date']], '%m/%d/%Y')
fogdat['hour'] <- factor(as.integer(fogdat[['hour']]), levels=c(6, 9, 12, 15, 18))
fogdat['location'] <- factor(fogdat[['location']])
fogdat['condition'] <- factor(fogdat[['condition']], levels=c('f', 'c', 'r'))
# some levels renaming
levels(fogdat[['condition']]) <- c('foggy', 'clear', 'rainy')
levels(fogdat[['location']]) <- c('cottage', 'half moon bay road', 'upper dam')
# some more date formatting
fogdat['day'] <- as.integer(format(fogdat[['date']], '%d'))
fogdat['month'] <- factor(format(fogdat[['date']], '%B'), levels=month.name)
fogdat['year'] <- factor(format(fogdat[['date']], '%Y'))
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
ggsave(filename='freqplotud.pdf', plot=relfreqplotud, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='freqplothmbr.pdf', plot=relfreqplothmbr, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='freqplotcot.pdf', plot=relfreqplotcot, dpi=300, height=15, 
       width=15, units='in')
# relative frequency by hour
relfreqhour <- function(dat, hour, am=TRUE){
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
ggsave(filename='relfreqplot6.pdf', plot=relfreqplot6, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='relfreqplot9.pdf', plot=relfreqplot9, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='relfreqplot12.pdf', plot=relfreqplot12, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='relfreqplot15.pdf', plot=relfreqplot15, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='relfreqplot18.pdf', plot=relfreqplot18, dpi=300, height=15, 
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
ggsave(filename='relfreqplot.pdf', plot=relfreqplot, dpi=300, height=15, 
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
ggsave(filename='freqplotfog.pdf', plot=fogfreqplot, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='freqplotrain.pdf', plot=rainfreqplot, dpi=300, height=15, 
       width=15, units='in')
ggsave(filename='freqplotclear.pdf', plot=clearfreqplot, dpi=300, height=15, 
       width=15, units='in')