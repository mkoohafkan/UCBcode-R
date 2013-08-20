# _dataformat.r
# data formatting code in probanalysis.r
require(reshape2)
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
fogdat['monthnum'] <- as.integer(format(fogdat[['date']], '%m'))
# sort it all
fogdat <- fogdat[with(fogdat, order(date, hour, location)),]