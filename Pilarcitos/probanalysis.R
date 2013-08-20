# set the working directory
setwd('./Pilarcitos')

# get data
source('_datformat.r')

# plot
source('_freqplots.r')

# develop tables
tableprep <- function(dat){
	dat['locund'] <- paste(dat[['condition']], 'at', 
	                       gsub(" ","", dat[['location']]))
	table(dat[dat[['location']] == 'upper dam', 'locund'], 
	      dat[dat[['location']] == 'half moon bay road', 'locund'],
		  dat[dat[['location']] == 'cottage', 'locund'])
}
xtabprep <- function(dat){
	dat['isfoggy'] <- dat[['condition']] == 'foggy'
	dat['location'] <- gsub(" ","", dat[['location']]) 
	dat['fogat'] <- rep('not', nrow(dat))
	dat[dat[['isfoggy']], 'fogat'] <- dat[['location']][dat[['isfoggy']]]
	table(dat[dat[['location']] == 'upperdam', 'fogat'], 
	      dat[dat[['location']] == 'halfmoonbayroad', 'fogat'],
		  dat[dat[['location']] == 'cottage', 'fogat'])
}

for(y in unique(fogdat[['year']])){
	yeardat <- fogdat[fogdat[['year']] == y,]
	yeartable <- tableprep(yeardat)
	save(yeartable, yeardat, 
	     file=paste('dat/', y, '_freqtable.rdata', sep=''))
	for(m in unique(yeardat[['month']])){
		monthdat <- yeardat[yeardat[['month']] == m,]
		monthtable <- tableprep(monthdat)
		save(monthtable, monthdat, 
		     file=paste('dat/', y, '-', m, '_freqtable.rdata', sep=''))
		for(h in unique(monthdat[['hour']])){
			hourdat <- monthdat[monthdat[['hour']] == h,]
			hourtable <- tableprep(hourdat)
			save(hourtable, hourdat, 
			     file=paste('dat/', y, '-', m, '-', h, '_freqtable.rdata', sep=''))
		}
	}
	# do year tables for summer months only
	summeryeardat <- yeardat[yeardat[['monthnum']] > 5 & 
							 yeardat[['monthnum']] < 9,]
	summeryeartable <- tableprep(summeryeardat)
	save(summeryeartable, 
		     file=paste('dat/summer', '-', y, '_freqtable.rdata', sep=''))
	# do the hour tables for the summer months too
	for(h in unique(fogdat[, 'hour'])){
		summerhourdat <- summeryeardat[summeryeardat[['hour']] == h,]
		summerhourtable <- tableprep(summerhourdat)
		save(summerhourtable, 
		     file=paste('dat/summer', '-', y, '-', h, '_freqtable.rdata', sep=''))
	}
}
# and a table for summer all years combined
summerdat <- fogdat[fogdat[['monthnum']] > 5 & fogdat[['monthnum']] < 9,]
summertable <- tableprep(summerdat)
save(summerdat, summertable, file='dat/summer_freqtable.rdata')

# how to play
# ftable(summertable)
# ftable(prop.table(summertable, 1))
# ftable(prop.table(summertable, 2))
# ftable(prop.table(summertable, 3))

# test for location independence
summerfogtable <- xtabprep(summerdat)
# xtabs(as.formula('~cottage + halfmoonbayroad + upperdam'), data=fogonly)