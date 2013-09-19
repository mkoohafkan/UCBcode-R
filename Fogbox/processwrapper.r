# process_wrapper.r
# wrapper for process_fogrun() and analyze_fogrun()
source('process_fogrun.r')
source('analyze_fogrun.r')
source('plot_fogrun.r')

# path to preprocessed trial data files
setwd('./trialdata')
# folder 'trialdata' should only contain run information
filelist <- dir(getwd())
datalist <- vector('list', length=length(filelist))
flush.console()
# process and analyze each run
for(i in seq(along=filelist)){
	cat(paste('Processing ', filelist[i], '...\n', sep=''))
	flush.console()
	datalist[[i]] <- analyze_fogrun(process_fogrun(filelist[i]))
	names(datalist)[i] <- datalist[[i]]$name
}
# get data summary
fogsummary <- data.frame(name=sapply(datalist, function(x) x$name),
						 species=sapply(datalist, function(x) x$species),
						 trial=sapply(datalist, function(x) x$trial),
						 isvert=sapply(datalist, function(x) x$isvert),
						 isrep=sapply(datalist, function(x) x$isrep),
						 LWSwater.avg=sapply(datalist, function(x) x$LWSwatermass$avg),
						 LWSwater.stdev=sapply(datalist, function(x) x$LWSwatermass$stdev),
						 LWSvolt.max=sapply(datalist, function(x) x$maxLWSvolt),
						 LWSvolt.last=sapply(datalist, function(x) x$lastLWSvolt))
# make some plots
res <- LWScurve(fogsummary)
lwsmodel <- res[[1]]
lwsplot <- res[[2]]
rm(res)
print(lwsplot)
ps <- runplot(datalist)
dev.new()
print(ps[[1]])
dev.new()
print(ps[[2]])