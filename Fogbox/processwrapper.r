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

# process and analyze each run
for(i in seq(along=filelist)){
	print(paste('Processing ', filelist[i], '...', sep=''))
	datalist[[i]] <- analyze_fogrun(process_fogrun(filelist[i]))
	names(datalist)[i] <- datalist[[i]]$name
}
# make some plots
LWScurve(datalist)
dev.new()
runplot(datalist)