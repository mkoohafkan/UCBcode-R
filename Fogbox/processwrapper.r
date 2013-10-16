# process_wrapper.r
# wrapper for process_fogrun(), analyze_fogrun(), fit_fogrun, plot_fogrun
source('process_fogrun.r')
source('analyze_fogrun.r')
source('fit_fogrun.r')
source('plot_fogrun.r')
flush.console()

# path to preprocessed trial data files
setwd('./trialdata')
# folder 'trialdata' should only contain run information
filelist <- dir(getwd())
# ignore files that are not .txt files
for(i in rev(seq(along=filelist)))
  if(substr(filelist[i], nchar(filelist[i]) - 3, nchar(filelist[i])) != '.txt')
	filelist <- filelist[-i]  
# process and analyze each run
datalist <- vector('list', length=length(filelist))
for(i in seq(along=filelist)){
  cat(paste('Processing ', filelist[i], '...\n', sep=''))
  flush.console()
  datalist[[i]] <- analyze_fogrun(process_fogrun(filelist[i]))
  names(datalist)[i] <- datalist[[i]]$name
}
# perform fittings (adds 'summary' entry to datalist)
datalist <- fit_fogrun(datalist)

# make some plots
setwd('./plots')
lwsplot <- lwscurveplot(datalist$summary$data, datalist$summary$lwscurve)
ps <- allrunsplot(datalist[names(datalist) != 'summary'])
fitratioplot <- ratioplot(datalist$summary$data)
speciesratioplot <- ratiomeans(datalist$summary$repavg, datalist$summary$speciesavg)
voltrange <- voltrangeplot(datalist$summary$data)
voltvsratio <- fitratiobyvolt(datalist$summary$data, datalist$summary$speciesavg)
sdp <- surfacedensityplots(datalist[names(datalist) != 'summary'])
lsdp <- lwssurfdensplots(datalist[names(datalist) != 'summary'])
lwsmvp <- lwsmvplots(datalist[names(datalist) != 'summary'])
vsmodels <- leafmodelvslwsmodel(datalist$summary$data)
leafvsvolt <- leafmodelvsvoltage(datalist$summary$data)
redwoodlast <- redwoodcurve(datalist$summary$data)
allredwood <- allredwoodcurve(datalist)

# save some plots
ggsave('lwscurve.pdf', plot=lwsplot, width=11, height=8)
ggsave('fitratioplot.pdf', plot=fitratioplot, width=11, height=8)
for(i in seq(along=sdp))
  ggsave(paste('surfdensplots/', datalist[[i]]$name , '_fits.pdf', sep=''), 
         plot=arrangeGrob(sdp[[i]], lsdp[[i]], ncol=1), width=11, height=8)
for(i in seq(along=lwsmvp))
  ggsave(paste('surfdensandlwsplots/', datalist[[i]]$name , '_surfdens_LWSmV.pdf', sep=''), 
         plot=arrangeGrob(sdp[[i]], lwsmvp[[i]], ncol=1), width=11, height=8)
