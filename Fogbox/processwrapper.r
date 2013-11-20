# process_wrapper.r
# wrapper for process_fogrun(), analyze_fogrun(), fit_fogrun, plot_fogrun
source('process_fogrun.r')
source('analyze_fogrun.r')
source('fit_fogrun.r')
source('plot_fogrun.r')
flush.console()

# path to preprocessed trial data files
setwd('./trialdata')
# leaf area table
leaftable <- read.csv('leafareas.csv')

# folder 'trialdata' should only contain run information and leafarea table
filelist <- dir(getwd())
# ignore files that are not .txt files
for(i in rev(seq(along=filelist)))
  if(substr(filelist[i], nchar(filelist[i]) - 3, nchar(filelist[i])) != '.txt')
	filelist <- filelist[-i]  
# process and analyze each run
proclist <- vector('list', length=length(filelist))
rawlist <- proclist
for(i in seq(along=filelist)){
  cat(paste('Processing ', filelist[i], '...\n', sep=''))
  flush.console()
  # load and pre-process the data
  rawlist[[i]] <- process_fogrun(filelist[i])
  names(rawlist)[i] <- rawlist[[i]]$name
  # process the data
  proclist[[i]] <- analyze_fogrun(rawlist[[i]], leaftable)
  names(proclist)[i] <- proclist[[i]]$name
}
# kill any runs with missing leaf areas
for(i in rev(seq(along=proclist)))
  if(length(proclist[[i]]$leafarea) < 1)
    proclist[[i]] <- NULL
# perform fittings (adds 'summary' entry and additional fields to proclist)
datalist <- fit_fogrun(proclist)

# write the data to files
setwd('./data')
w <- getOption("width")
options(width=10000)
for(i in seq(along=datalist)){
  fname <- paste(names(datalist)[i], '.txt', sep='')
  sink(fname)
  print(datalist[[i]])
  sink()
}
setwd('./../')
options(width=w)
# wipe the files out
#for(i in seq(along=datalist)){
#  fname <- paste(datalist[[i]]$name, '.txt', sep='')
#  unlink(fname)
#}

# make some plots
setwd('./plots')
# LWS CURVE
lwsplot <- lwscurveplot(datalist$summary$data, datalist$summary$lwscurve)
# DEPRECATED
ps <- allrunsplot(datalist[names(datalist) != 'summary'])
fitratioplot <- ratioplot(datalist$summary$data)
speciesratioplot <- ratiomeans(datalist$summary$speciesrepavg, datalist$summary$repavg)
# RESULTS
sloperatiobox <- ratiobox(datalist$summary$repavg)
massratiobox <- bigratiobox(datalist[names(datalist) != 'summary'])
pointplots <- massplot(datalist[names(datalist) != 'summary'])
# DIAGNOSTICS
voltrange <- voltrangeplot(datalist$summary$data)
voltvsratio <- fitratiobyvolt(datalist$summary$data, datalist$summary$speciesavg)
vsmodels <- leafmodelvslwsmodel(datalist$summary$data)
leafvsvolt <- leafmodelvsvoltage(datalist$summary$data)
# INIDIVIDUAL TRIAL PLOTS
sdp <- surfacedensityplots(datalist[names(datalist) != 'summary'])
lsdp <- lwssurfdensplots(datalist[names(datalist) != 'summary'])
lwsmvp <- lwsmvplots(datalist[names(datalist) != 'summary'])
massratioplots <- massratios(datalist[names(datalist) != 'summary'])
# REDWOOD PLOTS FOR SALLY
redwoodlast <- redwoodcurve(datalist$summary$data)
allredwood <- allredwoodcurve(datalist)

# save some plots
ggsave('lwscurve.pdf', plot=lwsplot, width=11, height=8)
ggsave('fitratioplot.pdf', plot=fitratioplot, width=11, height=8)
ggsave('speciesratioplot.pdf', plot=speciesratioplot, width=11, height=8)
ggsave('sloperatiobox.pdf', plot=sloperatiobox, width=11, height=8)
ggsave('massratiobox.pdf', plot=massratiobox, width=11, height=8)
ggsave('voltrange.pdf', plot=voltrange, width=11, height=8)
ggsave('voltvsratio.pdf', plot=voltvsratio, width=11, height=8)
ggsave('vsmodels.pdf', plot=vsmodels, width=11, height=8)
ggsave('leafvsvolt.pdf', plot=leafvsvolt, width=11, height=8)
for(i in seq(along=sdp))
  ggsave(paste('surfdensplots/', datalist[[i]]$name , '_fits.pdf', sep=''), 
         plot=arrangeGrob(sdp[[i]], lsdp[[i]], ncol=1), width=11, height=8)
for(i in seq(along=lwsmvp))
  ggsave(paste('surfdensandlwsplots/', datalist[[i]]$name , '_surfdens_LWSmV.pdf', sep=''), 
         plot=arrangeGrob(sdp[[i]], lwsmvp[[i]], ncol=1), width=11, height=8)