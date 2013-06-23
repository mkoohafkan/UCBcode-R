# make_krige_layers (script)
# script REQUIRES
# partial file path as a command-line argument
# <partialpath>_rundata.Rdata <-- necessary variables, path to grid
# <Rgridpath> <-- R datafile containing grid
#
# script CREATES
# <partialpath>_Rvars.Rdata <-- kriging results, cross validation, etc.
# <partialpath>_krigeresults.mat <-- Matlab file containing kriging results
# <partialpath>_<krigingtype>variogram.png <-- variogram plots
#
# Author: Michael Koohafkan
###############################################################################

argsin <- commandArgs(TRUE) # command line filepath: expects partial filename
inoutpath <- argsin[1] # e.g. "C:\\temp_avgmin\\2011-6\\temp_avgmin_2011-6"

# packages
require(R.matlab)
require(lattice)
require(automap)
require(R.utils)

# redefine autofitVariogram to accept argument:cutoff
load(autofitVariogram)
reassignInPackage("autofitVariogram", pkgName="automap", afvmod)

# load the matlab data and base grid
writeLines("loading data into R...")
source(paste(inoutpath, "_rundata.Rdata", sep="") )
source(paste(Rgridpath, collapse="") ) # defined by <inpath>_rundata.Rdata

writeLines("initiating kriging procedure... ")
# convert matlab-exported sample_dat to SpatialPointDataFrames
# if temperature_avg(min), transform first
varname <- paste(varname, collapse="") 
dotrans =  varname == "temperature_avg" | varname == "temperature_avgmin"
if(dotrans){
	writeLines("Transforming data...")
#	kelvin = sample_dat[,4] + 273.15
#	maxtemp = max(kelvin)
#	trans = 1.0001*maxtemp - kelvin
	source(paste(inoutpath, "_mintempvals.Rdata", sep="") )
	trans <- sample_dat[,4] - sample_predictions
	sample_df <- data.frame(x=sample_dat[,1], y=sample_dat[,2], z=sample_dat[,3], dv=trans)
}else{
	sample_df <- data.frame(x=sample_dat[,1], y=sample_dat[,2], z=sample_dat[,3], dv=sample_dat[,4])
}
unsampled_df <- data.frame(x=unsampled_locs[,1], y=unsampled_locs[,2], z=unsampled_locs[,3])
coordinates(sample_df) <- ~x+y
coordinates(unsampled_df) <- ~x+y

# autokrige!
ok.krige <- autoKrige(dv~1, sample_df, unsampled_df, verbose=TRUE)
if(dotrans){
	#ked.krige <- autoKrige(log(dv)~z, sample_df, unsampled_df, verbose=TRUE)
	ked.krige <- autoKrige(dv~1, sample_df, unsampled_df, verbose=TRUE)
}else{
	ked.krige <- autoKrige(dv~z, sample_df, unsampled_df, verbose=TRUE)
}
writeLines("performing cross-validation...")
# cross validate
ok.cv <- krige.cv(dv~1, sample_df, model=ok.krige$var_model)
ked.cv <- krige.cv(dv~z, sample_df, model=ked.krige$var_model)

# compare cross-validations
cvcompare <- compare.cv(ok.cv, ked.cv)

# re-trend the data (where necessary)
if(dotrans){
	writeLines("Untransforming data...")
	#untrans <- 1.0001*maxtemp - ked.krige$krige_output$var1.pred
	untrans <- ked.krige$krige_output$var1.pred + unsampled_predictions
	ked.krige$krige_output$var1.pred <- untrans
	sample_df <- data.frame(x=sample_dat[,1], y=sample_dat[,2], z=sample_dat[,3], dv=sample_dat[,4])
}

# some data for future plotting
getrange <- function(thedat){
	themin <- min(thedat)
	themean <- mean(thedat)
	themax <- max(thedat)
	getrange <- c(themin, themean, themax)
}

# OK
okprange <- getrange(ok.krige$krige_output$var1.pred)
oksdrange <- getrange(ok.krige$krige_output$var1.stdev)
okvrange <- getrange(ok.krige$krige_output$var1.var)
# KED
kedprange <- getrange(ked.krige$krige_output$var1.pred)
kedsdrange <- getrange(ked.krige$krige_output$var1.stdev)
kedvrange <- getrange(ked.krige$krige_output$var1.var)

writeLines("writing data to .mat file...")
# write prediction to mat file 
writeMat(paste(inoutpath, "_krigeresults.mat", sep=""), 
		 okpredictionval=ok.krige$krige_output$var1.pred, 
		 kedpredictionval=ked.krige$krige_output$var1.pred, 
         okpredictionvar=ok.krige$krige_output$var1.var, 
         okpredictionstdev=ok.krige$krige_output$var1.stdev,
		 kedpredictionvar=ked.krige$krige_output$var1.var, 
		 kedpredictionstdev=ked.krige$krige_output$var1.stdev,
		 kedpredrange=kedprange, kedstdevrange=kedsdrange, kedvarrange=kedvrange,
		 okpredrange=okprange, okstdevrange=oksdrange, okvarrange=okvrange)
 
# write R variables to file
writeLines("saving workspace variables to file...")
save(inoutpath, sample_df, unsampled_df, ok.krige, ked.krige, ok.cv, ked.cv, cvcompare, 
	 varname, file=paste(inoutpath, "_Rvars.Rdata", sep="") )

# save variogram plots as images
writeLines("saving variogram plots")
myvariogramplot <- function(krigeobj){
	annotatedplot <- xyplot(gamma ~ dist, data = krigeobj, panel = automap:::autokrige.vgm.panel,
			labels = as.character(krigeobj$exp_var$np), shift = 0.03, model = krigeobj$var_model,
			direction = c(krigeobj$exp_var$dir.hor[1], krigeobj$exp_var$dir.ver[1]),
			ylim = c(min(0, 1.04 * min(krigeobj$exp_var$gamma)), 1.04 * max(krigeobj$exp_var$gamma)),
			xlim = c(0, 1.04 * max(krigeobj$exp_var$dist)), xlab = "Distance", ylab = "Semi-variance",
			main = "Experimental variogram and fitted variogram model", mode = "direct") 
	return(annotatedplot)
}
# OK variogram
png(paste(inoutpath, '_(OK)variogram.png', sep="") )
myvariogramplot(ok.krige)
garbage <- dev.off()
# KED variogram
png(paste(inoutpath, '_(KED)variogram.png', sep="") )
myvariogramplot(ked.krige)
garbage <- dev.off()
writeLines("Procedure completed.")