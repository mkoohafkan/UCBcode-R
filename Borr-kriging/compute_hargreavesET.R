# compute_hargreavesET.R
# 
# Author: Michael
###############################################################################
# implements the FAO Irr. & Drainage 56 paper Hargreaves ETo equation
# krigetype = 'ok' or 'ked'    
# a + b*0.0023*(0.5*(Tmax + tmin) + 17.8)*Ra*sqrt(Tmax - Tmin)

argsin <- commandArgs(TRUE)
Tminpath <- argsin[1]	# partial file path to Tmin data
Tmaxpath <- argsin[2]	# partial file path to Tmax data
#Tminpath <- "C:/Users/Michael/Desktop/KrigLayers/temperature_avgmin/2011-04/temperature_avgmin_2011-04"
#Tmaxpath <- "C:/Users/Michael/Desktop/KrigLayers/temperature_avgmax/2011-04/temperature_avgmax_2011-04"

require(automap)

hargreaves <- function(tmin, tmax){ 
	a <- 0	# calibration coefficient, default = 0
	b <- 1	# calibration coefficient, default = 0
	n <- 106 # day of year
	c <- 2*pi*n/365
	Ra <- 0.408*0.0864*1367*((1.00011 + 0.034221*cos(c) + 0.001280*sin(c) + 
				             0.000719*cos(2*c) + 0.000077*sin(2*c) )^2)	# extraterrestrial solar radiation in W/m^2	  
	writeLines("Computed extraterrestrial radiation:")
	res <- a + b*0.0023*(0.5*(tmax + tmin) + 17.8)*Ra*sqrt(tmax - tmin)
	return(res)
}

# get ok and ked Tmin
writeLines("Loading temperature data...")
load(paste(Tminpath, "_Rvars.Rdata", sep="") )
okTmin <- ok.krige$krige_output$var1.pred
kedTmin <- ked.krige$krige_output$var1.pred
rm(inoutpath, sample_df, unsampled_df, ok.krige, ked.krige, 
   ok.cv, ked.cv, cvcompare, varname)
# get ok and ked Tmax
load(paste(Tmaxpath, "_Rvars.Rdata", sep="") )
okTmax <- ok.krige$krige_output$var1.pred
kedTmax <- ked.krige$krige_output$var1.pred
# this time, keep unsampled_df
rm(inoutpath, sample_df, ok.krige, ked.krige, ok.cv, ked.cv, cvcompare, varname)

writeLines("Calculating ETo...")
kedET <- hargreaves(kedTmin, kedTmax)
okET <- hargreaves(okTmin, okTmax)

# define limits
kedrange <- c(min(kedET), mean(kedET), max(kedET) )
okrange <- c(min(okET), mean(okET), max(okET) )

# save ET prediction to file
writeLines("writing ETo data to file...")
save(unsampled_df, kedET, okET, okrange, kedrange, 
	 file=paste(Tmaxpath, "_hargreavesET.Rdata", sep="") )
writeLines("Hargreaves ETo computed sucessfully.")