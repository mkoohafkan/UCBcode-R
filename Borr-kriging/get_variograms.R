# get_variograms.R
# 
# Author: Michael
###############################################################################

get_variograms <- function(formula, dflist){
	opts <- list(orig.behavior=FALSE, equal.np.bins=TRUE, min.np.bin=25)
	vgm.list <- vector("list", length(dflist))
	for(i in 1:length(dflist)){
		cutoff <- spDists(t(bbox(input_data)), longlat=FALSE)[1,2] * 0.5
		vgm.list[[i]] <- autofitVariogram(formula, dflist[[i]], 
				                          miscFitOptions=opts, cutoff=cutoff,
										  verbose=TRUE)
	}
	return(vgm.list)
}