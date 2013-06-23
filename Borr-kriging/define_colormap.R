# define_colormap 
# 
# Author: Michael
###############################################################################

# command-line argument: path to Rdata file containing predmin, predmax, varmin, varmax, stdevmin, stdevmax
argsin <- commandArgs(TRUE) 
inoutpath <- argsin[1]

#packages
require(ggplot2)
require(scales)
require(plyr)
require(RColorBrewer)

# load the data
source(paste(inoutpath, "_limits.Rdata", sep="") )

# color legend definitons
myscale <- function(lims, mid, vname){
	vartype <- unlist(strsplit(paste(vname, collapse=""), "_"))
	if(vartype[1] == "temperature"){
		writeLines("Creating temperature scale...")	
		# continuous version of RdYlGn
		mypalette <- brewer.pal(11,"RdYlGn")
		midcolor <- "#FFFFBF"
		lowcolor <- "#006837"
		highcolor <- "#A50026"
		plims <- c(round_any(lims[1], 2, f=floor), round_any(lims[2], 2, f=ceiling) )
		pbreaks <- round_any(c(lims[1], lims[2]), 1)
		plabs <- pbreaks
		pname <- expression(degree~C)
	} else if(vartype[1] == "humidity"){
		if(vartype[2] == "avgmin"){
			bgcolor <- "grey90" # min hum = day hum
		} else if(vartype[2] == "avgmax"){
			bgcolor <- "grey50" # max hum = night hum
		}
		writeLines("Creating humidity scale")
		# continuous version of PuOr
		mypalette <- brewer.pal(11,"PuOr")
		lowcolor <- "#7F3B08"
		midcolor <- "#F7F7F7"
		highcolor <- "#2D004B"
		plims <- c(round_any(lims[1], 5, f=floor), round_any(lims[2], 5, f=ceiling) )
		pbreaks <- round_any(c(lims[1], lims[2]), 1)
		plabs <- pbreaks
		pname <- "%"
	} else{
		lowcolor <- muted("blue")
		highcolor <- muted("red")
		midcolor <- "white"
		plims <- lims
		pbreaks <- seq(lims[1], lims[2], by=abs(lims[2]-lims[1])/4)
		plabs <- pbreaks
		pname <- " "
	}
	formattedscale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
					  					   low=lowcolor, high=highcolor, mid=midcolor, 
										   limits=plims, midpoint=mid, breaks=pbreaks, 
										   label=plabs, name=pname)
	return(formattedscale)
}
mystatscale <- function(lims, brks, labs, sname){
	formattedscale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
			low="white", high="black", limits=lims, 
			breaks=brks, label=labs, 
			name="Standard Deviation")
	return(formattedscale)				 
}
# define limits
# prediction
okplims <- c(okprange[1], okprange[3])
okpmid <- okprange[2]
kedplims <- c(kedprange[1], kedprange[3])
kedpmid <- kedprange[2]
# define colorscales
okpscale <- myscale(okplims, okpmid, varname)
kedpscale <- myscale(kedplims, kedpmid, varname)
# other stats
# variance
okvlims <- c(okvrange[1], okvrange[3])
okvmid <- okvrange[2]
kedvlims <- c(kedvrange[1], kedvrange[3])
kedvmid <- kedvrange[2]
# standard deviation
oksdlims <- c(oksdrange[1], oksdrange[3])
oksdmid <- oksdrange[2]
kedsdlims <- c(kedsdrange[1], kedsdrange[3])
kedsdmid <- kedsdrange[2]
# define colorscales


#save(okpscale, kedpscale, vscale, sdscale, plims, vlims, sdlims, pmid, vmid, sdmid, 
#		pbreaks, vbreaks, sdbreaks, plabels, vlabels, sdlabels, bgcolor,
#		file=paste(inoutpath, "_colorscales.Rdata", sep="") )
save(okpscale, kedpscale, okpmid, kedpmid, okplims, kedplims, varname, file=paste(inoutpath, "_colorscales.Rdata", sep="") )

writeLines("color scales generated successfully.")









## other breaks
#sdbreaks <- seq(sdlims[1], sdlims[2], by=abs(sdlims[2]-sdlims[1])/4)
#sdlabels <- signif(sdbreaks, 2)	
#vbreaks <- seq(vlims[1], vlims[2], by=abs(vlims[2]-vlims[1])/4)
#vlabels <- signif(vbreaks, 2)
## create colorscales
#
#
## make scales
#okpscale <- myscale(lowcolor, midcolor, highcolor, okplims, okpbreaks)
#kedpscale <- myscale(lowcolor, midcolor, highcolor, okplims, okpbreaks)
#		
#pscale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
#		                       low=lowcolor, high=highcolor, mid=midcolor, 
#							   limits=plims, midpoint=pmid, breaks=pbreaks, 
#							   label=plabels, name=pname)
#vscale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
#		                       low="white", high="black", limits=vlims, 
#							   breaks=vbreaks, label=vlabels,  name=" ")
#sdscale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
#		                        low="white", high="black", limits=sdlims, 
#								breaks=sdbreaks, label=sdlabels, name=" ")
# save the colorscales to a file
