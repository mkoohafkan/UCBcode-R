# TODO: Add comment
# 
# Author: Michael
###############################################################################

argsin <- commandArgs(TRUE)
inoutpath <- argsin[1]

# packages
require(ggplot2)
require(scales)
require(automap)
require(RColorBrewer)
require(plyr)

# image format
pftype = ".pdf"
pwidth = 8
pheight = 8
pdpi = 300

# load the data
writeLines("loading data...")
load(paste(inoutpath, "_hargreavesET.Rdata", sep="") ) # loads variables including unsampled_df, ked.krige, ok.krige
xy <- coordinates(unsampled_df)*0.3048

# define plots
plotked = TRUE
plotok = FALSE
myplot <- function(therange, ...){
	# colors
	mypalette <- brewer.pal(7,"YlGn")
	highcolor <- "#006837"
	lowcolor <- "#543005"
	midcolor <- "#FFFFBF"
	#highcolor <- "#FFFFFF"
	#lowcolor <- "#000000"
	#midcolor <- "#969696"	
	#labels and limits
	themid <- therange[2]
	thelims <- c(round_any(therange[1], 2, f=floor), round_any(therange[3], 2, f=ceiling) )
	thebreaks <- round_any(c(therange[1], therange[3]), 1)
	thelabels <- thebreaks
	# define the scale
	thescale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
			low=lowcolor, high=highcolor, mid=midcolor, 
			limits=thelims, midpoint=themid, breaks=thelims, 
			label=thelabels, name=" ")
	# make the plot
	formattedplot <- qplot(..., geom="raster") + theme_bw() +
			coord_fixed() + ylab("y (m)") + xlab("x (m)") + thescale +
			theme(axis.title.x = element_text(size=18), 
					axis.text.x = element_text(size=14), 
					axis.title.y = element_text(size=18), 
					axis.text.y = element_text(size=14), 
					legend.text = element_text(size = 18) )
					#,
					#panel.background = element_rect(fill = bgcolor, colour = NA),
					#plot.background = element_rect(fill = "transparent", colour = NA))	
	return(formattedplot)
}
if(plotked){
	writeLines("writing KED plot...")
	savepath <- paste(inoutpath, "_(KED)hargreavesET test", pftype, sep="")
	kedpred.plot <- myplot(kedrange, xy[,1], xy[,2], fill=kedET)
	ggsave(filename=savepath, plot=kedpred.plot, width=pwidth, height=pheight, dpi=pdpi)		
}
if(plotok){
	writeLines("writing OK plot...")
	savepath <- paste(inoutpath, "_(OK)hargreavesET", pftype, sep="")
	okpred.plot <- myplot(okrange, xy[,1], xy[,2], fill=okET)
	ggsave(file=savepath, plot=okpred.plot, width=pwidth, height=pheight, dpi=pdpi)		
}
writeLines("plots written successfully.")
