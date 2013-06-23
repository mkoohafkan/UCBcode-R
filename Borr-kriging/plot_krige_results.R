# TODO: Add comment
# 
# Author: Michael
###############################################################################

argsin <- commandArgs(TRUE)
inoutpath <- argsin[1]

pftype = ".png"
pwidth = 8
pheight = 8
pdpi = 300

# packages
require(ggplot2)
require(scales)
require(automap)
require(plyr)
require(RColorBrewer)

# load the data
writeLines("loading data...")
#load(paste(inoutpath, "_colorscales.Rdata", sep="") ) # loads objects pscale, vscale, sdscale
load(paste(inoutpath, "_Rvars.Rdata", sep="") ) # loads variables including unsampled_df, ked.krige, ok.krige
source(paste(inoutpath, "_limits.Rdata", sep="") ) # loads color scale limits

xy <- coordinates(unsampled_df)*0.3048

# define plots to make
plotked = TRUE
plotok = FALSE
plotpred = TRUE
plotvar = FALSE
plotsd = FALSE

# define plot title
tag <- tail(unlist(strsplit(inoutpath, "_", fixed=TRUE) ), 1)

pplot <- function(therange, vname, ...){
	vartype <- unlist(strsplit(paste(vname, collapse=""), "_"))	
	# set the background color
	if(vartype[1] == "temperature" & vartype[2] == "avgmin"){
		bgcolor <- "grey50" # mintemp = night temp
	} else if(vartype[1] == "humidity" & vartype[2] == "avgmax"){
		bgcolor <- "grey50" # max hum = night hum
	} else{
		bgcolor <- "grey90" # daytime temp, hum
	}
	# set the color scale
	if(vartype[1] == "temperature"){
		# continuous version of RdYlGn
		#mypalette <- brewer.pal(11,"RdYlGn")
		midcolor <- "#FFFFBF"
		lowcolor <- "#006837"
		highcolor <- "#A50026"
		thelims <- c(round_any(therange[1], 1, f=floor), round_any(therange[3], 1, f=ceiling) )		
		thebreaks <- thelims
		#thebreaks <- round_any(c(therange[1], therange[3]), 1)
		thelabs <- thebreaks
		thename <- expression(degree~C)
	} else if(vartype[1] == "humidity"){
		if(vartype[2] == "avgmin"){
			bgcolor <- "grey90" # min hum = day hum
		} else if(vartype[2] == "avgmax"){
			bgcolor <- "grey50" # max hum = night hum
		}
		# continuous version of PuOr
		#mypalette <- brewer.pal(11,"PuOr")
		lowcolor <- "#7F3B08"
		midcolor <- "#F7F7F7"
		highcolor <- "#2D004B"
		thelims <- c(round_any(therange[1], 5, f=floor), round_any(therange[3], 5, f=ceiling) )
		thebreaks <- round_any(c(therange[1], therange[3]), 1)
		thelabs <- thebreaks
		thename <- "%"
	} else{
		lowcolor <- muted("blue")
		highcolor <- muted("red")
		midcolor <- "white"
		thelims <- c(round_any(therange[1], 1, f=floor), round_any(therange[3], 1, f=ceiling) )
		thebreaks <- seq(thelims[1], thelims[2], by=abs(thelims[2] - thelims[1])/4)
		thelabs <- thebreaks
		thename <- " "
	}
	themid <- therange[2]
	# define the colorscale
	thescale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
									 low=lowcolor, high=highcolor, mid=midcolor, 
									 limits=thelims, midpoint=themid, breaks=thebreaks, 
									 label=thelabs, name=thename)
	# plot the data
	formattedplot <- qplot(..., geom="raster") + coord_fixed() + ylab("y (m)") + xlab("x (m)") + 
					 thescale + theme(axis.title.x = element_text(size=18), 
							          axis.text.x = element_text(size=14), 
									  axis.title.y = element_text(size=18), 
									  axis.text.y = element_text(size=14), 
									  legend.text = element_text(size = 18),
									  panel.background = element_rect(fill = bgcolor, colour = NA),
									  plot.background = element_rect(fill = "transparent", colour = NA))	
	return(formattedplot)
}
statplot <- function(therange, stattype, ...){
	if(stattype == "sd"){
		thename <- "standard deviation"
	} else if(stattype == "v"){
		thename <- "variance"
	}
	lowcolor <- "white"
	highcolor <- "black"
	thelims <- c(round_any(therange[1], 0.1, f=floor), round_any(therange[3], 0.1, f=ceiling) )
	thebreaks <- seq(thelims[1], thelims[2], by=abs(thelims[2] - thelims[1])/4)
	thelabs <- thebreaks
	# make the scale
	thescale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
			low=lowcolor, high=highcolor, limits=thelims, breaks=thebreaks, 
			label=thelabs, name=thename)
	# plot the data
	formattedplot <- qplot(..., geom="raster") + coord_fixed() + ylab("y (m)") + xlab("x (m)") + 
					 thescale + theme(axis.title.x = element_text(size=18), 
					 				  axis.text.x = element_text(size=14), 
									  axis.title.y = element_text(size=18), 
									  axis.text.y = element_text(size=14), 
									  legend.text = element_text(size = 18),
									  panel.background = element_rect(fill = bgcolor, colour = NA),
									  plot.background = element_rect(fill = "transparent", colour = NA))		
}

if(plotked){
	writeLines("writing KED plots...")
	if(plotpred){
		savepath <- paste(inoutpath, "_(KED)prediction", pftype, sep="")
		kedpred.plot <- pplot(kedprange, varname, xy[,1], xy[,2], fill=ked.krige$krige_output$var1.pred)
		ggsave(filename=savepath, plot=kedpred.plot, width=pwidth, height=pheight, dpi=pdpi)		
	} 
	if(plotvar){
		savepath <- paste(inoutpath, "_(KED)variance", sep="")
#		kedvar.plot <- formattedplot(xy[,1], xy[,2], ked.krige$krige_output$var1.var, "v", tag, savepath)
	} 
	if(plotsd){
		savepath <- paste(inoutpath, "_(KED)stdeviation", sep="")
#		kedsd.plot <- formattedplot(xy[,1], xy[,2], ked.krige$krige_output$var1.stdev, "sd", tag, savepath)
	}
}
if(plotok){
	writeLines("writing OK plots...")
	if(plotpred){
		savepath <- paste(inoutpath, "_(OK)prediction", pftype, sep="")
		okpred.plot <- pplot(okprange, varname, xy[,1], xy[,2], fill=ok.krige$krige_output$var1.pred)
		ggsave(file=savepath, plot=okpred.plot, width=pwidth, height=pheight, dpi=pdpi)		
	}
	if(plotvar){
		savepath <- paste(inoutpath, "_(OK)variance", sep="") 
		#okvar.plot <- formattedplot(xy[,1], xy[,2], fill=ok.krige$krige_output$var1.var, "v", tag, savepath)
	}
	if(plotsd){
		savepath <- paste(inoutpath, "_(OK)stdeviation", sep="") 
		#oksd.plot <- formattedplot(xy[,1], xy[,2], ok.krige$krige_output$var1.stdev, "sd", tag, savepath)
	}
}
writeLines("plots written successfully.")
