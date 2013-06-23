krigeplot <- function(plotobj, vname, therange){
	vartype <- unlist(strsplit(paste(vname, collapse=""), "_"))	
	# set the background color
	if(vartype[1] == "temperature" & (vartype[2] == "avgmin"| vartype[2] == "min")){
		bgcolor <- "grey50" # mintemp = night temp
	} else if(vartype[1] == "humidity" & (vartype[2] == "avgmax" | vartype[2] == "max")){
		bgcolor <- "grey50" # max hum = night hum
	} else{
		bgcolor <- "grey90" # daytime or average temp, hum, or other var
	}
	# set the color scale
	if(vartype[1] == "temperature"){
		# continuous version of RdYlGn
		midcolor <- "#FFFFBF"
		lowcolor <- "#006837"
		highcolor <- "#A50026"
		thelims <- c(round_any(therange[1], 1, f=floor), round_any(therange[3], 1, f=ceiling) )		
		thebreaks <- thelims
		thelabs <- thebreaks
		#thename <- expression(degree~C)
		thename <- "K"
	} else if(vartype[1] == "humidity"){
		if(vartype[2] == "avgmin"){
			bgcolor <- "grey90" # min hum = day hum
		} else if(vartype[2] == "avgmax"){
			bgcolor <- "grey50" # max hum = night hum
		}
		# continuous version of PuOr
		lowcolor <- "#7F3B08"
		midcolor <- "#F7F7F7"
		highcolor <- "#2D004B"
		thelims <- c(round_any(therange[1], 1, f=floor), round_any(therange[3], 1, f=ceiling) )
		thebreaks <- thelims
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
	themid <- 0.5*(thelims[1] + thelims[2])
	# define the colorscale
	thescale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
			low=lowcolor, high=highcolor, mid=midcolor, 
			limits=thelims, midpoint=themid, breaks=thebreaks, 
			label=thelabs, name=thename)
	# plot the data
	formattedplot <- plotobj + geom_raster(aes_string(fill = vname)) +coord_fixed() + 
			         ylab("y (km)") + xlab("x (km)") + thescale + 
					 theme(axis.title.x = element_text(size=18), 
					 	   axis.text.x = element_text(size=14), 
						   axis.title.y = element_text(size=18), 
						   axis.text.y = element_text(size=14), 
						   legend.text = element_text(size = 18),
						   panel.background = element_rect(fill = bgcolor, colour = NA),
						   plot.background = element_rect(fill = "transparent", colour = NA))	
	return(formattedplot)
}

ETplot <- function(df, therange){
	# colors
	#mypalette <- brewer.pal(7,"YlGn")
	highcolor <- "#006837"
	lowcolor <- "#543005"
	midcolor <- "#FFFFBF"
	#labels and limits
	themid <- therange[2]
	thelims <- c(round_any(therange[1], 2, f=floor), round_any(therange[3], 2, f=ceiling) )
	thebreaks <- round_any(c(therange[1], therange[3]), 1)
	thelabels <- thebreaks
	# define the scale
	thescale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
			low=lowcolor, high=highcolor, mid=midcolor, 
			limits=thelims, midpoint=themid, breaks=thelims, 
			label=thelabels, name="mm/day")
	# make the plot
	formattedplot <- qplot(df[,1], df[,2], fill=df[,3], geom="raster") + theme_bw() +
			coord_fixed() + ylab("y (km)") + xlab("x (km)") + thescale +
			theme(axis.title.x = element_text(size=18), 
					axis.text.x = element_text(size=14), 
					axis.title.y = element_text(size=18), 
					axis.text.y = element_text(size=14), 
					legend.text = element_text(size = 18))	
	return(formattedplot)
}

varianceplot <- function(df, therange){
	highcolor <- muted("red")
	lowcolor <- muted("blue")
	midcolor <- "white"
	themid <- therange[2]
	thelims <- c(therange[1], therange[3] )
	thebreaks <- thelims
	thelabels <- thebreaks
	# define the scale
	thescale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
			low=lowcolor, high=highcolor, mid=midcolor, 
			limits=thelims, midpoint=themid, breaks=thelims, 
			label=thelabels, name="variance\n\n")
	# make the plot
	formattedplot <- qplot(df[,1], df[,2], fill=df[,4], geom="raster") + 
			coord_fixed() + ylab("y (km)") + xlab("x (km)") + thescale +
			theme(axis.title.x = element_text(size=18), 
					axis.text.x = element_text(size=14), 
					axis.title.y = element_text(size=18), 
					axis.text.y = element_text(size=14), 
					legend.text = element_text(size = 18),
					panel.background = element_rect(fill = "grey70", colour = NA))	
	return(formattedplot)
}

elevplot <- function(ds, responsevar){
	elevplot <- qplot(ds[['z']], ds[[responsevar]], geom="point")
	return(elevplot)
}

elevdistro <- function(basegrid, sampleobj){
	# plot freunecy distribution of elevation and the 
	# elevations of the sampling locations
	# dem
	elevdistro <- ggplot(data=basegrid, aes(z, ..density..)) + 
                    geom_freqpoly(size=1, binwidth=5) + 
			  geom_histogram(data=sampleobj, binwidth=5, color="black", 
					     fill='red', alpha=0.2) + 
			  theme_bw() + xlab("elevation")
	return(elevdistro)
}