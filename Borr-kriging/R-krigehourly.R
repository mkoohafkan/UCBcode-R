# R-krigehourly.R
#
# MODIFIED VERSION OF R-krige.R
# CHECKS EACH DATASET FOR PIECEWISE OR LINEAR TREND
# 
# Author: Michael
###############################################################################

source("C:/Users/Michael/Desktop/GITrepo/R/borr-kriging/thefuncs.R")

# function
check_piecewise_trend <- function(pv, rv, bp, df){
	# TODO fix it so it doesn't do piecewise during the day 
	# use f-statistic and adjusted R squared to identify fit to use:
	# linear or piecewise linear at specified breakpoint
	f1 <- as.formula(paste(rv, "~", pv))
	f2 <- as.formula(paste(rv, "~ 0 +", pv, "*(", pv, "<", bp, ") - ", pv))
	
	m1 <- summary(lm(f1, df))
	m2 <- summary(lm(f2, df))
	
	if(m1$adj.r.squared < m2$adj.r.squared & m1$fstatistic[1] < m2$fstatistic[1]){
		cat(m2$adj.r.squared, ' vs ',  m1$adj.r.squared, '\n')
		cat(m1$fstatistic[1], ' vs ', m2$fstatistic[1], '\n')
		return(f2)
	}else{
		return(f1)
	}
}

# load the grid
basepath <- 'C:/Users/Michael/Desktop/KrigLayers/pureR/hourly matching/'
gridpath <- paste(basepath, 'basegridxyz.txt', sep="")
basegrid <- gridtoDF(gridpath)

imgtype <- '.png'

opts <- list(orig.behavior=FALSE, equal.width.bins=FALSE, equal.np.bins=TRUE, min.np.bin=30)
pr = TRUE
cress = FALSE

# prep
predictorvar <- 'z'
pgroup <- 600
responsevar <- 'temperature_avg'
dbtable <- 'krigedatahourly'
fields <- c("projx", "projy", predictorvar, responsevar)

# make the queries 
queries <- scratchqueries(responsevar, dbtable)
labels <- scratchlabels(responsevar)

# create directories
for(label in labels) dir.create(file.path(basepath, label), recursive=T, showWarnings=F)

# connect to database
borrcon <- connectBORR()

# get the data
datalist <- datafromDB(queries, fields, borrcon)

# convert to spatial dataframe
coordsformula <- as.formula(paste("~", fields[1], "+", fields[2]))
xoffset <- min(basegrid[[fields[1]]])
yoffset <- min(basegrid[[fields[2]]])
basegrid[[fields[1]]] <- basegrid[[fields[1]]] - xoffset
basegrid[[fields[2]]] <- basegrid[[fields[2]]] - yoffset
coordinates(basegrid) <- coordsformula
# also get minps, maxps, cutoffs
minps <- rep(NA, length(datalist))
maxps<- minps
cutoffs <- minps
for(i in 1:length(datalist)){
	# convert to spatial dataframe
	datalist[[i]][[fields[1]]] <- datalist[[i]][[fields[1]]] - xoffset
	datalist[[i]][[fields[2]]] <- datalist[[i]][[fields[2]]] - yoffset
	coordinates(datalist[[i]]) <- coordsformula
	# also get minps, maxps, cutoffs
	minps[i] <- min(datalist[[i]][[predictorvar]])
	maxps[i] <- max(datalist[[i]][[predictorvar]])
	cutoffs[i] <- spDists(t(bbox(datalist[[i]])), longlat = FALSE)[1,2] * 0.5
}

# define formula
formulalist <- vector("list", length(datalist))
grouplist <- rep(0, length(datalist))
for(i in 1:length(datalist)){
	formulalist[[i]] <- check_piecewise_trend(predictorvar, responsevar, pgroup, datalist[[i]])
	# identify which formula is being used -- a bit of a hack
	if(unlist(strsplit(as.character(formulalist[[i]])[3], " "))[1] != predictorvar) grouplist[i] <- pgroup
}

# define variograms
vgmlist <- vector("list", length(datalist))
for(i in 1:length(datalist)){
	vgmlist[[i]] <- afvmod(formulalist[[i]], datalist[[i]], miscFitOptions=opts, 
			               cutoff=cutoffs[i], cressie=cress, PR=pr)
	png(file=paste(basepath, labels[[i]], '(KED)variogram.png', sep="") )
	print(annotatedplot(vgmlist[[i]]))
	dev.off()
}

# plot some linear models
for(i in 1:length(datalist)){
	fitplot <- ggplot(data = as.data.frame(datalist[[i]]), aes_string(x=predictorvar, y=responsevar)) +
			   geom_smooth(aes(group=z < grouplist[[i]]), se=TRUE, method="lm", colour="red") + 
			   geom_point()
	ggsave(filename=paste(basepath, labels[[i]], "_fitplot", imgtype, sep=""), 
			plot=fitplot, width=8, height=4, dpi=300)
}

# krige and plot
for(i in 1:length(datalist)){
	# define the gridmask, might be needed later
	gridmask <- basegrid[[predictorvar]] >= minps[i] & basegrid[[predictorvar]] <= maxps[i]
	gridobj <- basegrid[gridmask,]
	vgmobj <- vgmlist[[i]]
	sampleobj <- remove.duplicates(datalist[[i]])
	krigeobj <- as.data.frame(krige(formulalist[[i]], sampleobj, gridobj, model=vgmobj$var_model))
	names(krigeobj) <- c(fields[1:2], responsevar, "variance")
	krigeobj[predictorvar] <- as.data.frame(gridobj)[[predictorvar]]
	sampleobj <- as.data.frame(sampleobj)
	save(krigeobj, vgmobj, sampleobj, gridmask, file=paste(basepath, labels[[i]], "_krigeobj.Rdata", sep="") )
	# make raster plot
	prange <- c(min(krigeobj[[responsevar]]), mean(krigeobj[[responsevar]]), max(krigeobj[[responsevar]]))
	krigeraster <- krigeplot(ggplot(data=krigeobj, aes_string(x=fields[1], y=fields[2])), 
					         responsevar, prange) + 
			       geom_point(color="black", alpha=0.5, data=sampleobj)
	ggsave(filename=paste(basepath, labels[[i]], "_krigeplot", imgtype, sep=""), 
		   plot=krigeraster, width=8, height=8, dpi=300)
	# make elev plot
	elevplot <- ggplot(data=krigeobj, aes_string(x=predictorvar, y=responsevar)) + 
			    geom_point(size=0.25, color="black", alpha=0.25) +
			    geom_point(color="darkred", data=sampleobj) 
	ggsave(filename=paste(basepath, labels[[i]], "_elevplot", imgtype, sep=""), 
		   plot=elevplot, width=8, height=4, dpi=300)
}