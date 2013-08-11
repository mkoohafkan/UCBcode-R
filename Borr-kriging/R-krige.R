# R-krige.R
# 
# Author: Michael
###############################################################################

source("C:/repositories/codeRepo/R/trunk/Borr-kriging/thefuncs.R")
basepath <- 'C:/Users/Michael/Desktop/KrigLayers/pureR/daily/'

# load the grid
gridpath <- paste(basepath, 'basegridxyz.txt', sep="")
basegrid <- gridtoDF(gridpath)

imgtype <- '.png'

# prep
predictorvar <- 'z'
responsevar <- 'temperature_avgmin'
dbtable <- 'krigedatadaily'
fields <- c("projx", "projy", "nodeid", predictorvar, responsevar)

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
xoffset <- min(basegrid[[fields[1]]])
yoffset <- min(basegrid[[fields[2]]])
coordsformula <- as.formula(paste("~", fields[1], "+", fields[2]))
basegrid[[fields[1]]] <- basegrid[[fields[1]]] - xoffset
basegrid[[fields[2]]] <- basegrid[[fields[2]]] - yoffset
coordinates(basegrid) <- coordsformula
# also get minps, maxps, cutoffs
minps <- rep(NA, length(datalist))
maxps<- minps
cutoffs <- minps

# format datalist
for(i in seq(1, length(datalist), 1)){
	# correct xy locations to match basegrid cells
	datalist[[i]] <- na.omit(datalist[[i]])
	# convert to spatial dataframe
	datalist[[i]][[fields[1]]] <- datalist[[i]][[fields[1]]] - xoffset
	datalist[[i]][[fields[2]]] <- datalist[[i]][[fields[2]]] - yoffset
	coordinates(datalist[[i]]) <- coordsformula
	# also get minps, maxps, cutoffs
	minps[i] <- min(datalist[[i]][[predictorvar]])
	maxps[i] <- max(datalist[[i]][[predictorvar]])
	cutoffs[i] <- spDists(t(bbox(datalist[[i]])), longlat = FALSE)[1,2] * 0.5
	datalist[[i]] <- remove.duplicates(datalist[[i]])
}

# helper function for formulas
piece.formula <- function(var.name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}
# define formula
rvtrend <- vector("list", length(datalist))
if(responsevar != 'temperature_avgmax' || responsevar !="humidity_avgmin"){
	for(i in seq(1, length(datalist), 1)){
		# get best estimate of breakpoint
		initmod <- lm(as.formula(paste(responsevar,"~", predictorvar)), datalist[[i]])
		segmod <- segmented.lm(initmod, seg.Z=as.formula(paste("~", predictorvar)), psi=600)
		pgroup <- segmod$psi[2] # best estimate of breakpoint
		rvtrend[[i]] <- as.formula(paste(responsevar, "~", piece.formula(predictorvar, pgroup)))
	}	
} else{
	rvtrend[] <- as.formula(paste(responsevar,"~", predictorvar))
}

# specify variogram options
vgmfitmethod <- 1 # 1,2,6,7. For 5 use fit.variogram.reml
#vgmmodels <- c("Exp")
vgmmodels <- c("Sph", "Exp", "Gau", "Ste")
#fixvalues=c(0, NA, NA)
fixvalues=c(NA, NA, NA)
if(responsevar == 'temperature_avgmin' || responsevar == 'temperature_avg' || 
   responsevar == 'temperature_min'){
	opts <- list(orig.behavior=FALSE, equal.width.bins=FALSE, init.width=0.1, 
	             equal.np.bins=TRUE, min.np.bin=10)
	cutoffs[] <- 1
	pr = FALSE
	cress = TRUE
} else if(responsevar == 'humidity_avgmax' || responsevar == 'humidity_max' ||
		  responsevar == 'humidity_avg'){
	opts <- list(orig.behavior=FALSE, equal.width.bins=FALSE, init.width=0.1, 
				 equal.np.bins=TRUE, min.np.bin=10)
	cutoffs[] <- 1
	pr = FALSE
	cress = TRUE
} else{
	opts <- list(orig.behavior=FALSE, equal.width.bins=FALSE, init.width=0.1, 
				 equal.np.bins=TRUE, min.np.bin=10)
	cutoffs[] <- 1
	pr = FALSE
	cress = TRUE
} 
# define variograms
vgmlist <- vector("list", length(datalist))
for(i in seq(1, length(datalist), 1)){
	vgmlist[[i]] <- afvmod(rvtrend[[i]], datalist[[i]], vgmmodels,
						   miscFitOptions=opts, cutoff=cutoffs[i], 
						   cressie=cress, PR=pr, fit.method=vgmfitmethod,
						   fix.values=fixvalues)
	# plot thevariogram
	png(file=paste(basepath, labels[[i]], '(KED)variogram.png', sep="") )
	print(annotatedplot(vgmlist[[i]]))
	dev.off()
}

# plot some linear models
ggformula <- vector("list", length(datalist))
for(i in seq(1, length(datalist), 1)){
	ggformula[[i]] <- as.formula(gsub(responsevar, 'y', 
                                 gsub(predictorvar, 'x', 
								      deparse(rvtrend[[i]]))))
	fitplot <- ggplot(data=as.data.frame(datalist[[i]]), 
	                  aes_string(x=predictorvar, y=responsevar)) +
			   geom_smooth(method='lm', formula=ggformula[[i]], color="red") + 
			   geom_point()
	ggsave(filename=paste(basepath, labels[[i]], "_fitplot", imgtype, sep=""), 
		   plot=fitplot, width=8, height=4, dpi=300)
}

# krige and plot
for(i in seq(1, length(datalist), 1)){
	# define the gridmask, might be needed later
	gridmask <- basegrid[[predictorvar]] >= minps[i] & 
	            basegrid[[predictorvar]] <= maxps[i]
	gridobj <- basegrid[gridmask,]
	vgmobj <- vgmlist[[i]]
	sampleobj <- datalist[[i]]
	fitformula <- rvtrend[[i]]
	krigeobj <- as.data.frame(krige(fitformula, sampleobj, gridobj, 
	                                model=vgmobj$var_model))
	names(krigeobj) <- c(fields[1:2], responsevar, "variance")
	krigeobj[predictorvar] <- as.data.frame(gridobj)[[predictorvar]]
	sampleobj <- as.data.frame(sampleobj)
	save(krigeobj, vgmobj, sampleobj, fitformula, gridmask, 
	     file=paste(basepath, labels[[i]], "_krigeobj.Rdata", sep="") )
	# make raster plot
	prange <- c(min(krigeobj[[responsevar]]), mean(krigeobj[[responsevar]]), 
				max(krigeobj[[responsevar]]))
	krigeraster <- krigeplot(ggplot(data=krigeobj, 
									aes_string(x=fields[1], y=fields[2])), 
					         responsevar, prange) + 
			       geom_point(color="black", alpha=0.5, data=sampleobj)
	ggsave(filename=paste(basepath, labels[[i]], "_krigeplot", imgtype, sep=""), 
		   plot=krigeraster, width=8, height=8, dpi=300)
	# make variance plot	   
	vrange <- c(min(krigeobj[["variance"]]), mean(krigeobj[["variance"]]), 
				max(krigeobj[["variance"]]))
	varianceraster <- krigeplot(ggplot(data=krigeobj, 
									   aes_string(x=fields[1], y=fields[2])), 
					            "variance", vrange) + 
					  geom_point(color="black", alpha=0.5, data=sampleobj)
	ggsave(filename=paste(basepath, labels[[i]], "_varianceplot", imgtype, sep=""), 
		   plot=varianceraster, width=8, height=8, dpi=300)
	# make elev plot
	elevplot <- ggplot(data=krigeobj, aes_string(x=predictorvar, y=responsevar)) + 
			    geom_point(size=0.25, color="black", alpha=0.25) +
				geom_point(color="red", data=sampleobj) +
			    geom_smooth(method='lm', formula=ggformula[[i]], 
				            data=sampleobj, se=FALSE, color="red")
	ggsave(filename=paste(basepath, labels[[i]], "_elevplot", imgtype, sep=""), 
			plot=elevplot, width=8, height=4, dpi=300)
}