source("C:/repositories/codeRepo/R/trunk/Borr-kriging/thefuncs.R")
basepath <- 'C:/Users/Michael/Desktop/KrigLayers/pureR/'
dir.create(file.path(basepath, 'boxplots'), recursive=T, showWarnings=F)
dirpath <- paste(basepath, 'boxplots/', sep="")
conn <- connectBORR()

###
tablename <- 'temphumiditydaily'
#responsevar <- 'temperature_avgmin'
#ordervar <- 'ASC'
responsevar <- 'temperature_avgmax'
ordervar <- 'DESC'
rankvar <- paste('rank_', responsevar, sep="")
fields <- c('result_time', 'mnthyr', 'day', 'nodeid', responsevar, rankvar)
# the query
query <- paste("SELECT date_trunc('day', result_time)::date::text AS result_time, ",
					  "date_trunc('month', result_time)::date::char(7) AS mnthyr, ",
					  "date_part('day', result_time)::integer AS day, ",
					  "nodeid, ", responsevar, " , rank() ", 
			   "OVER (PARTITION BY date_trunc('day', result_time) ",
			   "ORDER BY ", responsevar, "  ", ordervar, ") ", "AS ", rankvar, " ",
			   "FROM ", tablename, " ",
			   "WHERE date_part('month', result_time) > 5  ",
					 "AND date_part('month', result_time) < 9  ",
					 "AND date_part('year', result_time) > 2010  ",
			   "ORDER BY ", rankvar, " , result_time, nodeid")
###
			   
# query data
rankdata <- datafromDB(query, fields, conn)[[1]] # because we only have one frame

# base plots
opts <- vector("list", length=2)
basep <- ggplot(rankdata, aes(x=factor(nodeid)))
opts[[1]] <- facet_wrap(~ mnthyr)
opts[[2]] <- xlab("nodeid")

# histogram
hp <- basep + geom_histogram() + opts
ggsave(filename=paste(dirpath, responsevar, "_hp.pdf", sep=""), 
	   plot=hp, width=50, height=10, dpi=300, 
	   limitsize=FALSE)

# box and whisker plots
# of temperature
tempbp <- geom_boxplot(aes_string(y=responsevar))
tempp <- basep + tempbp + opts + ylab(responsevar)
ggsave(filename=paste(dirpath, responsevar, "_bp.pdf", sep=""), 
	   plot=tempp, width=50, height=10, dpi=300, 
	   limitsize=FALSE)
# of ranks
rankbp <- geom_boxplot(aes_string(y=rankvar))
rankp <- basep + rankbp + opts + ylab(paste("rank by", responsevar))
ggsave(filename=paste(dirpath, rankvar, "_bp.pdf", sep=""), 
	   plot=rankp, width=50, height=10, dpi=300, 
	   limitsize=FALSE)
	   
# violin plots
# helper function
median.quartile <- function(x){
	out <- quantile(x, probs = c(0.25,0.5,0.75))
	names(out) <- c("ymin","y","ymax")
	return(out) 
}

sp <- stat_summary(aes_string(y=responsevar), fun.y=mean, geom="point", shape=4)
#sp<- stat_summary(aes_string(y=responsevar), fun.data=median.quartile, geom="pointrange", shape=4)
# of temperature
tempvp <- geom_violin(aes_string(y=responsevar), trim=FALSE)
tempp <- basep + tempvp + sp + opts + ylab(responsevar) + sp         
ggsave(filename=paste(dirpath, responsevar, "_vp.pdf", sep=""), 
	   plot=tempp, width=50, height=10, dpi=300, 
	   limitsize=FALSE)
# of ranks
sp <- stat_summary(aes_string(y=rankvar), fun.y=mean, geom="point", shape=4)
rankvp <- geom_violin(aes_string(y=rankvar), trim=FALSE)
rankp <- basep + rankvp + sp + opts + ylab(paste("rank by", responsevar))
ggsave(filename=paste(dirpath, rankvar, "_vp.pdf", sep=""), 
	   plot=rankp, width=50, height=10, dpi=300, 
	   limitsize=FALSE)