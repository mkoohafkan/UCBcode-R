# TODO: Add comment
# 
# Author: Michael
###############################################################################

loadunloadworkspace <- function(fp){
	if(getcwd == fp){
		# save and unload the workspace
		save.image()
		rm(list = ls(all.names = TRUE))
		return("all workspace objects were unloaded.")
	}
	else{
		# set the working directory and load the workspace
		setcwd(fp)
		load(".Rdata")
		return("the workspace was loaded successfully.")
	}
	return("an unexpected error has occurred.")
}