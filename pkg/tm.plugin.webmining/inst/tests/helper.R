# TODO: Add comment
# 
# Author: mario
###############################################################################

file.appid <- system.file("appid.R", package = "tm.plugin.webmining")

if(file.exists(file.appid)){
	source(file.appid)
	cat("appid.R File provided.\n")
}else{
	cat("No appid.R File provided.\n")
}

