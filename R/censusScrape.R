#!/usr/bin/env R
# Author:  David M. Condon
# Maintainer:  Jason A. French
# URL:  http://sapa-project.org/r/

getUSCD <- 
function(x, state=NULL, key=NULL, delay=NULL) {

	# A function for scraping and cleaning up census data by ZCTA
	# max number of variables is 50
	# 
	# args:
	#	x: 		a list of strings of census variables like: c("H0060001","H0060002")
	#	state:	a list of numeric census state codes like: 17 or c(17,19)
	#	key:	your own census key (Google it)
	#	delay:	number to seconds to pause btw API hits
	#
	# returns: USCD data.frame()

	if(!require(rjson)){
		stop('You need to install the rjson package!')
	}

	if(length(x) > 1) {
		x <- paste(x, collapse=',' )
		}
		
	USCD <- data.frame()

	for (i in 1:length(state)) {
		url <- paste("http://api.census.gov/data/2010/sf1?get=", x,
			"&for=zip+code+tabulation+area:*&in=state:", state[i], "&key=", key, sep="")
		ZCTAstrings <- fromJSON(getURL(url))
		USCDdf <- as.data.frame(t(sapply(ZCTAstrings[2:length(ZCTAstrings)], rbind)))
		USCD <- rbind(USCD, USCDdf)
		Sys.sleep(delay)
		}

	names(USCD) <- unlist(ZCTAstrings[1])
	colnames(USCD)[ncol(USCD)] <- "ZCTA"

	return(USCD)
	
	}

	getACS <- 
	function(x, key=NULL) {
		#
		# A function for scraping and cleaning up ACS data by ZCTA
		# x is a list of strings of ACS variables like: c("H0060001","H0060002")
		# max number of variables is 50
		# key is your own census key (google it)

		require(rjson)

		if(length(x) > 1) {
			x <- paste(x, collapse=',' )
			}
			
		url <- paste("http://api.census.gov/data/2011/acs5?get=", x, "&for=zip+code+tabulation+area:*&key=", key, sep="")
		
		ZCTAstrings <- fromJSON(getURL(url))

		ACS <- as.data.frame(t(sapply(ZCTAstrings[2:length(ZCTAstrings)], rbind)))
			
		names(ACS) <- unlist(ZCTAstrings[1])
		colnames(ACS)[ncol(ACS)] <- "ZCTA"

		return(ACS)
		
		}