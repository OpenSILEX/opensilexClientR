#-------------------------------------------------------------------------------
# Program: zzz.R
# Objective:
# Author: I. Sanchez
# Creation: 19/03/2018
# Update: 03/09/2019 A. Charleroy
#-------------------------------------------------------------------------------

## title defines a public configuration for the web service access
##
## description defines a public configuration for the web service access
## param libname character
## param pkgname character
## examples
## # not run
## keywords internal
#   configWS <- list( 
#                    DEFAULT_PAGE = 0,
#                    DEFAULT_PAGESIZE = 100)
## Define an environment for the opensilex configuration
configWS<-new.env(emptyenv())

.onLoad <- function(libname, pkgname){
  # internal variables 
  assign("DEFAULT_PAGE", 0, configWS)
  assign("DEFAULT_PAGESIZE", 100, configWS)
  
  debugLevel <-list(CRITICAL = 50, ERROR = 40,
                    WARNING = 30, INFO = 20,
                    DEBUG = 10, NOTSET = 0)
  assign("DEBUG_LEVEL", debugLevel, configWS) 
}

##' @title getConfig 
##' get config
##' @export
getConfig <- function(){
  return(configWS)
}

