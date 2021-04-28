#-------------------------------------------------------------------------------
# Program: getExperiments.R
# Objective: functions to get the experiments service from WS
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq
# Creation: 24/01/2019
# Update: 06/01/2020 (by I.Sanchez)
#-------------------------------------------------------------------------------

# ##' @title getExperimentDesign
# ##'
# ##' @description retrieves the informations for one experiment
# ##' @param uri URI of the experiment
# ##' @return WSResponse object
# ##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
# ##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
# ##' service
# ##' @seealso You have to install the opensilexWSClientR before running any 
# ##'          request on OpenSILEX web service.
# ##' @examples
# ##' \donttest{
# ##'  connectToOpenSILEXWS( 
# ##'                  username = "guestopensilex.org",
# ##'                   password = "guest",
# ##'           url = "http:/localhost:8666/rest")
# ##'  Exp1<-getExperiment(
# ##'         experimentURI ="test-expe:test-serre")
# ##'  Exp1$data
# ##' }
# ##' @export
# getExperimentDesign <- function( uri = NULL){
#   if (is.null(uri) || uri == "" ){
#     stop("no uri provided")
#   } else {
#     
#     operations <- opensilexWSClientR::getOperations()
#     searchByExp <- operations$getExperiment(uri = uri)
#     resultByExp <- opensilexWSClientR::getDataAndMetadataFromResponse(searchByExp)  
#  
#     return(resultByExp)
#   }
# }

#' @title getVariablesByExperiment
##'
##' @description retrieves the informations variables used in one xperiment
##' @param uri URI of the experiment
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
##' service
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on OpenSILEX web service.
##' @examples
##' \donttest{
##'  connectToOpenSILEXWS( 
##'                  username = "guestopensilex.org",
##'                   password = "guest",
##'           url = "http:/localhost:8666/rest")
##'  ExpVar<-getExperiment(
##'         experimentURI ="test-expe:test-serre")
##'  ExpVar$data
##' }
##' @export
getVariablesByExperiment <- function( uri = NULL){
  if (is.null(uri) || uri == "" ){
    stop("no uri provided")
  } else {
    
    operations <- opensilexWSClientR::getOperations()
   
    searchVariablesByExp <-  operations$getUsedVariables(uri = "test-expe:abt_2004")
    resultVariablesByExp <- opensilexWSClientR::getDataAndMetadataFromResponse(searchVariablesByExp)  
    
    transformedData = data.table::rbindlist(
      lapply(resultVariablesByExp$data, function(x) data.table(t(x))),
      fill = TRUE
    )
    resultVariablesByExp$data = transformedData
    
    return(resultVariablesByExp)
  }
}

##' @title retrieves the experiments from the web service
##'
##' @description Retrieves the available experiments and/or linked to a project
##' @param name character, search by the name of an experiment (optional)
##' @param year character, search by the year of the experiment (optional)
##' @param is_ended boolean, search to the end date of experiment (optional)
##' @param species list, search experiment by list of species uri(s) (optional)
##' @param factors list, search experiment by list of factor uri(s) (optional)
##' @param projects list, search experiment by list of projects uri (optional)
##' @param is_public boolean, search experiment public status (optional)
##' @param order_by character, search by the place of an experiment (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param page_size numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
##' service
##' @import data.table
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on OpenSILEX web service.
##' @examples
##' \donttest{
##'  connectToOpenSILEXWS( 
##'                  username = "guestopensilex.org",
##'                   password = "guest",
##'    url = "http:/localhost:8666/rest")
##'  expes <- getExperiments()
##'  expes$data
##' }
##' @export
getExperiments <- function(name = "", year = "",is_ended = NULL,species = list(),factors = list(),projects = list(),is_public = NULL, order_by = "name=asc",page = NULL,page_size = NULL){
  # set Page
  if(is.null(page)){
    page  <- get("DEFAULT_PAGE", opensilexWSClientR:::configWS)
  }
  
  # set pageSize
  if(is.null(page_size)){
    page_size <- get("DEFAULT_PAGESIZE",  opensilexWSClientR:::configWS)
  }
  
  operations <- opensilexWSClientR::getOperations()
  searchExp <- operations$searchExperiments(name =name , year =year ,is_ended = is_ended,species = species,factors = factors,projects = 
                                          projects,is_public = is_public,order_by = order_by,page = page,page_size = page_size)
  resultExp <- opensilexWSClientR::getDataAndMetadataFromResponse(searchExp)  
  
  transformedData = data.table::rbindlist(
    lapply(resultExp$data, function(x) data.table(t(x))),
    fill = TRUE
  )
  resultExp$data = transformedData
    
  return(resultExp)
}
 