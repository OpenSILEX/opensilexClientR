# #-------------------------------------------------------------------------------
# # Program: getData.R
# # Objective: functions to get the Data service from WS2
# #            * getData
# # Authors: Hollebecq Jean-Eudes, Isabelle Sanchez
# # Creation: 12/03/2019
# #-------------------------------------------------------------------------------
# 
# ##' @title getData
# ##'
# ##' @description Retrieves the data from the web service
# ##' @param variableUri character, search by the uri of a variable (NOT optional). You can access the list of variables through \code{\link{getVariables2}} function.
# ##' @param startDate character, search from start date (optional)
# ##' @param endDate character, search to end date (optional)
# ##' @param objectUri character, search by object uri
# ##' @param objectLabel character, search by object label
# ##' @param provenanceUri character, search by provenance uri
# ##' @param provenanceLabel character, search by provenance label
# ##' @param page numeric, displayed page (pagination Plant Breeding API)
# ##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
# ##' @return WSResponse object
# ##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
# ##' @seealso You have to install the opensilexWSClientR before running any 
# ##'          request on Opensilex web service.
# ##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
# ##' service
# ##' @examples
# ##' \donttest{
# ##' connectToOpenSILEXWS(
# ##'                url = "http://www.opensilex.org/openSilexAPI/rest/",
# ##'                username="guest@opensilex.org",
# ##'                password="guest")
# ##'  vars<-getVariables2()$data$uri
# ##'  totalCount <- getData(variableUri = vars[4])$totalCount
# ##'  data <- getData(variableUri = vars[4],pageSize = totalCount)
# ##'  data <- getData(variableUri = vars[5],
# ##'                  startDate = "2017-05-01",
# ##'                  endDate = "2017-06-01",
# ##'                  pageSize = totalCount)
# ##'  str(data$data)
# ##' }
# ##' @export
# getData <- function(variableUri = "",
#                     objectUri = "",
#                     objectLabel = "",
#                     provenanceUri = "",
#                     provenanceLabel  = "",
#                     startDate = "",
#                     endDate = "",
#                     page = NULL,
#                     pageSize = NULL){
#   
#   if(is.factor(variableUri)) variableUri <- as.character(variableUri)
#   attributes <- list(pageSize=pageSize,  page = page)
#   if (variableUri!="")     attributes <- c(attributes, variableUri = variableUri)
#   if (objectUri!="")       attributes <- c(attributes, objectUri = objectUri)
#   if (objectLabel!="")     attributes <- c(attributes, objectLabel = objectLabel)
#   if (startDate!="")       attributes <- c(attributes, startDate = startDate)
#   if (endDate!="")         attributes <- c(attributes, endDate = endDate)
#   if (provenanceUri!="")   attributes <- c(attributes, provenanceUri = provenanceUri)
#   if (provenanceLabel!="") attributes <- c(attributes, provenanceLabel = provenanceLabel)
#   
#   variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("DATASEARCH", configWS)),
#                                          attributes = attributes, wsVersion = 2)
#   
#   return(variableResponse)
# }