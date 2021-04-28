# #-------------------------------------------------------------------------------
# # Program: postAnnotations.R
# # Objective: functions to post a new annotation to the WS2
# #            * postAnnotations
# # Authors: Hollebecq Jean-Eudes
# # Creation: 11/12/2019
# # Update: 04/06/2020
# #-------------------------------------------------------------------------------
#  
# 
# #-------------------------------------------------------------------------------
# # Program: postAnnotations.R
# # Objective: functions to post a new annotation to the WS2
# #            * postAnnotaions
# # Authors: Arnaud Charleroy
# # Creation: 04/06/2020
# # Update:
# #-------------------------------------------------------------------------------
# 
##' @title postAnnotations
##'
##' @description send a list of annotation to the web service
##' @import opensilexClientToolsR
##' @param newAnnotation AnnotationCreationDTO
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToOpenSILEXWS(
##'                url = "http://www.opensilex.org/rest/",
##'                username="guest@opensilex.org",
##'                password="guest")
##'
##'newAnnotation <- AnnotationCreationDTO$new(
##'  targets = list("http://www.phenome-fppn.fr/test/2019/o19000074"),
##'  motivation  = "http://www.w3.org/ns/oa#describing",
##'  description =  "the object has been observed"
##')
##'
##'  response <- postAnnotations(newAnnotation)
##'  response$success
##'  response$metadata
##'  }
##' @export
createAnnotation <- function(newAnnotation){
  annoService <- opensilexClientToolsR::AnnotationsApi$new()
  Response <- annoService$create_annotation( newAnnotation)
  return(Response)
}