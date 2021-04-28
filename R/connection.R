#-------------------------------------------------------------------------------
# Program: connection.R
# Objective: functions to facilitate connection on OpenSILEX web service
# Author: A. Charleroy
# Creation: 27/04/2021
# Update: 27/04/2021 (A.Charleroy)
#-------------------------------------------------------------------------------


##' @title connectToOpenSILEX 
##' @param url character, if apiID is private add the url of the chosen API,
##'   containing the IP, the full url with the protocol. e.g.
##'   'http://www.opensilex.org/rest/'
##' @param identifier e-mail of the user to create the token
##' @param password password of the user to create the token
##' @import opensilexWSClientR
##' @description load name space and connexion parameters of the webservice.
##'   Execute only once at the beginning of the requests. In the case of a
##'   WebService change of address or a renaming of services, please edit this
##'   list. and execute the function. Demonstration instances :
##'   \describe{
##'   connectToOpenSILEX(identifier="guest@opensilex.org",password="guest", url
##'   = "http://www.opensilex.org/openSilexAPI/rest/") }
##' @export
connectToOpenSILEX<-function( identifier = NULL, password = NULL, url = NULL){
  
  # configWS is an environment with specific variables to opensilex web service
  # if apiID is private, we use the url given by the user
   
    if (is.null(identifier)) {
      stop("Please, give an username")
    }
    if (is.null(password)) {
      stop("Please, give an user password")
    }  
    if(is.null(url)){
      stop("Please, give an OpenSILEX WS full URL")
    } 
  opensilexWSClientR::connectToOpenSILEXWS(username = identifier,
                                           password = password,
                                           url = url)

} 