#-------------------------------------------------------------------------------
# Program: zzz.R
# Objective: defines a public configuration for the web service access in an 
#             environment
# Author: I. Sanchez
# Creation: 19/03/2018
# Update: 06/09/2019
#-------------------------------------------------------------------------------

## title defines a public configuration for the web service access
##
## description defines a public configuration for the web service access
## param libname character
## param pkgname character
## examples
## # not run
## keywords internal
# http://147.100.179.156:8080/phenomeapi/
#   configWS <- list(
#                    EXPERIMENT = "experiments",
#                    VARIABLES = "variables",
#                    ENVIRONMENT = "environment",
#                    PROJECTS = "projects",
#                    PLANTS = "plants",
#                    IMAGESANALYSIS = "imagesAnalysis",
#                    PHENOTYPES = "phenotypes",
#                    WATERING = "watering")

# Define an environment for the phenomeapi configuration
configWS<-new.env(emptyenv())

.onLoad <- function(libname, pkgname){
  
  # connectToPHIS parameters
  assign("WS_1_PUBLIC_PATH","http://147.100.179.156:8080/phenomeapi/resources/", configWS)
  assign("WS_1_PUBLIC_USERNAME","guestopensilex.org", configWS)
  assign("WS_1_PUBLIC_PASSWORD","guest", configWS)
  
  assign("WS_2_PUBLIC_PATH","http://www.opensilex.org/openSilexAPI/rest/", configWS)
  assign("WS_2_PUBLIC_USERNAME","guest@opensilex.org", configWS)
  assign("WS_2_PUBLIC_PASSWORD","guest", configWS)
  
  # WS phis1
  assign("EXPERIMENT", "experiments", configWS)
  assign("VARIABLES", "variables", configWS)
  assign("PLANTS", "plants", configWS)
  assign("IMAGESANALYSIS", "imagesAnalysis", configWS)
  assign("PHENOTYPES", "phenotypes", configWS)
  assign("WATERING", "watering", configWS)

  # WS phis2
  assign("AGROOBJECTS", "agronomicalObjects", configWS)
  assign("DATASETS", "datasets", configWS)
  assign("DATASEARCH", "data/search", configWS)
  assign("DATA", "data", configWS)
  assign("ANNOTATIONS", "annotations", configWS)
  assign("ENVIRONMENTS", "environments", configWS)
  assign("SCIENTIFIC_OBJECTS", "scientificObjects", configWS)
  assign("SENSORS", "sensors", configWS)
  assign("VECTORS", "vectors", configWS)
  assign("SPECIES", "species", configWS)
  assign("TRAITS", "traits", configWS)
  assign("UNITS", "units", configWS)
  assign("EVENTS", "events", configWS)
  assign("EXPERIMENTS", "experiments", configWS)
  assign("INFRASTRUCTURES", "infrastructures", configWS)
  assign("IMAGES", "images", configWS)
  assign("RADIOMETRIC_TARGETS", "radiometricTargets", configWS)
  assign("METHODS", "methods", configWS)
  assign("PROVENANCES", "provenances", configWS)
  assign("URI", "uri", configWS)
  
  # commun
  assign("VARIABLES", "variables", configWS)
  assign("VOCABULARY", "vocabularies/namespaces", configWS)
  assign("VARIABLES_DETAILS", "variables/details", configWS)
  assign("ENVIRONMENT", "environment", configWS)
  assign("PROJECTS", "projects", configWS)
  assign("RECONNECT_ON_DISCONNECTION", TRUE, configWS)
}
