#-------------------------------------------------------------------------------
# Program: data.R
# Objective: functions to add or retreive data
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq
# Creation: 24/05/2021
# Update: 
#-------------------------------------------------------------------------------

##' @title addSensorDataByCSV
##' @description Add sensor data from a formatted csv
##' @param file_path absolute csv file path
##' @param delimiter csv file delimiter
##' @param start_column csv start variable column number
##' @param provenance provenance uri
##' @param time_zone provenance uri
##' Datetime Column and variables column are mandatory
##' 
##' CSV example
##' ,Air_temperature_20min_instant,Air_temperature_20min_instant
##' ,Celsius_degree,Celsius_degree
##' ,Canopy_air_temperature,Canopy_air_temperature
##' Sensor,http://www.opensilex.org/sunagri/2020/s20031,http://www.opensilex.org/sunagri/2020/s20031
##' DateTime,http://www.opensilex.org/sunagri/id/variables/v082,http://www.opensilex.org/sunagri/id/variables/v083
##' 2019-05-20T15:40:00Z,19.971676,19.971676
##' 2019-05-20T16:00:00Z,20.224501,20.224501
##' \donttest{
##' connectToOpenSILEX( identifier="guest@opensilex.org",
##'               password="guest"
##'               url = "http://www.opensilex.org/openSilexAPI/rest/",)
##'   addSensorDataByCSV(
##'   start_column = 2,
##'   delimiter = ",",
##'   provenance = "demo:id/provenance/1594993768653",
##'    file_path = "/home/filpath/file.csv",
##'    time_zone = "Europe/Paris" 
##'   )
##' }
##' CSV example with Scientific object
##' 
##' ,Method,Air_temperature_20min_instant
##' ,Unit,Celsius_degree
##' ,Entity + characteristic,Canopy_air_temperature
##' ,Sensor,sunagri:2020/s20031
##' DateTime,Scientific_object;sunagri:id/variables/v151
##' 2019-05-20T15:40:00Z;sunagri:2019/o19064783;19.971676
##' 2019-05-20T16:00:00Z;sunagri:2019/o19064783;20.224501
##' 2019-05-20T16:20:00Z;sunagri:2019/o19064783;19.466026
##' 
##' \donttest{
##'connectToOpenSILEX( identifier="guest@opensilex.org",
##'               password="guest"
##'               url = "http://www.opensilex.org/openSilexAPI/rest/",)
##'   addSensorDataByCSV(
##'   start_column = 3,
##'   delimiter = ",",
##'   provenance = "demo:id/provenance/1594993768653",
##'    file_path = "/home/filpath/file.csv",
##'    time_zone = "Europe/Paris" 
##'   )
##' }
##' @import dplyr
##' @export
addSensorDataByCSV <- function(file_path= NULL,delimiter = ",",start_column = NULL,provenance= NULL,experiment=NULL,time_zone = "Europe/Paris"){
  if(is.null(file_path) && !is.character(file_path)){
    stop("file_path must not be null and must be a string")
  }
  if(is.null(start_column && !integer(start_column))){
    stop("start_column must not be null and must be an integer")
  }
  if(is.null(provenance) && !is.character(provenance)){
    stop("provenance must not be null and must be a string")
  }
  csv_metadata <- readr::read_delim(n_max = 4 , delim = delimiter, file =file_path)
  n_column = start_column
  csv_data <- readr::read_delim(skip = 4, delim = delimiter, file = file_path)
  deviceApi <- opensilexClientToolsR::DevicesApi$new()
  variablesApi <- opensilexClientToolsR::VariablesApi$new()
  
  data = list()
  has_next = TRUE
  logging::loginfo("preparing data ...")
  # test duplicate
  duplicatedData <- csv_data %>%  
    dplyr::group_by(DateTime) %>% 
    dplyr::mutate(dupe = dplyr::n()>1)  %>%    
    dplyr::filter(dupe  == TRUE)
  
  if(nrow(duplicatedData) != 0){
    head(duplicatedData)
    stop("Duplicated rows")
    return(duplicatedData)
  } 
  while(has_next){
    s = tryCatch({
      variable <- csv_metadata[4,n_column][[1]]
      sensor <- csv_metadata[3,n_column][[1]]
      n_column =  n_column +1
      
      if(is.null(sensor) || sensor == ""){
        logging::logerror(paste('sensor value',sensor))
        stop("Bad sensor value")
      }
      
      sensor_prov = opensilexClientToolsR::ProvEntityModel$new(
        rdf_type =  "vocabulary:SensingDevice",
        uri = sensor
      )
       prov <- opensilexClientToolsR::DataProvenanceModel$new(
         uri = provenance,
         prov_used = list(
           sensor_prov
         ),
         settings = opensilexClientToolsR::ObjectDTO$new()
       )   
       if(!is.null(experiment)){
         prov$experiments = list(experiment)
       }
       for (nrow in  1:nrow(csv_data) ) {   
         row = csv_data[nrow,]
         dataObject <- opensilexClientToolsR::DataCreationDTO$new( 
             provenance =  prov, 
             variable  = variable,
             date  = format(row$DateTime, format="%Y-%m-%dT%H:%M:%S%z",tz= time_zone),
             value =  ObjectDTO$new(as.numeric(row[`variable`][[1]])),
             metadata =  opensilexClientToolsR::ObjectDTO$new()
           )  
         if("Scientific_object" %in% names(row)){
           dataObject$scientific_object = row$Scientific_object
         }
         data <- append(data,dataObject) 
      }
    }
      , error= function(e){
        logging::loginfo(paste("last variable ",n_column, "columns"))
        return(FALSE)
    }) 
    if(!is.null(s) && s == FALSE){
      has_next = FALSE
    } 
  } 
  
  logging::loginfo(paste(length(data), "data found"))
  logging::loginfo("sending data ...")
  dataApi <- opensilexClientToolsR::DataApi$new()
  data_objects <- list()
  total_count = 0
  limit_to_send = 15000 
  logging::loginfo(paste(length(data), "data to add"))
  
  
  for( row_number in data){
    data_objects <- append(data_objects, row_number)
    if(length(data_objects) == limit_to_send){
      res <- dataApi$add_list_data(body = data_objects)
      if(res$success == FALSE){
        logging::logwarn("Data not send")
        print(res$data)
      }
      total_count = total_count +  length(data_objects)
      logging::loginfo(paste(total_count, "/",length(data), " data treated"))
      data_objects <- list() 
    }
  }
  
  if(length(data_objects) > 0 ){
    res <- dataApi$add_list_data(body = data_objects)
    if(res$success == FALSE){
      logging::logwarn("Data not send")
      print(res$data)
    }
    total_count = total_count +  length(data_objects)
    logging::loginfo(paste(total_count, "/",length(data), " data treated"))
  }
   
  logging::loginfo("Process ending - ", paste(length(data), "data"))
}

##'expData <- tibble::tibble(
##'     ScientificObject = c(
##'      "sunagri:SUA2011-1/so-test",
##'      "sunagri:SUA2011-1/so-test",
##'      "sunagri:SUA2011-1/so-test"),
##'     Date = c("2015-04-15", "2015-04-16", "2015-04-15T15:10:20+01:00"),
##'     Variable = c(
##'           "sunagri:id/variables/v148",
##'           "sunagri:id/variables/v148",
##'           "sunagri:id/variables/v148"
##'            ),
##'     Value = c(
##'      30.2,
##'      40.4,
##'      50.5
##'     )
##' )
##' @keywords  internal
##' @importFrom magrittr %>%
addDataByExperiment <- function(expData= NULL,experiment = NULL,provenance= NULL){
  if(is.null(expData)){
    stop("expData must not be null")
  }
  if(is.null(experiment)){
    stop("experiment must not be null")
  }
  if(is.null(provenance)){
    stop("provenance must not be null")
  }
  cols = c("ScientificObject","Variable","Date","Value")
  resultByColumn = tibble::has_name(expData,cols)
  result = all(resultByColumn == TRUE)
  if(!result){
    logging::loginfo(cols)
    logging::loginfo(resultByColumn)
    logging::logerror('Missing columns - "ScientificObject","Variable","Date","Value" columns expected')
    stop("Missing columns")
  } 
  
  # test duplicate
  duplicatedData <- expData %>%  
    dplyr::group_by(Date, ScientificObject,Variable) %>% 
    dplyr::mutate(dupe = dplyr::n()>1)  %>%    
    dplyr::filter(dupe  == TRUE)
  
  if(nrow(duplicatedData) != 0){
    head(duplicatedData)
    stop("Duplicated rows")
    return(duplicatedData)
  } 
  
  
  # variablesColNames = c(dplyr::distinct(expData, Variable))
  # expData <- expData %>% tidyr::spread(Variable,Value)
  # expData <- rbind(expData[1:2, ]  * NA, expData[1:5, ], expData[5:6, ])
  
  
  dataFormatted <- list()
  count = 0
  limit = 2
  
  dataApi <- DataApi$new() 
  
  for(numData in 1:nrow(expData)){
    prov <- opensilexClientToolsR::DataProvenanceModel$new(
      uri = provenance,
      experiments = list()
    )  
    dataObject <- opensilexClientToolsR::DataCreationDTO$new( 
             provenance =  prov,
             scientific_objects = expData[numData,]$ScientificObject,
             variable  = expData[numData,]$Variable,
             date  = expData[numData,]$Date,
             value =  ObjectDTO$new(expData[numData,]$Value),
             metadata =  opensilexClientToolsR::ObjectDTO$new()
           )  
    dataFormatted <- append(dataFormatted,dataObject)
    count = count +1 
    if(count == limit){ 
      response <- dataApi$add_list_data(body =dataFormatted)
      count=0
      dataFormatted =  list()
    }
  }   
  if(length(dataFormatted) > 0){
    count=0
    response <- dataApi$add_list_data(body = dataFormatted)
    response$data
  }
  # insert R6 object 
  stop()
}
#   
#   
#   expData %>% dplyr::add_row( list(tibble::tib(),tibble::tibble_row()),  .after = 0) 
#   logging::loginfo("Transform dataset in vertical way")
#   dataSunagriTransformed <- melt(as.data.table(expData, stringsAsFactors=FALSE), id.vars = c("ObjectId", "Date"),
#                                  measure.vars = c("Variable"))
#   
#   logging::loginfo("get variables from webservice")
#   variable_api <- opensilexClientToolsR::VariablesApi$new()
#   data <- variable_api$get_variables_by_ur_is
#   viewVar <- tibble::tibble(uri = character(),label = character())
#   for (vars in variable$data) {
#     viewVar <- dplyr::bind_rows(viewVar,tibble::tibble(uri= vars$uri, label = vars$label))
#   }
#   
#   
#   logging::loginfo("get provenances from webservice")
#   prov <- phisWSClientR::getProvenances(uri = provenanceUri)$data
#   if(is.null(prov)){
#     logging::logerror(paste("Unknown Provenance",provenance))
#     stop()
#   } 
#   provenanceUri <- prov$uri
#   
#   dataSunagriNames <-names(dataSunagri %>% select(c("ScientificObjectURI", "Date")))
#   if(length(dataSunagriNames) != 2){
#     logging::logerror("Missing ScientificObjectURI or Date column")
#     stop()
#   }
#   
#   logging::loginfo("Check variables")
#   variablesColNames <- names(dataSunagri %>% select(-c("ScientificObjectURI", "Date")))
#   for (variablesColName in 1:length(variablesColNames)) {
#     var <- viewVar %>% filter(label == variablesColNames[[variablesColName]])
#     # checkvariable
#     if(is.null(var) || is.null(var$uri) || is.na(var$uri)){
#       logging::logerror(paste("Unknown variable",variablesColNames[[variablesColName]]))
#       stop()
#     }
#   }
#   
#   logging::loginfo("Transform dataset in vertical way")
#   dataSunagriTransformed <- melt(as.data.table(dataSunagri, stringsAsFactors=FALSE), id.vars = c("ScientificObjectURI", "Date"),
#                                  measure.vars = variablesColNames)
#   variablesOldName <- viewVar$label
#   variablesNewName <- viewVar$uri
#   dataSunagriTransformed[, variable := factor(variable, levels = variablesOldName, labels = variablesNewName)]
#   dataSunagriTransformed[, variable:=as.character(variable)]
#   dataSunagriTransformed[, provenanceUri:=provenanceUri]
#   dataSunagriTransformed[, Date:=format(Date, "%Y-%m-%dT%H:%M:%S%z")]
#   dataSunagriTransformed[ , `:=`(objectUri = ScientificObjectURI, ScientificObjectURI = NULL)]
#   dataSunagriTransformed[ , `:=`(date = Date, Date = NULL)]
#   dataSunagriTransformed[ , `:=`(variableUri = variable, variable = NULL)]
#   
#   count = 0
#   dataDF <- data.frame(stringsAsFactors = FALSE)
#   total <- nrow(dataSunagriTransformed)
#   batchNumber <- 100
#   pb <- progress::progress_bar$new(total = total)
#   
#   futureList <- list()
#   for (variable in 1:nrow(dataSunagriTransformed)) {
#     if(count == batchNumber){
#       count = 0
#       futureTask <- future::future({
#         phisWSClientR::connectToPHISWS(apiID="ws_private",username=username,password=password, url = webServiceAPI)
#         results <- phisWSClientR::po&stData(dataDF)
#         print(results$metadata$status$message)
#         if(results$success == FALSE){
#           logging::logerror(results$metadata$status$message)
#         }
#       })
#       futureList = append(futureList, futureTask)
#       dataDF <- data.frame(stringsAsFactors = FALSE)
#     }else{
#       dataDF <- rbind(dataDF,dataSunagriTransformed[variable,])
#     }
#     count = count + 1
#     pb$tick()
#   }
#   
#   if(count != 0){
#     futureTask <- future::future({
#       results <- postData(dataDF)
#       if(results$success == FALSE){
#         logging::logerror(results$metadata$status$message)
#       }
#     })
#     futureList = append(futureList, futureTask)
#   }
#   results <- future::values(futureList)
# }
# 
# is.validateDate <- function(x){
#   date <- strptime(x, "%Y-%m-%dT%H:%M:%SZ") 
#   if(!is.na(date)){
#     return(TRUE)
#   }else{
#     date <- strptime(x, "%Y-%m-%d") 
#     if(!is.na(date)){
#       return(TRUE)
#     }
#   }
#   return(FALSE)
# }

