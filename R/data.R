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
##'               url = "http://www.opensilex.org/openSilexAPI/rest",)
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
##' ;Method;Air_temperature_20min_instant
##' ;Unit,Celsius_degree
##' ;Entity + characteristic;Canopy_air_temperature
##' ;Sensor;sunagri:2020/s20031
##' DateTime;Scientific_object;sunagri:id/variables/v151
##' 2019-05-20T15:40:00Z;sunagri:2019/o19064783;19.971676
##' 2019-05-20T16:00:00Z;sunagri:2019/o19064783;20.224501
##' 2019-05-20T16:20:00Z;sunagri:2019/o19064783;19.466026
##' 
##' \donttest{
##'connectToOpenSILEX( identifier="guest@opensilex.org",
##'               password="guest"
##'               url = "http://www.opensilex.org/openSilexAPI/rest",)
##'   addSensorDataByCSV(
##'   start_column = 3,
##'   delimiter = ",",
##'   provenance = "demo:id/provenance/1594993768653",
##'    file_path = "/home/filpath/file.csv",
##'    time_zone = "Europe/Paris" 
##'   )
##' }
##' @import dplyr
##' @import readr
##' @importFrom magrittr %>%
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
  data <- readr::read_delim(skip = 4, delim = delimiter, file = file_path)
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

##' @title addDataByDataFrame
##' @description Add  data from a tibble
##' @param data data table
##' @param provenance provenance uri
##' @param time_zone provenance uri
##' "Variable","Date","Value" column are mandatory 
##' A column Sensor or ScientificObject is required
##' Both can be set
##' 
##' You can use as_tibble function
##' Data example: 
##' A tibble: 2 x 5
##'Date   ScientificObject       Variable          Value Sensor         
##'<chr>      <chr>                  <chr>             <dbl> <chr>          
##' 2019-05-2… http://www.opensilex.… http://sinfonia.…  20   http://sinfoni…
##' 2019-05-2… http://www.opensilex.… http://sinfonia.…  19.5 http://sinfoni…
##' Dataframe example with Scientific_object
##' \donttest{
##'connectToOpenSILEX( identifier="guest@opensilex.org",
##'               password="guest"
##'               url = "http://www.opensilex.org/openSilexAPI/rest")
##'   data = tibble::tribble(
##'    ~Date, ~ScientificObject,  ~Variable, ~Value,~Sensor,
##'   "2019-05-20 17:40:00", "http://www.opensilex.org/demo/DIA2017-1/so-test","http://www.opensilex.org/demo/set/variables#variable.t_t_t",  20.0, "http://www.opensilex.org/demo/set/devices/humidity-sensor-test",
##'   "2019-05-20 17:00:00", "http://www.opensilex.org/demo/DIA2017-1/so-test","http://www.opensilex.org/demo/set/variables#variable.t_t_t",  19.5, "http://www.opensilex.org/demo/set/devices/humidity-sensor-test"
##'   )
##'   addDataByDataFrame(
##'      data = data,
##'      provenance = "demo:id/provenance/1594993768653", 
##'      experiment= "demo:exp/sensor/s100000",
##'    time_zone = "Europe/Paris" 
##'   ) 
##' }
##' @import dplyr
##' @import readr
##' @importFrom magrittr %>%
##' @export
addDataByDataFrame <- function(data= NULL,  provenance= NULL, 
                               experiment=NULL, time_zone = "Europe/Paris"){
  
  if(is.null(data) && nrow(data) == 0){
    stop("data must not be null and must not be empty")
  }
  if(is.null(experiment) && !is.character(experiment)){
    logging::logwarn("experiment not set - be careful data will be available globally in the system")
    logging::logwarn("In 5 second the process will continue")
    Sys.sleep(5)
  }
  if(is.null(provenance) && !is.character(provenance)){
    stop("provenance must not be null and must be a string")
  }  
  logging::loginfo("preparing data ...")
  
  cols = c("Variable","Date","Value")
  resultByColumn = tibble::has_name(data,cols)
 result = all(resultByColumn == TRUE)
  if(!result){
    logging::logerror('Missing columns - "Variable","Date","Value" columns are mandatory')
    stop("Missing columns")
  }else{
    if( !tibble::has_name(data,"Sensor") &&  !tibble::has_name(data,"ScientificObject")){
      stop("Missing columns - Sensor or ScientificObject to link the data")
    }
    resultBySOColumn = tibble::has_name(data,"ScientificObject")
    if(resultBySOColumn){
      # test duplicate
      duplicatedData <- data %>%  
        dplyr::group_by(Date,Variable,ScientificObject) %>% 
        dplyr::mutate(dupe = dplyr::n()>1)  %>%    
        dplyr::filter(dupe  == TRUE)
      
      if(nrow(duplicatedData) != 0){
        head(duplicatedData)
        stop("Duplicated rows")
        return(duplicatedData)
      } 
      
    }else{
      # test duplicate
      duplicatedData <- data %>%  
        dplyr::group_by(Date,Variable) %>% 
        dplyr::mutate(dupe = dplyr::n()>1)  %>%    
        dplyr::filter(dupe  == TRUE)
      
      if(nrow(duplicatedData) != 0){
        head(duplicatedData)
        stop("Duplicated rows")
        return(duplicatedData)
      } 
    }
  } 
  logging::loginfo("No duplicated rows")
  
  dataObjects <- list()
  for (nrow in  1:nrow(data) ) { 
    row = data[nrow,]
    data_prov = NULL
    if(!is.null(row$Sensor)){
      sensor_prov <- opensilexClientToolsR::ProvEntityModel$new(
        rdf_type =  "vocabulary:SensingDevice",
        uri = row$Sensor
      )
      data_prov <- opensilexClientToolsR::DataProvenanceModel$new(
        uri = provenance,
        prov_used = list(
          sensor_prov
        ),
        settings = opensilexClientToolsR::ObjectDTO$new()
      )   
    }else{
      data_prov <- opensilexClientToolsR::DataProvenanceModel$new(
        uri = provenance, 
        settings = opensilexClientToolsR::ObjectDTO$new()
      )  
    } 
    
    if(!is.null(experiment)){
      data_prov$experiments = list(experiment)
    }
    
    datetime = NULL
    isDate = FALSE
    tryCatch({
      datetime = readr::parse_date(row$Date)
      isDate = TRUE
    },
    warning=function(cond){
      return(FALSE)
    },
    error = function(cond){
      return(FALSE)
    }) 
    if(!isDate){
      isDateTime = FALSE
      tryCatch(
        expr = {
          datetime = readr::parse_datetime(row$Date)
          isDateTime = TRUE
        },
        warning=function(cond){
          return(FALSE)
        },
        error = function(cond){
          return(FALSE)
        })
      if(!isDateTime){
        stop("Error dateformat")
      }
    } 
    
    if(is.null(datetime)){
      stop("Wrong date time")
    }
    SO = NULL
    
    tryCatch(
      expr = {
        if(!is.null(row$ScientificObject)){
          SO = row$ScientificObject
        } 
      },
      warning=function(cond){
        return(FALSE)
      },
      error = function(cond){
        return(FALSE)
      })
    
    
    dataObject <- opensilexClientToolsR::DataCreationDTO$new( 
      provenance =  data_prov, 
      variable  = row$Variable,
      date  = format(datetime, format="%Y-%m-%dT%H:%M:%S%z",tz= time_zone),
      value =  ObjectDTO$new(row$Value),
      metadata =  opensilexClientToolsR::ObjectDTO$new()
    )
    if(!is.null(SO)){
      dataObject$scientific_object = SO
    }
    dataObjects <- append(dataObjects,dataObject) 
  }  
  
  logging::loginfo(paste(length(dataObjects), "data found"))
  logging::loginfo("sending data ...")
  dataApi <- opensilexClientToolsR::DataApi$new()
  data_objects <- list()
  total_count = 0
  limit_to_send = 15000 
  logging::loginfo(paste(length(dataObjects), " observations to send"))
  
  
  for( row_number in dataObjects){
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
      if(!is.null(res$data$result) && res$data$result$title == "DUPLICATE_DATA_KEY"){
        logging::logwarn("Data slice has already been sent")
      }else{
        print(res$data) 
        stop("Data not send") 
      } 
    }
    total_count = total_count +  length(data_objects)
    logging::loginfo(paste(total_count, "/",length(dataObjects), " data treated"))
  }
  
  logging::loginfo("Process ending ")
}
 