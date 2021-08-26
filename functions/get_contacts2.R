library(httr)
library(jsonlite)
library(tidyverse)


get_contacts2 <- function() {
  
  #Check version of Go.Data
  gd.version <- get_godata_version() %>%
    as.data.frame()
  names(gd.version) <- "version"
  gd.version <- gd.version %>%
    select(version) %>%
    separate(version,c("a","b","c")) %>%
    mutate_all(as.numeric)
  
  if (gd.version$a < 2 | gd.version$b < 38 | gd.version$c < 1) {
    stop("Go.Data must be version 2.38.1 or later. Please use the function get_cases() instead.")
  }
  
  
  #Submit an export request to the system
  export.request <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts/export",
                               "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22dontTranslateValues%22%3A%22true%22%2C%20%22jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
                               "&access_token=",get_access_token()))
  if (export.request$status_code!=200) {
    stop(paste0('Error code: ', export.request$status_code))
  } else if (export.request$status_code==200) {
    
    #Get the Request ID
    export.request.id <- export.request %>%
      content("text") %>% 
      fromJSON(flatten=TRUE) %>% 
      unlist() %>% 
      unname()
    
    message(paste0("Export Request ID: ", export.request.id))
    
    #Check status of request
    Sys.sleep(1) # wait 1 second before checking status
    export.request.status <- GET(paste0(url,"api/export-logs/",export.request.id,"?access_token=",get_access_token())) %>%
      content("text") %>% fromJSON(flatten=TRUE)
    
    message(paste0("Export Request Status: ",export.request.status$statusStep))
    
    #Keep checking status every 3 seconds until it is finished
    if (export.request.status$statusStep != "LNG_STATUS_STEP_EXPORT_FINISHED") {
      Sys.sleep(3)
      export.request.status <- GET(paste0(url,"api/export-logs/",export.request.id,"?access_token=",get_access_token())) %>%
        content("text") %>% 
        fromJSON(flatten=TRUE)
      message(paste0("Export Request Status: ",export.request.status$statusStep))
    }
    
    #If the status is finished, then download the export
    if (export.request.status$statusStep == "LNG_STATUS_STEP_EXPORT_FINISHED") {
      df <- GET(paste0(url,"api/export-logs/",export.request.id,"/download?access_token=",get_access_token())) %>%
        content("text") %>% 
        fromJSON(flatten=TRUE)
    }
    
  }
  names(df)[names(df) %in% "_id"] <- "id" # fix one strange variable name
  return(df)
}
