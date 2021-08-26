library(httr)
library(jsonlite)
library(tidyverse)

get_godata_version <- function() {
  
  version.request <- GET(paste0(url,"api/system-settings/version")) %>%
    content("text") %>%
    fromJSON(flatten=TRUE)
  
  return(version.request$version)
  
}
