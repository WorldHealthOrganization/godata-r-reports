library(httr)
library(jsonlite)
library(tidyverse)

get_access_token <- function() {
  response <- POST(url=paste0(url,"api/oauth/token?access_token=123"),  
                   body = list(username = username,password = password),                                       
                   encode = "json") %>%
    content(as = "text") %>%
    fromJSON(flatten = TRUE)
  return(response$access_token)
}
