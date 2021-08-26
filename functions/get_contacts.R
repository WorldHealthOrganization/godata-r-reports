library(httr)
library(jsonlite)
library(tidyverse)


get_contacts <- function() {
  
  #Check version of Go.Data
  gd.version <- get_godata_version() %>%
    as.data.frame()
  names(gd.version) <- "version"
  gd.version <- gd.version %>%
    select(version) %>%
    separate(version,c("a","b","c")) %>%
    mutate_all(as.numeric)
  
  if (gd.version$a >= 2 & gd.version$b >= 38 & gd.version$c >= 1) {
    warning("Your version of Go.Data is 2.38.1 or later. Consider using the function get_cases2().")
  }
  
  
  #get total number of contacts
  contacts_n <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts/filtered-count"), 
                    add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
    content(as="text") %>% fromJSON(flatten=TRUE) %>% unlist() %>% unname()
  
  #import contacts in batches
  contacts <- tibble()
  batch_size <- 50000 # number of records to import per iteration
  skip <- 0
  while (skip < contacts_n) {
    message("********************************")
    message(paste0("Importing records ", format(skip+1, scientific = FALSE), " to ", format(skip+batch_size, scientific = FALSE)))
    contacts.i <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts",
                             "/?filter={%22limit%22:",format(batch_size, scientific = FALSE),",%22skip%22:",format(skip, scientific = FALSE),"}"), 
                      add_headers(Authorization = paste("Bearer", get_access_token(), sep = " "))) %>%
      content(as='text') %>%
      fromJSON( flatten=TRUE) %>%
      as_tibble()
    contacts <- contacts %>% bind_rows(contacts.i)
    skip <- skip + batch_size
    message(paste0("Imported ", format(nrow(contacts.i), scientific = FALSE)," records"))
    message(paste0("Data Frame now has ", format(nrow(contacts), scientific = FALSE), " records"))
    rm(contacts.i)
  }
  df <- contacts
  return(df)
}
