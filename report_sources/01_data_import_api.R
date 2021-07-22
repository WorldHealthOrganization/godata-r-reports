###################################################################################################

url <- "https://godata-r13.who.int/"                   # <--------------------- insert instance url here, don't forget the slash at end !
username <- "godata_api@who.int"                           # <--------------------- insert your username for signing into Go.Data webapp here
password <- "xxxxxxxxxxxxxxxxxxx"                           # <--------------------- insert your password for signing into Go.Data webapp here
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"   # <--------------- insert your outbreak ID here! (find it in URL when you have selected outbreak)

###################################################################################################

# SCRIPT TO PULL IN COLLECTIONS ACROSS ANY GO.DATA INSTANCE #
# updated 21 July 2021

###################################################################################################
# read in from Go.Data API, using your updated log-in credentials by Clicking "Source"
# no need to modify the below unless you would like to bring in additional API endpoints used in the dashboards in webapp, etc!
# Script authored and maintained by Go.Data team (godata@who.int)
###################################################################################################

# this script currently returns: 
#                                     cases, 
#                                     contacts,
#                                     contacts of contacts,
#                                     events
#                                     follow ups, 
#                                     lab results,
#                                     locations,
#                                     relationships,
#                                     teams,
#                                     users

###################################################################################################
# source required scripts, including packages that need to be installed
#       this includes set_core_fields.R script, which ensures that collections have all the columns they need and set to NA those that don't exist
#       otherwise, the JSON drops it if these questions were consistently not answered, which can break the scripts if its a core variable
###################################################################################################
if (!require("here")) install.packages("here")
source(here::here("scripts", "aaa_load_packages.R"))
#source(here::here("scripts", "set_core_fields.R"))

###################################################################################################
# GET ACCESS TOKEN
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token                 ## this is your access token !!! that allows API calls

###################################################################################################
# GET CASES
###################################################################################################

response_cases <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases"), 
                      add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_cases <- content(response_cases, as = "text")
cases <- as_tibble(fromJSON(json_cases, flatten = TRUE))
rm(response_cases)


###################################################################################################
# RE-FETCH ACCESS TOKEN IN CASE TIME-0UT, GET CONTACTS
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token

##################################################################################################

# import oubtreak Contacts 
response_contacts <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts"), 
                         add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_contacts <- content(response_contacts, as = "text")
contacts <- as_tibble(fromJSON(json_contacts, flatten = TRUE))
rm(response_contacts)

###################################################################################################
# RE-FETCH ACCESS TOKEN IN CASE TIME-0UT, GET FOLLOWUPS
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token

##################################################################################################

# import contact follow-ups (could filter last 21 days only to avoid system time-out)
response_followups <- GET(paste0(
  url,
  "api/outbreaks/",
  outbreak_id,
  "/follow-ups"
  # /?filter={%22where%22:{%22and%22:[{%22date%22:{%22between%22:[%22",
  # date_21d_ago,
  # "%22,%22",
  # date_now,
  # "%22]}}]}}"
    ),
  add_headers(Authorization = paste("Bearer", access_token, sep = " ")))
json_followups <- content(response_followups, as="text")
followups <- as_tibble(fromJSON(json_followups, flatten = TRUE)) 
rm(response_followups)

###################################################################################################
# RE-FETCH ACCESS TOKEN IN CASE TIME-0UT, GET EVENTS
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token

##################################################################################################

# import oubtreak Events 
response_events <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/events"), 
                         add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_events <- content(response_events, as = "text")
events <- as_tibble(fromJSON(json_events, flatten = TRUE))
rm(response_events)

###################################################################################################
# RE-FETCH ACCESS TOKEN IN CASE TIME-0UT, GET CONTACTS OF CONTACTS
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token

##################################################################################################

# import oubtreak Contact of Contacts 
response_contacts_of_contacts <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts-of-contacts"), 
                       add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_contacts_of_contacts <- content(response_contacts_of_contacts, as = "text")
contacts_of_contacts <- as_tibble(fromJSON(json_contacts_of_contacts, flatten = TRUE))
rm(response_contacts_of_contacts)

###################################################################################################
# RE-FETCH ACCESS TOKEN IN CASE TIME-0UT, GET LAB RESULTS
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token

##################################################################################################

# import oubtreak Lab Results 
response_lab_results <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/lab-results/aggregate"), 
                                     add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_lab_results <- content(response_lab_results, as = "text")
lab_results <- as_tibble(fromJSON(json_lab_results, flatten = TRUE))
rm(response_lab_results)

###################################################################################################
# RE-FETCH ACCESS TOKEN IN CASE TIME-0UT, GET RELATIONSHIPS
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token

##################################################################################################

# import outbreak Relationships
response_relationships <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/relationships/export"), 
                              add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_relationships <- content(response_relationships, as = "text")
relationships <- as_tibble(fromJSON(json_relationships, flatten = TRUE))
rm(response_relationships)

###################################################################################################
# RE-FETCH ACCESS TOKEN IN CASE TIME-0UT, GET LOCATIONS
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token

##################################################################################################

# import location hierarchy (outbreak agnostic)
response_locations <- GET(paste0(url,"api/locations"), 
                          add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_locations <- content(response_locations, as = "text")
locations <- as_tibble(fromJSON(json_locations, flatten = TRUE))
rm(response_locations)

###################################################################################################
# RE-FETCH ACCESS TOKEN IN CASE TIME-0UT, GET TEAMS
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token

##################################################################################################

# import Teams (outbreak agnostic)
response_teams <- GET(paste0(url,"api/teams"), 
                      add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_teams <- content(response_teams, as = "text")
teams <- as_tibble(fromJSON(json_teams, flatten = TRUE))
rm(response_teams)

###################################################################################################
# RE-FETCH ACCESS TOKEN IN CASE TIME-0UT, GET USERS
###################################################################################################

url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,  
                 body = list(
                   username = username,                                          
                   password = password),                                       
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  glimpse()

access_token <- content$access_token

##################################################################################################
# import Users (outbreak agnostic)
response_users <- GET(paste0(url,"api/users"), 
                      add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_users <- content(response_users, as = "text")
users <- as_tibble(fromJSON(json_users, flatten = TRUE))
rm(response_users)


rm(content)
rm(response)
