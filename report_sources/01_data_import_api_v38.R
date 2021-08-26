###################################################################################################

url <- "http://localhost:3000/"                   # <--------------------- insert instance url here, don't forget the slash at end !
username <- "fullerj@who.int"                           # <--------------------- insert your username for signing into Go.Data webapp here
password <- "Monn12345678"                           # <--------------------- insert your password for signing into Go.Data webapp here
outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"   # <--------------- insert your outbreak ID here! (find it in URL when you have selected outbreak)

# url <- "https://godata-r13.who.int/"                   # <--------------------- insert instance url here, don't forget the slash at end !
# username <- "godata_api@who.int"                           # <--------------------- insert your username for signing into Go.Data webapp here
# password <- "godata_api@who"                           # <--------------------- insert your password for signing into Go.Data webapp here
# outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"   # <--------------- insert your outbreak ID here! (find it in URL when you have selected outbreak)


###################################################################################################

# SCRIPT TO PULL IN COLLECTIONS ACROSS ANY GO.DATA INSTANCE #
# updated 04 August 2021

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
# FUNCTION TO GET ACCESS TOKEN
###################################################################################################
source(here::here("scripts", "get_godata_version.R"))

source(here::here("scripts", "get_access_token.R"))
print(get_access_token()) #Test to make sure url, username, and password are valid

###################################################################################################
# GET CASES
###################################################################################################

source(here::here("scripts", "get_cases2.R"))
source(here::here("scripts", "get_cases.R"))

cases <- get_cases2() #For Go.Data Version 2.38.1 or later
cases <- get_cases()

###################################################################################################
# GET CONTACTS
###################################################################################################

source(here::here("scripts", "get_contacts2.R"))
source(here::here("scripts", "get_contacts.R"))

contacts <- get_contacts2()
contacts <- get_contacts()

###################################################################################################
# GET FOLLOW-UP OF CONTACTS
###################################################################################################

source(here::here("scripts", "get_followups2.R"))
source(here::here("scripts", "get_followups.R"))

followups <- get_followups2() #For Go.Data Version 2.38.1 or later
followups <- get_followups()
