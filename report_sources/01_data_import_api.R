#Install package
devtools::install_github("WorldHealthOrganization/godataR")

#Set parameters
###################################################################################################

url <- "your Go.Data URL"                   # <--------------------- insert instance url here, don't forget the slash at end !
username <- "xxxxxxxxxxx@youremail.com"                           # <--------------------- insert your username for signing into Go.Data webapp here
password <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"                           # <--------------------- insert your password for signing into Go.Data webapp here
outbreak_id <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx"   # <--------------- insert your outbreak ID here! (find it in URL when you have selected outbreak)

###################################################################################################


#Get a data frame of collections with godataR function, pertaining to a specific outbreak
cases <- godataR::get_cases2(
                            url=url, username=username, password=password, outbreak_id=outbreak_id)

contacts <- godataR::get_contacts2(
                            url=url, username=username, password=password, outbreak_id=outbreak_id)

contacts_of_contacts <- godataR::get_contacts_of_contacts2(
                            url=url, username=username, password=password, outbreak_id=outbreak_id)

lab_results <- godataR::get_labresults2(
                            url=url, username=username, password=password, outbreak_id=outbreak_id)

relationships <- godataR::get_relationships2(
                            url=url, username=username, password=password, outbreak_id=outbreak_id)

followups <- godataR::get_followups2(
                            url=url, username=username, password=password, outbreak_id=outbreak_id)

events <- godataR::get_events2(
                            url=url, username=username, password=password, outbreak_id=outbreak_id)

clusters <- godataR::get_clusters(
                            url=url, username=username, password=password, outbreak_id=outbreak_id)

#Get a data frame of collections with godataR function, related to admin settings applied to your Go.Data instance

users <- godataR::get_users(
                            url=url, username=username, password=password) 

teams <- godataR::get_teams(
                            url=url, username=username, password=password)

locations <- godataR::get_locations(
                            url=url, username=username, password=password)

reference_data <- godataR::get_reference_data(
                            url=url, username=username, password=password)

