# Import Collections from Go.Data API

#load package
library(godataR)


#Get a data frame of collections with godataR function, pertaining to a specific outbreak
cases <- get_cases(url=url, username=username, password=password, outbreak_id=outbreak_id)

contacts <- get_contacts(url=url, username=username, password=password, outbreak_id=outbreak_id)

contacts_of_contacts <- get_contacts_of_contacts(url=url, username=username, password=password, outbreak_id=outbreak_id)

lab_results <- get_labresults(url=url, username=username, password=password, outbreak_id=outbreak_id)

relationships <- get_relationships(url=url, username=username, password=password, outbreak_id=outbreak_id)

followups <- get_followups(url=url, username=username, password=password, outbreak_id=outbreak_id)

events <- get_events(url=url, username=username, password=password, outbreak_id=outbreak_id)

clusters <- get_clusters(url=url, username=username, password=password, outbreak_id=outbreak_id)


#Get a data frame of collections with godataR function, related to admin settings applied to your Go.Data instance
users <- get_users(url=url, username=username, password=password) 

teams <- get_teams(url=url, username=username, password=password)

locations <- get_locations(url=url, username=username, password=password)

reference_data <- get_reference_data(url=url, username=username, password=password)

language_tokens <- get_language_tokens(url=url, username=username, password=password, language="english_us")