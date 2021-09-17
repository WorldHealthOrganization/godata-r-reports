####################################################################################################################################################
## Go.Data Cleaning Scripts of collections retrieved directly from API
# 
#       Before running this Script, please first run 01_data_import_api.R
#
#       This script is for cleaning the Go.Data data for core variables ONLY (i.e. does not retrieve from custom questionnaire as this varies across instances)
#       You can adapt these relatively easily if you need to add in variables from your questionnaire.
#       Script authored and maintained by Go.Data team (godata@who.int)


# Main cleaning functions include:

# 1. flatten hierarchical locations and bring in all vars for all admin levels, for easier joining later.
# 2. remove nested and list fields that you do not need, so dataframes can be exported properly.
# 3. for nested and list field that you do need, unnest and standardize responses i.e. hosp and isolation
# 4. standardize column name syntax
# 5. simplify categorical var syntax
# 6. get dates in right format
# 7. add useful new vars (age category)

####################################################################################################################################################


##########################################################################
## CLEAN LOCATIONS
## 
## rearrange via joins to get into more usable hierarchy format
## these can then be joined to cases, contacts, etc for further analysis.
##########################################################################

#This generates a warning, but it can be ignored

locations_clean <- locations %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  filter(active == TRUE | is.na(active)) %>%
  mutate(admin_level = sub(".*LEVEL_", "", geographicalLevelId)) %>%
  #unnest(identifiers, keep_empty=TRUE) %>%
  select(location_id = id,
         admin_level,
         name,
         ##code,
         parent_location_id = parentLocationId,
         ##population_density = populationDensity,
         lat = geoLocation.lat,
         long = geoLocation.lng
  ) %>%
  filter(!is.na(admin_level))

max.admin.level <- max(as.numeric(locations_clean$admin_level))
for (i in 0:max.admin.level) {
  admin_i <- locations_clean %>% filter(admin_level==i)
  names(admin_i) <- paste0("admin_",i,"_",names(admin_i))
  admin_i$location_id <- pull(admin_i, paste0("admin_",i,"_location_id"))
  admin_i$parent_location_id <- pull(admin_i, paste0("admin_",i,"_parent_location_id"))
  assign(paste0("admin_",i), admin_i)
}
admin_0$parent_location_id <- NULL
for (i in max.admin.level:1) {
  
  print(paste0("*****Starting Admin ", i, "*****"))
  
  admin_i <- get(paste0("admin_",i))
  
  for (x in 1:i) {
    print(paste0("*Joining Admin ", i-x, "*"))
    admin_ix <- get(paste0("admin_",i-x))
    admin_i <- left_join(admin_i, admin_ix, by=c("parent_location_id" = "location_id"))
    admin_i$parent_location_id <- admin_i$parent_location_id.y
    admin_i$parent_location_id.y <- NULL
    assign(paste0("admin_",i), admin_i)
  }
  admin_i$parent_location_id <- NULL
  assign(paste0("admin_",i), admin_i)
}

full <- admin_0
for (i in 1:max.admin.level) {
  admin_i <- get(paste0("admin_",i))
  full <- full %>% bind_rows(admin_i)
}
locations_clean <- locations_clean %>% left_join(full, by="location_id")

##########################################################################
### Clean & Un-nest CASES
##########################################################################

# Unnest pertinent list fields, where there can be multiple rows per case:

# Unnest Addresses and have a standalone table with all addresses even if more than 1 per person
cases_address_history_clean <- cases %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  select(id, visualId, addresses) %>%
  unnest(addresses, names_sep = "_") %>%
  select_all(~gsub("\\.","_",tolower(.))) %>%
  select_if(Negate(is.list)) %>%
  mutate(addresses_typeid = sub(".*TYPE_","",addresses_typeid)) %>%
  left_join(locations_clean, by=c("addresses_locationid" = "location_id")) %>%
  # bring in GPS from locations in case blank from case record, otherwise use case
  mutate(lat = case_when(
            is.na(addresses_geolocation_lat) ~ lat, TRUE ~ addresses_geolocation_lat),
         long = case_when(
           is.na(addresses_geolocation_lng) ~ lat, TRUE ~ addresses_geolocation_lng)) %>%
  select(id,
         visualid,
         addresses_locationid,
         addresses_typeid,
         lat,
         long,
         address = addresses_addressline1,
         postal_code = addresses_postalcode,
         city = addresses_city,
         telephone = addresses_phonenumber,
         email = addresses_emailaddress,
         matches("^admin_.*name$")) 


# Unnest Date Ranges - Isolation / Hospitalization History
cases_dateranges_history_clean <- cases %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  unnest(dateRanges, names_sep = "_") %>%
  select_at(vars(id, visualId, starts_with("dateRanges"),-dateRanges_dateRanges), tolower) %>%
  mutate(dateranges_typeid = sub(".*TYPE_", "", dateranges_typeid)) %>%
  mutate(dateranges_centername = sub(".*NAME_", "", dateranges_centername)) %>%
  mutate_at(vars(dateranges_startdate, dateranges_enddate), as.Date) 


# Unnest Vaccination History, where vaccination is complete
cases_vacc_history_clean <- cases %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  unnest(vaccinesReceived, names_sep = "_") %>%
  select_at(vars(id, visualId, starts_with("vaccinesReceived"),-vaccinesReceived_vaccinesReceived), tolower) %>%
  mutate(vaccinesreceived_vaccine = sub(".*VACCINE_", "", vaccinesreceived_vaccine)) %>%
  mutate(vaccinesreceived_status = sub(".*STATUS_", "", vaccinesreceived_status)) %>%
  mutate_at(vars(vaccinesreceived_date), as.Date) 
################################################################################  


cases_clean <- cases %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(Negate(is.list)) %>%
  
  # take out all that are not core variables, otherwise diff versions and problems exporting to excel
  select(-contains("questionnaireAnswers")) %>%
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(date_of_birth = dob,
         # date_of_follow_up_start = follow_up_start_date,
         # date_of_follow_up_end = follow_up_end_date,
         datetime_updated_at = updated_at,
         datetime_created_at = created_at) %>%
  
  # take out other unnecessary vars that are unnecessary and may confuse (i.e. was_case for cases)
  select(-c(
         is_date_of_onset_approximate,
         is_date_of_reporting_approximate,
         was_case,
         deleted,
         created_on
         )) %>%
  
  #clean up all blank fields
  mutate_all(na_if,"") %>%
  
  # clean date formats (TODO: edit this so that we can see time stamps)
  mutate_at(vars(starts_with("date_")), list(~ as.Date(substr(., 1, 10)))) %>%
  mutate(datetime_updated_at = as.POSIXct(datetime_updated_at,format="%Y-%m-%dT%H:%M")) %>%
  mutate(datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M")) %>%
  
  #  truncate responses of categorical vars so easier to read
  mutate(classification = sub(".*CLASSIFICATION_", "", classification),
         gender = sub(".*GENDER_", "", gender),
         occupation = sub(".*OCCUPATION_", "", occupation),
         outcome = sub(".*OUTCOME_", "", outcome_id),
         pregnancy_status = sub(".*STATUS_", "", pregnancy_status),
         risk_level = sub(".*LEVEL_", "", risk_level)) %>%
  

  # join in info from dateRanges block (i.e. hospitalization and isolation)
  # taking into account more than 1 entry per case, just bring back booleans here, can get full history from other table
  # NOTE: this is adapted for hosp and isolation drop down selections but if diff dropdown is desired, can adapt.
  
  # left_join(cases_hosp_history_clean %>% filter(dateranges_typeid=="ISOLATION"), by="id") %>%
  # rename_at(vars(starts_with("dateranges")), funs(str_replace(., "dateranges", "isolation"))) %>%
  
  
  mutate(isolated = case_when(id %in% cases_dateranges_history_clean$id[cases_dateranges_history_clean$dateranges_typeid == "ISOLATION"] ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(hospitalized = case_when(id %in% cases_dateranges_history_clean$id[cases_dateranges_history_clean$dateranges_typeid == "HOSPITALIZATION"] ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(icu = case_when(id %in% cases_dateranges_history_clean$id[cases_dateranges_history_clean$dateranges_typeid == "ICU_ADMISSION"] ~ TRUE, TRUE ~ FALSE)) %>%
  
  # join in current address from address history, only current place of residence
  # inner_join(select(locations_clean, location_id, ends_with("_name")), by = c("usual_place_of_residence_location_id" = "location_id")) %>%
  left_join(cases_address_history_clean %>% filter(addresses_typeid=="USUAL_PLACE_OF_RESIDENCE"), by="id") %>%

  # join in info from vacc block 
  mutate(vaccinated = case_when(id %in% cases_vacc_history_clean$id[cases_vacc_history_clean$vaccinesreceived_status == "VACCINATED"] ~ TRUE, TRUE ~ FALSE)) %>%

  
  # force NA ages to appear as NA, not as 0 like sometimes occurs  
  mutate(age_years = as.numeric(age_years)) %>%
  mutate(age_years = na_if(age_years,0)) %>%
  mutate(age_months = as.numeric(age_months)) %>%
  mutate(age_months = na_if(age_months,0)) %>%
  
  # standardize age vars into just one var
  mutate(age = case_when(!is.na(age_months) ~  age_months / 12,
                         TRUE ~ age_years)) %>%
  
  # WHO recommended age categories, updated Sept 2020
  mutate(
    age_class = factor(
      case_when(
        age <= 4 ~ "0-4",
        age <= 9 ~ "5-9",
        age <= 14 ~ "10-14",
        age <= 19 ~ "15-19",
        age <= 29 ~ "20-29",
        age <= 39 ~ "30-39",
        age <= 49 ~ "40-49",
        age <= 59 ~ "50-59",
        age <= 64 ~ "60-64",
        age <= 69 ~ "65-69",
        age <= 74 ~ "70-74",
        age <= 79 ~ "75-79",
        is.finite(age) ~ "80+",
        TRUE ~ "unknown"
      ), levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80+",
        "unknown"
      )),
    age_class = factor(
      age_class,
      levels = rev(levels(age_class)))) %>%
  
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, visual_id, classification, # identifier 
    first_name, middle_name, last_name, gender, age, age_class, occupation, pregnancy_status, # demographics
    date_of_reporting, date_of_onset, date_of_infection, date_become_case, date_of_burial, # dates
    was_contact, risk_level, risk_reason, safe_burial, transfer_refused, # epi
    responsible_user_id, # assigned contact tracer
    matches("^admin_.*name$"), lat, long, address, postal_code, city, telephone, email, # address
    vaccinated, isolated, hospitalized, icu, # vaccination & dateRanges block
    outcome, date_of_outcome,  # outcome
    location_id = addresses_locationid,  # uuid in case need later for joining of whatever sort.
    created_by, datetime_created_at, updated_by, datetime_updated_at) # record modification

##########################################################################################

#Pull out all cases that used to be contacts

contacts_becoming_cases <- cases_clean %>%
  filter(was_contact == TRUE) %>%
# set this status to became case and no longer active. 
  mutate(follow_up_status = "BECAME_CASE",
         was_case=NA, 
         date_of_last_contact=NA,
         follow_up_team_id=NA,
         relationship_exposure_type=NA, relationship_context_of_transmission=NA, relationship_exposure_duration=NA, relationship_exposure_frequency=NA, relationship_certainty_level=NA, relationship_cluster_id=NA,
         ) %>%
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, visual_id, classification, follow_up_status, # identifier 
    first_name, middle_name, last_name, gender, age, age_class, occupation, pregnancy_status, # demographics
    date_of_reporting, date_of_last_contact, date_of_burial, # dates
    risk_level, risk_reason, # epi 
    responsible_user_id, follow_up_team_id, # assigned contact tracer
    matches("^admin_.*name$"), lat, long, address, postal_code, city, telephone, email, # address
    vaccinated, 
    outcome, date_of_outcome,  # outcome
    relationship_exposure_type, relationship_context_of_transmission, relationship_exposure_duration, relationship_exposure_frequency, relationship_certainty_level, relationship_cluster_id, 
    location_id,  # uuid in case need later for joining of whatever sort.
    created_by, datetime_created_at, updated_by, datetime_updated_at) # record modification


##########################################################################
### Clean & Un-nest CONTACTS
##########################################################################

# Unnest pertinent list fields, where there can be multiple rows per case:

# Unnest Addresses and have a standalone table with all addresses even if more than 1 per person
contacts_address_history_clean <- contacts %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  select(id, visualId, addresses) %>%
  unnest(addresses, names_sep = "_") %>%
  select_all(~gsub("\\.","_",tolower(.))) %>%
  select_if(Negate(is.list)) %>%
  mutate(addresses_typeid = sub(".*TYPE_","",addresses_typeid)) %>%
  left_join(locations_clean, by=c("addresses_locationid" = "location_id")) %>%
  # bring in GPS from locations if blank in contact record, otherwise use contact address block
  mutate(lat = case_when(
    is.na(addresses_geolocation_lat) ~ lat, TRUE ~ addresses_geolocation_lat),
    long = case_when(
      is.na(addresses_geolocation_lng) ~ lat, TRUE ~ addresses_geolocation_lng)) %>%
  select(id,
         addresses_locationid,
         addresses_typeid,
         lat,
         long,
         address = addresses_addressline1,
         postal_code = addresses_postalcode,
         city = addresses_city,
         telephone = addresses_phonenumber,
         email = addresses_emailaddress,
         matches("^admin_.*name$")) 


# Unnest Vaccination History, where vaccination is complete
contacts_vacc_history_clean <- contacts %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  unnest(vaccinesReceived, names_sep = "_") %>%
  select_at(vars(id, visualId, starts_with("vaccinesReceived"),-vaccinesReceived_vaccinesReceived), tolower) %>%
  mutate(vaccinesreceived_vaccine = sub(".*VACCINE_", "", vaccinesreceived_vaccine)) %>%
  mutate(vaccinesreceived_status = sub(".*STATUS_", "", vaccinesreceived_status)) %>%
  mutate_at(vars(vaccinesreceived_date), as.Date) 

# if you have a question about quarantine in your questionnaire, could unnest that here and then do a left join later to join this to main table

#####################################################################################################################################################################


contacts_clean <- contacts %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(Negate(is.list)) %>%
  
  # take out all that are not core variables, otherwise diff versions and problems exporting to excel
  select(-contains("questionnaireAnswers")) %>%
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(date_of_birth = dob,
         date_of_follow_up_start = follow_up_start_date,
         date_of_follow_up_end = follow_up_end_date,
         datetime_updated_at = updated_at,
         datetime_created_at = created_at) %>%
  
  # take out other unnecessary vars that are unnecessary and may confuse (i.e. was_case for cases)
  select(-c(
    is_date_of_reporting_approximate,
    was_contact,
    follow_up_original_start_date,
    type,
    deleted,
    created_on
  )) %>%
  
  #clean up all character fields
  mutate_all(na_if,"") %>%
  
  # clean date formats (TODO: edit this so that we can see time stamps)
  mutate_at(vars(starts_with("date_")), list(~ as.Date(substr(., 1, 10)))) %>%
  mutate(datetime_updated_at = as.POSIXct(datetime_updated_at,format="%Y-%m-%dT%H:%M")) %>%
  mutate(datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M")) %>%
  
  #  truncate responses of categorical vars so easier to read
  mutate(classification = sub(".*CLASSIFICATION_", "", classification),
         gender = sub(".*GENDER_", "", gender),
         occupation = sub(".*OCCUPATION_", "", occupation),
         outcome = sub(".*OUTCOME_", "", outcome_id),
         pregnancy_status = sub(".*STATUS_", "", pregnancy_status),
         risk_level = sub(".*LEVEL_", "", risk_level),
         follow_up_status = sub(".*TYPE_", "", follow_up_status),
         relationship_certainty_level = sub(".*LEVEL_", "", relationship_certainty_level_id),
         relationship_exposure_type = sub(".*TYPE_", "", relationship_exposure_type_id),
         relationship_context_of_transmission = sub(".*TRANSMISSION_", "", relationship_social_relationship_type_id),
         relationship_exposure_frequency = sub(".*FREQUENCY_", "", relationship_exposure_frequency_id),
         relationship_exposure_duration = sub(".*DURATION_", "", relationship_exposure_duration_id),
         ) %>%
  
  
  # join in current address from address history, only current place of residence
  # inner_join(select(locations_clean, location_id, ends_with("_name")), by = c("usual_place_of_residence_location_id" = "location_id")) %>%
  left_join(contacts_address_history_clean %>% filter(addresses_typeid=="USUAL_PLACE_OF_RESIDENCE"), by="id") %>%
  
  # join in info from vacc block 
  mutate(vaccinated = case_when(id %in% contacts_vacc_history_clean$id[contacts_vacc_history_clean$vaccinesreceived_status == "VACCINATED"] ~ TRUE, TRUE ~ FALSE)) %>%
  
  
  # force NA ages to appear as NA, not as 0 like sometimes occurs  
  mutate(age_years = as.numeric(age_years)) %>%
  mutate(age_years = na_if(age_years,0)) %>%
  mutate(age_months = as.numeric(age_months)) %>%
  mutate(age_months = na_if(age_months,0)) %>%
  
  # standardize age vars into just one var
  mutate(age = case_when(!is.na(age_months) ~  age_months / 12,
                         TRUE ~ age_years)) %>%
  
  # WHO recommended age categories, updated Sept 2020
  mutate(
    age_class = factor(
      case_when(
        age <= 4 ~ "0-4",
        age <= 9 ~ "5-9",
        age <= 14 ~ "10-14",
        age <= 19 ~ "15-19",
        age <= 29 ~ "20-29",
        age <= 39 ~ "30-39",
        age <= 49 ~ "40-49",
        age <= 59 ~ "50-59",
        age <= 64 ~ "60-64",
        age <= 69 ~ "65-69",
        age <= 74 ~ "70-74",
        age <= 79 ~ "75-79",
        is.finite(age) ~ "80+",
        TRUE ~ "unknown"
      ), levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80+",
        "unknown"
      )),
    age_class = factor(
      age_class,
      levels = rev(levels(age_class)))) %>%
  
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, visual_id, classification, follow_up_status, # identifier 
    first_name, middle_name, last_name, gender, age, age_class, occupation, pregnancy_status, # demographics
    date_of_reporting, date_of_last_contact, date_of_burial, # dates
    was_case, risk_level, risk_reason, safe_burial, transfer_refused, # epi
    responsible_user_id, follow_up_team_id, # assigned contact tracer
    matches("^admin_.*name$"), lat, long, address, postal_code, city, telephone, email, # address
    vaccinated, # vaccination
    outcome, date_of_outcome,  # outcome
    relationship_exposure_type, relationship_context_of_transmission, relationship_exposure_duration, relationship_exposure_frequency, relationship_certainty_level, relationship_cluster_id, 
    location_id = addresses_locationid,  # uuid in case need later for joining of whatever sort.
    created_by, datetime_created_at, updated_by, datetime_updated_at) %>% # record modification
  


  #Join in cases that used to be contacts
    bind_rows(contacts_becoming_cases) 

##########################################################################
### Clean & Un-nest CONTACTS OF CONTACTS
##########################################################################

# Unnest pertinent list fields, where there can be multiple rows per case:

# Unnest Addresses and have a standalone table with all addresses even if more than 1 per person
contacts_of_contacts_address_history_clean <- contacts_of_contacts %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  select(id, visualId, addresses) %>%
  unnest(addresses, names_sep = "_") %>%
  select_all(~gsub("\\.","_",tolower(.))) %>%
  select_if(Negate(is.list)) %>%
  mutate(addresses_typeid = sub(".*TYPE_","",addresses_typeid)) %>%
  left_join(locations_clean, by=c("addresses_locationid" = "location_id")) %>%
  # bring in GPS from locations if blank in contact record, otherwise use contact address block
  mutate(lat = case_when(
    is.na(addresses_geolocation_lat) ~ lat, TRUE ~ addresses_geolocation_lat),
    long = case_when(
      is.na(addresses_geolocation_lng) ~ lat, TRUE ~ addresses_geolocation_lng)) %>%
  select(id,
         addresses_locationid,
         addresses_typeid,
         lat,
         long,
         address = addresses_addressline1,
         postal_code = addresses_postalcode,
         city = addresses_city,
         telephone = addresses_phonenumber,
         email = addresses_emailaddress,
         matches("^admin_.*name$")) 


# Unnest Vaccination History, where vaccination is complete
contacts_of_contacts_vacc_history_clean <- contacts_of_contacts %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  unnest(vaccinesReceived, names_sep = "_") %>%
  select_at(vars(id, visualId, starts_with("vaccinesReceived")), tolower) %>%
  mutate(vaccinesreceived_vaccine = sub(".*VACCINE_", "", vaccinesreceived_vaccine)) %>%
  mutate(vaccinesreceived_status = sub(".*STATUS_", "", vaccinesreceived_status)) %>%
  mutate_at(vars(vaccinesreceived_date), as.Date) 

# if you have a question about quarantine in your questionnaire, could unnest that here and then do a left join later to join this to main table

#####################################################################################################################################################################


contacts_of_contacts_clean <- contacts_of_contacts %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(Negate(is.list)) %>%
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(date_of_birth = dob,
         datetime_updated_at = updated_at,
         datetime_created_at = created_at) %>%
  
  #clean up all character fields
  mutate_all(na_if,"") %>%
  
  # clean date formats (TODO: edit this so that we can see time stamps)
  mutate_at(vars(starts_with("date_")), list(~ as.Date(substr(., 1, 10)))) %>%
  mutate(datetime_updated_at = as.POSIXct(datetime_updated_at,format="%Y-%m-%dT%H:%M")) %>%
  mutate(datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M")) %>%
  
  #  truncate responses of categorical vars so easier to read
  mutate(classification = sub(".*CLASSIFICATION_", "", classification),
         gender = sub(".*GENDER_", "", gender),
         occupation = sub(".*OCCUPATION_", "", occupation),
         outcome = sub(".*OUTCOME_", "", outcome_id),
         pregnancy_status = sub(".*STATUS_", "", pregnancy_status),
         risk_level = sub(".*LEVEL_", "", risk_level),
         relationship_certainty_level = sub(".*LEVEL_", "", relationship_certainty_level_id),
         relationship_exposure_type = sub(".*TYPE_", "", relationship_exposure_type_id),
         relationship_context_of_transmission = sub(".*TRANSMISSION_", "", relationship_social_relationship_type_id),
         relationship_exposure_frequency = sub(".*FREQUENCY_", "", relationship_exposure_frequency_id),
         relationship_exposure_duration = sub(".*DURATION_", "", relationship_exposure_duration_id),
  ) %>%
  
  
  # join in current address from address history, only current place of residence
  # inner_join(select(locations_clean, location_id, ends_with("_name")), by = c("usual_place_of_residence_location_id" = "location_id")) %>%
  left_join(contacts_of_contacts_address_history_clean %>% filter(addresses_typeid=="USUAL_PLACE_OF_RESIDENCE"), by="id") %>%
  
  # join in info from vacc block 
  mutate(vaccinated = case_when(id %in% contacts_of_contacts_vacc_history_clean$id[contacts_of_contacts_vacc_history_clean$vaccinesreceived_status == "VACCINATED"] ~ TRUE, TRUE ~ FALSE)) %>%
  
  
  # force NA ages to appear as NA, not as 0 like sometimes occurs  
  mutate(age_years = as.numeric(age_years)) %>%
  mutate(age_years = na_if(age_years,0)) %>%
  mutate(age_months = as.numeric(age_months)) %>%
  mutate(age_months = na_if(age_months,0)) %>%
  
  # standardize age vars into just one var
  mutate(age = case_when(!is.na(age_months) ~  age_months / 12,
                         TRUE ~ age_years)) %>%
  
  # WHO recommended age categories, updated Sept 2020
  mutate(
    age_class = factor(
      case_when(
        age <= 4 ~ "0-4",
        age <= 9 ~ "5-9",
        age <= 14 ~ "10-14",
        age <= 19 ~ "15-19",
        age <= 29 ~ "20-29",
        age <= 39 ~ "30-39",
        age <= 49 ~ "40-49",
        age <= 59 ~ "50-59",
        age <= 64 ~ "60-64",
        age <= 69 ~ "65-69",
        age <= 74 ~ "70-74",
        age <= 79 ~ "75-79",
        is.finite(age) ~ "80+",
        TRUE ~ "unknown"
      ), levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80+",
        "unknown"
      )),
    age_class = factor(
      age_class,
      levels = rev(levels(age_class)))) %>%
  
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, visual_id, classification, # identifier 
    first_name, middle_name, last_name, gender, age, age_class, occupation, pregnancy_status, # demographics
    date_of_reporting, date_of_last_contact, date_of_burial, # dates
    was_case, risk_level, risk_reason, safe_burial, transfer_refused, # epi
    responsible_user_id, # assigned contact tracer
    matches("^admin_.*name$"), lat, long, address, postal_code, city, telephone, email, # address
    vaccinated, # vaccination
    outcome, date_of_outcome,  # outcome
    relationship_exposure_type, relationship_context_of_transmission, relationship_exposure_duration, relationship_exposure_frequency, relationship_certainty_level, relationship_cluster_id, 
    location_id = addresses_locationid,  # uuid in case need later for joining of whatever sort.
    created_by, datetime_created_at, updated_by, datetime_updated_at)

  
##########################################################################
### Clean & Un-nest FOLLOW-UPS
##########################################################################


followups_clean <- followups %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(Negate(is.list)) %>%
  
  # take out all that are not core variables, otherwise diff versions and problems exporting to excel
  select(-contains("questionnaireAnswers")) %>%
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(
         datetime_updated_at = updated_at,
         datetime_created_at = created_at) %>%
  
  #clean up all character fields
  mutate_all(na_if,"") %>%
  
  # clean date formats (TODO: edit this so that we can see time stamps)
  mutate_at(vars(starts_with("date_")), list(~ as.Date(substr(., 1, 10)))) %>%
  mutate(datetime_updated_at = as.POSIXct(datetime_updated_at,format="%Y-%m-%dT%H:%M")) %>%
  mutate(datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M")) %>%
  
  #  truncate responses of categorical vars so easier to read
  mutate(followup_status = sub(".*TYPE_", "", status_id)) %>%
  
  left_join(contacts_address_history_clean %>% filter(addresses_typeid=="USUAL_PLACE_OF_RESIDENCE"), by="id") %>%
  
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, contact_id, contact_visual_id, # identifier 
    date, # dates 
    followup_number = index, followup_status, targeted, # FU status
    responsible_user_id, team_id, # assigned contact tracer
    matches("^admin_.*name$"), lat, long, address, postal_code, city, telephone, email, # address
    location_id = addresses_locationid,  # uuid in case need later for joining of whatever sort.
    created_by, datetime_created_at, updated_by, datetime_updated_at)  # record modification
  

##########################################################################
# Clean & Un-nest EVENTS
##########################################################################

events_clean <- events %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(Negate(is.list)) %>%
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(
    datetime_updated_at = updated_at,
    datetime_created_at = created_at) %>%
  
  #clean up all character fields
  mutate_all(na_if,"") %>%
  
  # clean date formats (TODO: edit this so that we can see time stamps)
  mutate_at(vars(starts_with("date_")), list(~ as.Date(substr(., 1, 10)))) %>%
  mutate(datetime_updated_at = as.POSIXct(datetime_updated_at,format="%Y-%m-%dT%H:%M")) %>%
  mutate(datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M")) %>%
  
  left_join(select(locations_clean,
                   location_id,
                   matches("^admin_.*name$")),
                   by = c("address_location_id" = "location_id")) %>%
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, name, # identifier 
    date, date_of_reporting, #dates
    description, 
    responsible_user_id, # assigned contact tracer
    matches("^admin_.*name$"), lat = address_geo_location_lat, long = address_geo_location_lng, address = address_address_line1, postal_code = address_postal_code, city = address_city, telephone = address_phone_number, email = address_email_address, # address
    location_id = address_location_id,  # uuid in case need later for joining of whatever sort.
    created_by, datetime_created_at, updated_by, datetime_updated_at)  # record modification


##########################################################################
# Clean & Un-nest LAB RESULTS
##########################################################################

lab_results_clean <- lab_results %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(Negate(is.list)) %>%
  
  # take out all that are not core variables, otherwise diff versions and problems exporting to excel
  select(-contains("questionnaireAnswers")) %>%
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(
    date_of_onset = person_date_of_onset,
    date_sequence_result = sequence_date_result,
    date_sequence_sent = sequence_date_sample_sent,
    datetime_updated_at = updated_at,
    datetime_created_at = created_at) %>%
  
  #clean up all character fields
  mutate_all(na_if,"") %>%
  
  # clean date formats (TODO: edit this so that we can see time stamps)
  mutate_at(vars(starts_with("date_")), list(~ as.Date(substr(., 1, 10)))) %>%
  mutate(datetime_updated_at = as.POSIXct(datetime_updated_at,format="%Y-%m-%dT%H:%M")) %>%
  mutate(datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M")) %>%
  
  #  truncate responses of categorical vars so easier to read
  mutate(test_type = sub(".*TEST_", "", test_type),
         sample_type = sub(".*SAMPLE_", "", sample_type),
         result = sub(".*RESULT_", "", result),
         status = sub(".*STATUS_", "", status),
         person_type = sub(".*TYPE_", "", person_type),
  ) %>%
  
  left_join(select(locations_clean,
                   location_id,
                   matches("^admin_.*name$")),
            by = c("person_address_location_id" = "location_id")) %>%
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, sample_id = sample_identifier, # identifier sample
    person_type, person_id, person_visual_id, # identifier person
    date_of_onset, date_sample_taken, date_sample_delivered, date_testing, date_of_result, #dates
    result, test_type, sample_type, lab_name, quantitative_result, tested_for, notes, status, # result details
    has_sequence = sequence_has_sequence, no_sequence_reason = sequence_no_sequence_reason, sequence_result_id, date_sequence_sent, date_sequence_result, sequence_lab_name = sequence_lab_id,
    matches("^admin_.*name$"),
    location_id = person_address_location_id,  # uuid in case need later for joining of whatever sort.
    created_by, datetime_created_at, updated_by, datetime_updated_at)  # record modification


##########################################################################
# Clean & Un-nest RELATIONSHIPS
##########################################################################

relationships_clean <- relationships %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%

  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(Negate(is.list)) %>%
  
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(
    datetime_updated_at = updated_at,
    datetime_created_at = created_at) %>%
  
  #clean up all character fields
  mutate_all(na_if,"") %>%
  
  # clean date formats (TODO: edit this so that we can see time stamps)
  mutate_at(vars(starts_with("date_")), list(~ as.Date(substr(., 1, 10)))) %>%
  mutate(datetime_updated_at = as.POSIXct(datetime_updated_at,format="%Y-%m-%dT%H:%M")) %>%
  mutate(datetime_created_at = as.POSIXct(datetime_created_at,format="%Y-%m-%dT%H:%M")) %>%
  
  #  truncate responses of categorical vars so easier to read
  mutate(source_person_type = sub(".*TYPE_", "", source_person_type),
         target_person_type = sub(".*TYPE_", "", target_person_type)) %>%
  
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, source_person_id,source_person_visual_id, target_person_id, target_person_visual_id, source_person_type, target_person_type, #id
    created_by, datetime_created_at, updated_by, datetime_updated_at)  # record modification


##########################################################################
## some other additions for handy things to have in linelist .csv
##########################################################################

contacts_per_case <- relationships_clean %>%
  group_by(source_person_id) %>%
  tally() %>%
  select(source_person_id, 
         no_contacts = n)

exposures_per_case <- relationships_clean %>%
  group_by(target_person_id) %>%
  tally() %>%
  select(target_person_id, 
         no_exposures = n)


# cases linelist, now with contacts per case (can add other vars here as needed)

cases_clean <- cases_clean %>%
  left_join(contacts_per_case, by = c("id" = "source_person_id")) %>%
  left_join(exposures_per_case, by = c("id" = "target_person_id")) %>%
  mutate(no_contacts = replace(no_contacts, is.na(no_contacts),0)) %>%
  mutate(no_exposures = replace(no_exposures, is.na(no_exposures),0)) %>%
  select(id, visual_id, no_contacts, no_exposures, everything())


##########################################################################
# Clean & Un-nest TEAMS
##########################################################################

teams_clean <- teams %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(
    datetime_updated_at = updated_at,
    datetime_created_at = created_at) %>%
  
  #clean up all character fields
  mutate_all(na_if,"") %>%
  
  
  unnest_wider(user_ids, names_sep = "_") %>%
  unnest_wider(location_ids, names_sep = "_") %>%
  
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, name,
    starts_with("user_ids"),
    starts_with("location_ids"),
    created_by, datetime_created_at, updated_by, datetime_updated_at)  # record modification

##########################################################################
## Clean & Un-nest USERS 
##########################################################################


users_clean <- users %>%
  
  # Remove all deleted records 
  #filter(deleted == FALSE | is.na(deleted)) %>%
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(
    datetime_last_login = last_login_date,
    datetime_created_at = created_at) %>%
  
  #clean up all character fields
  mutate_all(na_if,"") %>%
  
  unnest_wider(role_ids, names_sep = "_") %>%
  
  
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, first_name, last_name, email, institution_name, disregard_geographic_restrictions,
    starts_with("role_ids"),
    active_outbreak_id,
    created_by, datetime_created_at, datetime_last_login) 



###############################################################
## Export Raw Dataframes(to be overwritten each time script is run) 
###############################################################

## Specify location to save files
data_folder <- here::here("data", "raw")

## specify data frames to export
mydfs <- c("cases", "contacts", "contacts_of_contacts", "lab_results", "relationships", "followups", "events", "clusters", "users", "teams", "locations", "reference_data")
mydfs 


## export all as .rds files which we will use for report scripts as it preserves language characters better
for (i in 1:length(mydfs)){
  savefile<-paste0(data_folder,"/", mydfs[i], ".rds")
  saveRDS(get(mydfs[i]), file=savefile)
  print(paste("Dataframe Saved:", mydfs[i]))
}


###############################################################
## Export Cleaned Dataframes(to be overwritten each time script is run) 
###############################################################

## Specify location to save files
data_folder <- here::here("data", "clean")

## specify data frames to export
mydfs_clean <- ls(pattern = "_clean")
mydfs_clean

## export files as .csv
for (i in 1:length(mydfs_clean)){
  savefile<-paste0(data_folder,"/", mydfs_clean[i], ".csv")
  write.csv(get(mydfs_clean[i]), file=savefile, fileEncoding = "UTF-8", na="", row.names = F)
  
  print(paste("Dataframe Saved:", mydfs_clean[i]))
}

## export all as .rds files which we will use for report scripts as it preserves language characters better
for (i in 1:length(mydfs_clean)){
  savefile<-paste0(data_folder,"/", mydfs_clean[i], ".rds")
  saveRDS(get(mydfs_clean[i]), file=savefile)
  print(paste("Dataframe Saved:", mydfs_clean[i]))
}
