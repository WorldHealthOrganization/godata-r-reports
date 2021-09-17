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
  select_if(negate(is.list)) %>%
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


# Unnest Date Ranges - Isolation / Hospitalizaton History
cases_hosp_history_clean <- cases %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  unnest(dateRanges, names_sep = "_") %>%
  select_at(vars(id, visualId, starts_with("dateRanges"),-dateRanges_dateRanges), tolower) %>%
  mutate(dateranges_typeid = sub(".*TYPE_", "", dateranges_typeid)) %>%
  mutate(dateranges_centername = sub(".*NAME_", "", dateranges_centername)) %>%
  mutate_at(vars(dateranges_startdate, dateranges_enddate), as.Date) %>%
  filter(dateranges_typeid == "HOSPITALIZATION")
  # mutate(dateranges_enddate = case_when(!is.na(dateranges_enddate) ~ dateranges_enddate,
  #                                       TRUE ~ dateranges_startdate + 14)) %>%
  # mutate(dateranges_status = case_when(dateranges_enddate <= Sys.Date() ~ "completed",
  #                                      dateranges_enddate >= Sys.Date() ~ "ongoing",
  #                                      TRUE ~ "date_missing")) 

cases_iso_history_clean <- cases %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  unnest(dateRanges, names_sep = "_") %>%
  select_at(vars(id, visualId, starts_with("dateRanges"),-dateRanges_dateRanges), tolower) %>%
  mutate(dateranges_typeid = sub(".*TYPE_", "", dateranges_typeid)) %>%
  mutate(dateranges_centername = sub(".*NAME_", "", dateranges_centername)) %>%
  mutate_at(vars(dateranges_startdate, dateranges_enddate), as.Date) %>%
  filter(dateranges_typeid == "ISOLATION")



# Unnest Vaccination History, where vaccination is complete
cases_vacc_history_clean <- cases %>%
  filter(deleted == FALSE | is.na(deleted)) %>%
  unnest(vaccinesReceived, names_sep = "_") %>%
  select_at(vars(id, visualId, starts_with("vaccinesReceived"),-vaccinesReceived_vaccinesReceived), tolower) %>%
  mutate(vaccinesreceived_vaccine = sub(".*VACCINE_", "", vaccinesreceived_vaccine)) %>%
  mutate(vaccinesreceived_status = sub(".*STATUS_", "", vaccinesreceived_status)) %>%
  filter(vaccinesreceived_status == "VACCINATED") %>%
  mutate_at(vars(vaccinesreceived_date), as.Date) 
################################################################################  


cases_clean <- cases %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(negate(is.list)) %>%
  
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
  
  #clean up all character fields
  mutate_if(is_character, funs(na_if(.,""))) %>%
  
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
  
  mutate(isolated = case_when(id %in% cases_iso_history_clean$id ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(hospitalized = case_when(id %in% cases_hosp_history_clean$id ~ TRUE, TRUE ~ FALSE)) %>%
  
  # join in current address from address history, only current place of residence
  # inner_join(select(locations_clean, location_id, ends_with("_name")), by = c("usual_place_of_residence_location_id" = "location_id")) %>%
  left_join(cases_address_history_clean %>% filter(addresses_typeid=="USUAL_PLACE_OF_RESIDENCE"), by="id") %>%

  # join in info from vacc block 
  mutate(vaccinated = case_when(id %in% cases_vacc_history_clean$id ~ TRUE, TRUE ~ FALSE)) %>%
 # left_join(distinct(cases_vacc_history_clean %>% filter(vaccinesreceived_status=="VACCINATED"), by="id")) %>%

  
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
    vaccinated, isolated, hospitalized, # vaccination & dateRanges block
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
         relationship_exposure_type=NA, relationship_context_of_transmission=NA, relationship_exposure_duration=NA, relationship_exposure_frequency=NA, relationship_certainty_level=NA, relationship_cluster_id=NA,
         ) %>%
  # organize order of vars, only bring in what we need, take away confusing vars
  select(
    id, visual_id, classification, follow_up_status, # identifier 
    first_name, middle_name, last_name, gender, age, age_class, occupation, pregnancy_status, # demographics
    date_of_reporting, date_of_last_contact, date_of_burial, # dates
    risk_level, risk_reason, # epi 
    responsible_user_id, # assigned contact tracer
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
  select_if(negate(is.list)) %>%
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
  filter(vaccinesreceived_status == "VACCINATED") %>%
  mutate_at(vars(vaccinesreceived_date), as.Date) 

# if you have a question about quarantine in your questionnaire, could unnest that here and then do a left join later to join this to main table

#####################################################################################################################################################################


contacts_clean <- contacts %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(negate(is.list)) %>%
  
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
  mutate_if(is_character, funs(na_if(.,""))) %>%
  
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
  mutate(vaccinated = case_when(id %in% contacts_vacc_history_clean$id ~ TRUE, TRUE ~ FALSE)) %>%
  # left_join(distinct(cases_vacc_history_clean %>% filter(vaccinesreceived_status=="VACCINATED"), by="id")) %>%
  
  
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
    vaccinated, # vaccination & dateRanges block
    outcome, date_of_outcome,  # outcome
    relationship_exposure_type, relationship_context_of_transmission, relationship_exposure_duration, relationship_exposure_frequency, relationship_certainty_level, relationship_cluster_id, 
    location_id = addresses_locationid,  # uuid in case need later for joining of whatever sort.
    created_by, datetime_created_at, updated_by, datetime_updated_at) %>% # record modification


  #Join in cases that used to be contacts
    bind_rows(contacts_becoming_cases) 
  
  
##########################################################################
### Clean & Un-nest FOLLOW-UPS
##########################################################################


followups_clean <- followups %>%
  
  # Remove all deleted records 
  filter(deleted == FALSE | is.na(deleted)) %>%
  
  # Remove all nested fields, otherwise problems with exporting to excel
  select_if(negate(is.list)) %>%
  
  # take out all that are not core variables, otherwise diff versions and problems exporting to excel
  select(-contains("questionnaireAnswers")) %>%
  
  # standardize column name syntax, label timestamps as datetime
  janitor::clean_names() %>%
  rename(
         datetime_updated_at = updated_at,
         datetime_created_at = created_at) %>%
  
  #clean up all character fields
  mutate_if(is_character, funs(na_if(.,""))) %>%
  
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
if (nrow(events)==0) {
  events_clean <- data.frame(matrix(ncol=length(events_columns_final)))
  colnames(events_clean) <- events_columns_final
} else {
  
  
  ## unnest events to get contacts and exposures
  # events_relationship_history <- events %>%
  #   select(id, name, relationshipsRepresentation) %>%
  #   unnest(relationshipsRepresentation, names_sep = "_", keep_empty = TRUE) %>%
  #   mutate(other_participant_type = sub(".*TYPE_", "", relationshipsRepresentation_otherParticipantType)) %>%
  #   mutate(relationship_type = case_when(
  #     relationshipsRepresentation_source == TRUE  ~ "CONTACT",
  #     relationshipsRepresentation_target == TRUE  ~ "EXPOSURE",
  #   )) 
  # 
  # events_relationship_history_counts <- events_relationship_history %>%
  #   select(
  #     id,
  #     name,
  #     relationship_type,
  #     other_participant_type,
  #     other_participant_id = relationshipsRepresentation_otherParticipantId) %>%
  #   group_by(id, relationship_type) %>%
  #   tally()
  
  
  date_fields_events <- c("dateOfReporting",
                              # "dateOfLastContact",
                          "createdAt","updatedAt","date")
  
  events_clean <- events %>%
    filter(deleted==FALSE) %>%
    mutate_if(is_character, funs(na_if(.,""))) %>%
    # clean date formats
    mutate_at(vars(all_of(date_fields_events)), list(~ as.Date(substr(., 1, 10)))) %>%
    rename(event_name = name) %>%
    mutate(date_of_reporting = dateOfReporting,
           date_of_data_entry = createdAt,
           # date_of_last_contact = dateOfLastContact, 
           date_of_event = date) %>%
    left_join(locations_clean, by=c("address.locationId" = "location_id")) %>%
    #join in count of contacts per event 
    # left_join(events_relationship_history_counts %>% filter(relationship_type=="CONTACT"), by="id") %>% rename(number_of_contacts = n) %>%
    # #join in count of exposures per event
    # left_join(events_relationship_history_counts %>% filter(relationship_type=="EXPOSURE"), by="id") %>% rename(number_of_exposures = n) %>%
    # mutate(number_of_contacts = replace(number_of_contacts, is.na(number_of_contacts),0)) %>%
    # mutate(number_of_exposures = replace(number_of_exposures, is.na(number_of_exposures),0)) %>%
    
    select(
      uuid = id,
      event_name,
      description,
      # number_of_contacts,
      # number_of_exposures,
      date_of_event,
      date_of_reporting,
      date_of_data_entry,
      location_id = address.locationId,
      lat = address.geoLocation.lat,
      long = address.geoLocation.lng,
      geo_location_accurate = address.geoLocationAccurate,
      # address = address.addressLine1, # EO: not found in data
      # postal_code = address.postalCode,
      city = address.city,
      starts_with("admin_"),
      createdBy
    )
  
  
  
}


##########################################################################
# Clean & Un-nest CONTACTS OF CONTACTS
##########################################################################
# if (nrow(contacts_of_contacts)==0) {
#   contacts_of_contacts_clean <- data.frame(matrix(ncol=length(contacts_of_contacts_columns_final)))
#   colnames(contacts_of_contacts_clean) <- contacts_of_contacts_columns_final
# } else {
#   
#   #Unnest Addresses
#   addresses_contacts_of_contacts <- contacts_of_contacts %>%
#     filter(deleted == FALSE) %>%
#     select(id, addresses) %>%
#     unnest(addresses, names_sep = "_") %>%
#     mutate(addresses_typeId = sub("LNG_REFERENCE_DATA_CATEGORY_ADDRESS_TYPE_","",addresses_typeId)) %>%
#     left_join(locations_clean, by=c("addresses_locationId" = "location_id"))
#   
#   
#   date_fields_contacts_of_contacts <- c("dob","dateOfReporting","dateOfLastContact","createdAt","updatedAt","createdOn")
#   
#   contacts_of_contacts_clean <- contacts_of_contacts %>%
#     filter(deleted == FALSE) %>%
#     # take out all that are not core variables, can be modified if someone wants to analyze questionnaire vars
#     select_if( !(names(.) %in% c('dateRanges','addresses','classificationHistory','vaccinesReceived','documents','relationshipsRepresentation','followUpHistory'))) %>%
#     select(-contains("questionnaireAnswers")) %>%
#     # unnest commonly used fields
#     #unnest_wider(addresses, names_sep = "_") %>%
#     #unnest_wider(followUpHistory, names_sep = "_")
#     left_join(addresses_contacts_of_contacts %>% filter(addresses_typeId=="USUAL_PLACE_OF_RESIDENCE"), by="id")  
#   
#   # force bind other core vars in case they are missing in JSON from having never been answered
#   missing_columns_contacts <- setdiff(contact_columns, names(contacts_clean))
#   contacts_clean[missing_columns_contacts] <- NA
#   contacts_clean <- contacts_clean[contact_columns]
#   contacts_clean[contacts_clean == "NA"] <- NA
#   
#   contacts_clean <- contacts_clean %>%
#     # unnest location IDs if more than 1 is documented
#     # unnest(addresses_locationId) %>%
#     # unnest(addresses_city) %>%
#     # clean date formats
#     mutate_at(vars(all_of(date_fields_contacts)), list(~substr(., 1, 10)), ~as.Date) %>%  
#     mutate(date_of_reporting = dateOfReporting,
#            date_of_data_entry = createdAt,
#            date_of_last_contact = dateOfLastContact, 
#            date_of_followup_start = followUp.startDate,
#            date_of_followup_end = followUp.endDate) %>%
#     # format and combine age variables
#     mutate(age_years = as.numeric(age.years)) %>%
#     mutate(age_years = na_if(age_years,0)) %>%
#     mutate(age_months = as.numeric(age.months)) %>%
#     mutate(age_months = na_if(age_months,0)) %>%
#     mutate(age = case_when(
#       is.na(age_years) && !is.na(age_months) ~  age_months / 12,
#       TRUE ~ age_years)) %>%
#     
#     mutate(
#       age = as.numeric(age),
#       age_class = factor(
#         case_when(
#           age <= 4 ~ "0-4",
#           age <= 9 ~ "5-9",
#           age <= 14 ~ "10-14",
#           age <= 19 ~ "15-19",
#           age <= 29 ~ "20-29",
#           age <= 39 ~ "30-39",
#           age <= 49 ~ "40-49",
#           age <= 59 ~ "50-59",
#           age <= 64 ~ "60-64",
#           age <= 69 ~ "65-69",
#           age <= 74 ~ "70-74",
#           age <= 79 ~ "75-79",
#           is.finite(age) ~ "80+",
#           TRUE ~ "unknown"
#         ), levels = c(
#           "0-4",
#           "5-9",
#           "10-14",
#           "15-19",
#           "20-29",
#           "30-39",
#           "40-49",
#           "50-59",
#           "60-64",
#           "65-69",
#           "70-74",
#           "75-79",
#           "80+",
#           "unknown"
#         )),
#       age_class = factor(
#         age_class,
#         levels = rev(levels(age_class)))) %>%
#     
#     # select only core variables
#     select(
#       uuid = id,
#       contact_id = visualId,
#       active,
#       contact_status = followUp.status,
#       location_id = addresses_locationId,
#       lat = addresses_geoLocation.lat,
#       long = addresses_geoLocation.lng,
#       address = addresses_addressLine1,
#       postal_code = addresses_postalCode,
#       team_id = followUpTeamId,
#       city = addresses_city,
#       firstName,
#       lastName,
#       gender,
#       age,
#       age_class,
#       occupation,
#       telephone = addresses_phoneNumber,
#       pregnancy_status = pregnancyStatus,
#       date_of_reporting,
#       date_of_data_entry,
#       date_of_last_contact,
#       date_of_followup_start,
#       date_of_followup_end,
#       risk_level = riskLevel,
#       risk_reason = riskReason,
#       was_case = wasCase,
#       createdBy
#       
#     ) %>%
#     
#     mutate(gender = sub(".*GENDER_", "", gender)) %>%
#     mutate(occupation = sub(".*OCCUPATION_", "", occupation)) %>%
#     mutate(contact_status = sub(".*TYPE_", "", contact_status)) %>%
#     mutate(pregnancy_status = sub(".*STATUS_", "", pregnancy_status)) %>%
#     mutate(risk_level = sub(".*LEVEL_", "", risk_level)) %>%
#     mutate(address = case_when(address == "NA" ~ NA)) %>%
#     mutate(address = case_when(postal_code == "NA" ~ NA)) %>%
#     
#     #Join in cases that used to be contacts
#     bind_rows(contacts_becoming_cases) %>%
#     
#     # join in location hierarchy
#     left_join(locations_clean %>% select(!(admin_level:long)), by="location_id")
#   
#   
#   
# }

##########################################################################
# Clean & Un-nest LAB RESULTS
##########################################################################

# can be further cleaned

if (nrow(lab_results)==0) {
  lab_results_clean <- data.frame(matrix(ncol=length(lab_results_columns_final)))
  colnames(lab_results_clean) <- lab_results_columns_final
} else {
  
  lab_results_clean <- lab_results %>%
    filter(deleted==FALSE) %>%
    select_if(negate(is.list))
  
}

##########################################################################
# Clean & Un-nest RELATIONSHIPS
##########################################################################

#Only process data if the has more than 0 relationships, otherwise generate an empty relationships_clean dataset
if (nrow(relationships) == 0) {
  relationships_clean <- data.frame(matrix(ncol=length(relationships_columns_final)))
  colnames(relationships_clean) <- relationships_columns_final 
} else {
  
  date_fields_relationships <- c("Created at","Updated on","Deleted at","Date of last contact")
  
  relationships_clean <- relationships %>%
    filter(Deleted == FALSE)  %>%  
    mutate_at(vars(all_of(date_fields_relationships)), list(~substr(., 1, 10)), ~as.Date) %>%  
    mutate(date_of_last_contact = `Date of last contact`,
           date_of_data_entry = `Created at`) %>%
    select(
      uuid = ID,
      source_uuid = Source.UID,
      source_visualid = `Source.Case / Contact ID`,
      source_gender = Source.Gender,
      date_of_last_contact,
      date_of_data_entry,
      source_age = `Source.Age.Age / Years`,
      target_uuid = Target.UID,
      target_visualid = `Target.Case / Contact ID`,
      target_gender = Target.Gender,
      target_age = `Target.Age.Age / Years`,
      exposure_type = `Exposure type`,
      context_of_exposure = `Context of Exposure`,
      exposure_frequency = `Exposure frequency`,
      certainty_level = `Certainty level`,
      exposure_duration = `Exposure duration`,
      relation_detail = `Relation detail`,
      cluster = Cluster,
      is_contact_date_estimated = `Is contact date estimated?`,
      comment = Comment,
      createdBy = `Created by`
      
    ) %>%
    mutate_if(is.character, list(~na_if(.,"")))
  
  relationships_clean[relationships_clean == ""] <- NA
  
}


##########################################################################
# Clean & Un-nest TEAMS
##########################################################################

#Only process data if the has more than 0 teams, otherwise generate an empty teams_clean dataset
if (nrow(teams) == 0) {
  teams_clean <- data.frame(matrix(ncol=length(teams_columns_final)))
  colnames(teams_clean) <- teams_columns_final 
} else {
  
  teams_clean <- teams %>%
    filter(deleted == FALSE) %>%
    unnest(userIds, keep_empty = TRUE) %>%
    unnest(locationIds, keep_empty = TRUE) %>%
    select(uuid = id,
           name,
           user_id = userIds,
           location_id = locationIds
    )
  
}

##########################################################################
## Clean & Un-nest USERS 
##########################################################################

#Only process data if the has more than 0 teams, otherwise generate an empty teams_clean dataset
if (nrow(users) == 0) {
  users_clean <- data.frame(matrix(ncol=length(users_columns_final)))
  colnames(users_clean) <- users_columns_final 
} else {
  
  users_clean <- users %>%
    filter(deleted == FALSE) %>%
    # filter(activeOutbreakId == outbreak_id) %>%
    unnest_wider(roleIds, names_sep = "_") %>%
    # clean_data(guess_dates = FALSE) %>%
    select(uuid = id,
           firstname = firstName,
           lastname = lastName,
           email = email
    )
  
}


##########################################################################
## some other additions for handy things to have in linelist .csv
##########################################################################

contacts_per_case <- relationships_clean %>%
  group_by(source_visualid, source_uuid) %>%
  tally() %>%
  select(source_visualid, 
         source_uuid,
         contacts_per_case = n)


# cases linelist, now with contacts per case (can add other vars here as needed)

cases_clean <- cases_clean %>%
  left_join(contacts_per_case, by = c("uuid" = "source_uuid")) %>%
  mutate(contacts_per_case = replace(contacts_per_case, is.na(contacts_per_case),0))



###############################################################
## Export Dataframes(to be overwritten each time script is run) 
###############################################################

rm(contacts_address_history_clean)
rm(cases_address_history_clean)
rm(cases_hosp_history_clean)

## Specify location to save files
data_folder <- here::here("data")

## specify data frames to export
mydfs<- ls(pattern = "_clean")
mydfs

## export files as .csv
for (i in 1:length(mydfs)){
  savefile<-paste0(data_folder,"/", mydfs[i], ".csv")
  write.csv(get(mydfs[i]), file=savefile, fileEncoding = "UTF-8", na="", row.names = F)
  
  print(paste("Dataframe Saved:", mydfs[i]))
}

## export all as .rds files which we will use for report scripts as it preserves language characters better
for (i in 1:length(mydfs)){
  savefile<-paste0(data_folder,"/", mydfs[i], ".rds")
  saveRDS(get(mydfs[i]), file=savefile)
  print(paste("Dataframe Saved:", mydfs[i]))
}
