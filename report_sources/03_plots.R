# Plots - epi curves for case and contact registration

# load data

cases <- rio::import(here::here("data", "clean", "cases_clean.rds")) %>%
  as_tibble()

contacts <- rio::import(here::here("data", "clean", "contacts_clean.rds")) %>%
  as_tibble()

contacts_of_contacts <- rio::import(here::here("data", "clean", "contacts_of_contacts_clean.rds")) %>%
  as_tibble()

relationships <- rio::import(here::here("data", "clean", "relationships_clean.rds")) %>%
  as_tibble()

lab_results <- rio::import(here::here("data", "clean", "lab_results_clean.rds")) %>% as_tibble() %>%
  as_tibble()

events <- rio::import(here::here("data", "clean", "events_clean.rds")) %>%
  as_tibble()

followups <- rio::import(here::here("data", "clean", "followups_clean.rds")) %>%
  as_tibble()

users <- rio::import(here::here("data", "clean", "users_clean.rds")) %>%
  as_tibble()

teams <- rio::import(here::here("data", "clean", "teams_clean.rds")) %>%
  as_tibble()

locations <- rio::import(here::here("data", "clean", "locations_clean.rds")) %>%
  as_tibble()

################################################################################

# epicurves, date of reporting

