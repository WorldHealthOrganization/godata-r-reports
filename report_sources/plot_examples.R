# Plots - epi curves for case and contact registration

# required packages
library(pacman)
library(incidence2)

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

# set themes

scale_classification <- scale_fill_manual(
  "Classification",
  values = c(CONFIRMED             = "#F55927",
             NOT_A_CASE_DISCARDED  = "#DADAD9",
             PROBABLE              = "#5C77B6",
             SUSPECT               = "#6CBBC6"),
  
  labels = c(CONFIRMED             = "Confirmed",
             NOT_A_CASE_DISCARDED  = "Not A Case",
             PROBABLE              = "Probable",
             SUSPECT               = "Suspect"))


scale_test <- scale_fill_manual(
  "Tests",
  values = c(POSITIVE        = "#F55927",
             NEGATIVE    = "#DADAD9",
             PENDING = "#F6DDCC",
             INCONCLUSIVE = "#FBEEE6"
             
  ),
  
  labels = c(POSITIVE        = "Positive",
             NEGATIVE     = "Negative",
             PENDING     = "Pending",
             INCONCLUSIVE     = "Unknown/Inconclusive"
             
  ))


scale_day <- ggplot2::scale_x_date(breaks = "3 days",
                                   expand=c(0,0),
                                   date_label = format("%d %b %Y"))
#########################################################################################

# epicurve, date of reporting


# create the incidence object, aggregating cases by day
epi_date_of_reporting <- incidence(       # create incidence object
  x = cases,             # dataset
  date_index = date_of_reporting,  # date column
  groups = classification,
  interval = "day"          # date grouping interval
)

epicurve_date_of_reporting <-
  plot(epi_date_of_reporting,
       fill = classification,                       # box/bar color,
       legend = "top",                       # legend on top
       title = "Epidemic curve by Date of Reporting",  # title
       xlab = "Day of onset",               # x-axis label
       show_cases = FALSE,                    # show each case as an individual box
       alpha = 0.7,                          # transparency 
       n.breaks = 20,
       border = "grey",                      # box border
       angle = 45,                           # angle of date labels
       centre_dates = FALSE,                 # date labels at edge of bar
       date_format = "%d %b %Y") + # adjust how dates are displayed 
  
  scale_y_continuous(expand = c(0,0)) +      # y-axis
  
  labs(subtitle = paste0("(total n = ",nrow(cases),")")) +
  theme(
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=12, face = "bold"),
    #axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size=11),
    plot.caption = element_text(size = 8, face = "italic"),
    panel.grid = element_blank(),
    axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
    panel.grid.major.y = element_line(size = 0.2, colour = "grey"))



# epicurve, date of onset

# create the incidence object, aggregating cases by day
  epi_date_of_onset <- incidence(       # create incidence object
    x = cases,             # dataset
    date_index = date_of_onset,  # date column
    groups = classification,
    interval = "day"          # date grouping interval
  )
  
  epicurve_date_of_onset <-
    plot(epi_date_of_onset,
         fill = classification,                       # box/bar color,
         legend = "top",                       # legend on top
         title = "Epidemic curve by Date of Onset",  # title
         xlab = "Day of onset",               # x-axis label
         show_cases = FALSE,                    # show each case as an individual box
         alpha = 0.7,                          # transparency 
         n.breaks = 20,
         border = "grey",                      # box border
         angle = 45,                           # angle of date labels
         centre_dates = FALSE,                 # date labels at edge of bar
         date_format = "%d %b %Y") + # adjust how dates are displayed 
    scale_y_continuous(expand = c(0,0)) +      # y-axis
    
    labs(                               
      subtitle = stringr::str_glue(                            
        "n = {total_case_count} 
        {missing_onset} cases are missing date of onset and not shown",
        total_case_count = nrow(cases),
        missing_onset = nrow(cases %>% filter(is.na(date_of_onset))))) +
    theme(
      legend.title = element_text(size=10, face = "bold"),
      plot.title = element_text(size=12, face = "bold"),
      #axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size=11),
      plot.caption = element_text(size = 8, face = "italic"),
      panel.grid = element_blank(),
      axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
      panel.grid.major.y = element_line(size = 0.2, colour = "grey"))

# registration of contacts, by date of reporting
  
  # create the incidence object
  contacts_by_reg <- incidence(       # create incidence object
    x = contacts,             # dataset
    date_index = date_of_reporting,  # date column
    interval = "day"          # date grouping interval
  )
  
  plot_contacts_by_reg <-
    plot(contacts_by_reg,                       # box/bar color,
         legend = "top",                       # legend on top
         title = "Contacts Listed by Date of Reporting",  # title
         xlab = "Day of reporting",               # x-axis label                   
         alpha = 0.7,                          # transparency 
         n.breaks = 20,
         border = "grey",                      # box border
         angle = 45,                           # angle of date labels
         centre_dates = FALSE,                 # date labels at edge of bar
         date_format = "%d %b %Y") + # adjust how dates are displayed 
    scale_y_continuous(expand = c(0,0)) +      # y-axis
    
    labs(                               
      subtitle = stringr::str_glue(                            
        "n = {total_contact_count}",
        total_contact_count = nrow(contacts))) +
    theme(
      legend.title = element_text(size=10, face = "bold"),
      plot.title = element_text(size=12, face = "bold"),
      #axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size=11),
      plot.caption = element_text(size = 8, face = "italic"),
      panel.grid = element_blank(),
      axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
      panel.grid.major.y = element_line(size = 0.2, colour = "grey"))
  

# registration of events, by date of reporting
  
  # create the incidence object
  events_by_event_date <- incidence(       # create incidence object
    x = events,             # dataset
    date_index = date,  # date column
    groups = name,
    interval = "day"          # date grouping interval
  )
  
  plot_events_by_event_date <-
    plot(events_by_event_date,                       # box/bar color,
         fill = name,
         legend = "top",                       # legend on top
         title = "Events by Date of Event",  # title
         xlab = "Day of event",               # x-axis label                   
         alpha = 0.7,                          # transparency 
         n.breaks = 20,
         border = "grey",                      # box border
         angle = 45,                           # angle of date labels
         centre_dates = FALSE,                 # date labels at edge of bar
         date_format = "%d %b %Y") + # adjust how dates are displayed 
    
    scale_y_continuous(expand = c(0,0)) +      # y-axis
    
    labs(                               
      subtitle = stringr::str_glue(                            
        "n = {total_event_count}",
        total_event_count = nrow(events))) +
    theme(
      legend.title = element_text(size=10, face = "bold"),
      plot.title = element_text(size=12, face = "bold"),
      #axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size=11),
      plot.caption = element_text(size = 8, face = "italic"),
      panel.grid = element_blank(),
      axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
      panel.grid.major.y = element_line(size = 0.2, colour = "grey"))


# registration of lab results, by date of reporting
  
  
  tests_by_day <- lab_results %>%
    group_by(date_sample_taken) %>%
    summarise(POSITIVE = sum(result == "POSITIVE"),
              NEGATIVE = sum(result == "NEGATIVE"),
              INCONCLUSIVE = sum(result == "INCONCLUSIVE"),
              TOTAL = sum(result == "INCONCLUSIVE" | result == "POSITIVE" | result == "NEGATIVE"),
              PERC_POS = POSITIVE / TOTAL) %>%
    select(-TOTAL) %>%
    pivot_longer(-c(date_sample_taken,PERC_POS),
                 names_to = "result", 
                 values_to = "count") 
  
  
  epicurve_test_plot <-
    ggplot(tests_by_day, aes(x= date_sample_taken, y = count, fill = result)) +
    geom_col() +
    scale_test +
    theme(
      axis.title.y = element_text(color = "grey"),
      axis.title.y.right = element_text(color = "blue")) +
    scale_day +
    scale_y_continuous(expand = c(0,0)) +      # y-axis
    theme_classic() +
    # rotate_x_text(45) +
    labs(title = "Tests Performed and % Positivity",
         subtitle = "Persons tested (either RDT or PCR) who had a final positive result",
         x = "Day of sample collection",
         y = " ") +
    theme(
      legend.title = element_text(size=10, face = "bold"),
      legend.position = "top",
      plot.title = element_text(size=12, face = "bold"),
      axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size=11),
      plot.caption = element_text(size = 8, face = "italic"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()) +
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
  epicurve_test_plot 
  
  
# demographics - case
  

# demographics - contacts

# data completion

