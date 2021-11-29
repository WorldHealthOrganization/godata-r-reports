# Epi curves for cases by day and week
######################################################################################################

pacman::p_load(
  tidyverse,    # includes many packages for tidy data wrangling and presentation
  janitor, # tables and data cleaning
  here,
  rio,
  lubridate,
  htmlwidgets,
  flexdashboard,
  formattable,
  kableExtra,
  RColorBrewer, 
  viridis,
  qdapTools,
  incidence2
)



# load data


cases <- rio::import(here::here("data", "clean", "cases_clean.rds")) %>%
  as_tibble()

relationships <- rio::import(here::here("data", "clean", "relationships_clean.rds")) %>%
  as_tibble()


followups <- rio::import(here::here("data", "clean", "followups_clean.rds")) %>%
  as_tibble()

users <- rio::import(here::here("data", "clean", "users_clean.rds")) %>%
  as_tibble()

teams <- rio::import(here::here("data", "clean", "teams_clean.rds")) %>%
  as_tibble()

locations <- rio::import(here::here("data", "clean", "locations_clean.rds")) %>%
  as_tibble()

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

# counts

case_with_age_and_sex <- sum(!is.na(cases$gender) & cases$age_class != "unknown") 

# set time periods

database_date <- Sys.Date()  
database_week <- isoweek(database_date)
prev_1_date <- database_date - 1
prev_7_date <- prev_1_date - 7
before_prev_7_date <- prev_7_date -7


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
       xlab = "Day of reporting",              # x-axis label
       ylab = " ",
       show_cases = FALSE,                    # show each case as an individual box
       alpha = 0.7,                          # transparency 
       n.breaks = 20,
       border = "grey",                      # box border
       angle = 45,                           # angle of date labels
       centre_dates = FALSE,                 # date labels at edge of bar
       date_format = "%d %b %Y") + # adjust how dates are displayed 
  
  scale_y_continuous(expand = c(0,0)) +      # y-axis
  
  theme_classic() +
  
  labs(                               
    subtitle = stringr::str_glue(                            
      "n = {total_cases}",
      total_cases = nrow(cases)
    )) +
  
  theme(
    legend.position = "top",
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=14, face = "bold"),
    #axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size=11, face = "italic"),
    panel.grid = element_blank(),
    axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
    panel.grid.major.y = element_line(size = 0.2, colour = "grey")) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


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
         ylab = " ",
         show_cases = FALSE,                    # show each case as an individual box
         alpha = 0.7,                          # transparency 
         n.breaks = 20,
         border = "grey",                      # box border
         angle = 45,                           # angle of date labels
         centre_dates = FALSE,                 # date labels at edge of bar
         date_format = "%d %b %Y") + # adjust how dates are displayed 
    scale_y_continuous(expand = c(0,0)) +      # y-axis
    
    theme_classic() +
    
    labs(                               
      subtitle = stringr::str_glue(                            
        "n = {total_with_onset} with date of onset",
        total_with_onset = nrow(cases %>% filter(!is.na(date_of_onset)))
      )) +
    theme(
      legend.position = "top",
      legend.title = element_text(size=10, face = "bold"),
      plot.title = element_text(size=14, face = "bold"),
      #axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size=11, face = "italic"),
      panel.grid = element_blank(),
      axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
      panel.grid.major.y = element_line(size = 0.2, colour = "grey")) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
  # create the incidence object, aggregating cases by week
  epi_week_of_onset <- incidence(       # create incidence object
    x = cases,             # dataset
    date_index = date_of_onset,  # date column
    groups = classification,
    interval = "week"          # date grouping interval
  )
  
  epicurve_week_of_onset <-
    plot(epi_week_of_onset,
         fill = classification,                       # box/bar color,
         legend = "top",                       # legend on top
         title = "Epidemic curve",  # title
         xlab = "Week of onset",               # x-axis label
         ylab = "",
         show_cases = FALSE,                    # show each case as an individual box
         alpha = 0.7,                          # transparency 
         n.breaks = 10,
         border = "grey",                      # box border
         angle = 45,                           # angle of date labels
         centre_dates = FALSE,                 # date labels at edge of bar
         date_format = "%d %b %Y") + # adjust how dates are displayed 
    scale_y_continuous(expand = c(0,0)) +      # y-axis
    
    theme_classic() +
    
    labs(                               
      subtitle = stringr::str_glue(                            
        "n = {total_with_onset} with date of onset",
        total_with_onset = nrow(cases %>% filter(!is.na(date_of_onset)))
        )) +
    theme(
      legend.position = "top",
      legend.title = element_text(size=10, face = "bold"),
      plot.title = element_text(size=14, face = "bold"),
      #axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size=11, face = "italic"),
      panel.grid = element_blank(),
      axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
      panel.grid.major.y = element_line(size = 0.2, colour = "grey")) 
    # theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


  
  # age breakdown of cases over time
  
  cases_per_week <- cases %>%
    filter(age_class != "unknown") %>%
    mutate(iso_week = isoweek(date_of_reporting)) %>%
    group_by(iso_week) %>%
    summarise(weekly_total = n()) 
  
  case_age_breakdown_over_time <- cases %>%
    mutate(iso_week = isoweek(date_of_reporting)) %>%
    filter(age_class != "unknown") %>%
    mutate(week_of_reporting = as.Date(cut(date_of_reporting, breaks = "week"))) %>%
    group_by(week_of_reporting, iso_week, age_class) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    inner_join(cases_per_week, by = "iso_week") %>%
    mutate(prop = count/weekly_total*100)
  
  case_age_breakdown_over_time_plot <- 
    ggplot(case_age_breakdown_over_time, aes(x = week_of_reporting, y = count, fill = age_class )) +
    geom_col() +
    scale_fill_viridis(discrete = T, option = "magma") +
    scale_x_date(date_breaks = "2 weeks",
                 date_labels = "%b %d",
                 limits = c(min(case_age_breakdown_over_time$week_of_reporting), max(case_age_breakdown_over_time$week_of_reporting))) +
    ylim(0,NA) +
    theme_minimal() +
    labs(x = "",
         y = "",
         fill = "Age group",
         title = "Age breakdown of COVID-19 cases by reporting week",
         subtitle = stringr::str_glue(                            
           "n = {total_case_count} 
        {missing_age} cases are missing age and not shown",
           total_case_count = nrow(cases),
           missing_age = nrow(cases %>% filter(age_class == "unknown")))) +
    theme(
      legend.title = element_text(size=10, face = "bold"),
      plot.title = element_text(size=12, face = "bold"),
      axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size=11),
      plot.caption = element_text(size = 8, face = "italic"),
      panel.grid.major.y = element_line(size = 0.2, colour = "grey"),
      panel.grid = element_blank())
  
  
  # age / sex pyramid of contacts
  
  case_age_sex_breakdown <- cases %>%
    filter(age_class != "unknown") %>%
    filter(!is.na(gender)) %>%
    group_by(gender, age_class) %>%
    summarise(num = n()) %>%
    ungroup() %>%
    mutate(prop = num / sum(num) * 100)
  drop_levels <- levels(droplevels(case_age_sex_breakdown$age_class))
  
  age_pyramid_case <- 
    ggplot(data = case_age_sex_breakdown, aes(x = age_class, fill = gender)) + 
    geom_bar(data = subset(case_age_sex_breakdown, gender == "FEMALE"), aes(y = prop),
             stat = "identity",width=1,  alpha = 0.6, col = "white") +
    geom_bar(data = subset(case_age_sex_breakdown, gender == "MALE"),
             stat = "identity",width=1,  aes(y = prop*(-1)), alpha = 0.6, col = "white") +
    coord_flip() + 
  scale_fill_manual(
    values=c(
      "MALE" = "#00bfbf", 
      "FEMALE" = "#005252"),
    guide = guide_legend(reverse = TRUE)
  ) + 
    
    theme_classic() +
    
    labs(y = "Percent of total",
         x = "Age group",
         fill = "Sex",
         title = "Age/sex pyramid of COVID-19 case",
         subtitle = paste0(round(sum(case_age_sex_breakdown$prop[case_age_sex_breakdown$gender == "MALE"]),digits=1),"% Male, ",
                           round(sum(case_age_sex_breakdown$prop[case_age_sex_breakdown$gender == "FEMALE"]),digits=1)," % Female\n",
                           "(n = ", case_with_age_and_sex, " with both age and sex recorded)"
         )) +
    
    scale_x_discrete(limits = rev(drop_levels)) +
    theme(
      legend.title = element_text(size=10, face = "bold"),
      plot.title = element_text(size=12, face = "bold"),
      axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
      axis.title.y = element_text(size = 12),
      plot.subtitle = element_text(size=11),
      plot.caption = element_text(size = 8, face = "italic"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()) 
  
  
  
  cases$occupation <- as.factor(cases$occupation)
  case_unknown_occupation <- sum(is.na(cases$occupation) | cases$occupation == "UNKNOWN")
  case_other_occupation <- sum(cases$occupation == "OTHER")   ### why is this not working??
  case_occupation_breakdown <- cases %>%
    # filter(!is.na(occupation),
    #        occupation != "OTHER") %>%
    group_by(occupation) %>%
    count() %>%
    arrange(desc(n)) 
  # case_occupation_breakdown_plot_freq <- 
  #   ggplot(subset(case_occupation_breakdown, !is.na(occupation)), aes(x = occupation, y = n)) +
  #   geom_bar(stat = "identity") +
  #   coord_flip() +
  #   theme_classic()
  color_count = length(unique(cases$occupation))
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  case_occupation_pie_chart <-
    ggplot(subset(case_occupation_breakdown, !is.na(occupation) & !(occupation == "OTHER")), aes(x = "", y = n, fill = occupation)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_minimal() +
    scale_fill_manual(values = getPalette(color_count)) +
    labs(
      fill = "Occupation",
      title = "Occupational breakdown of COVID-19 cases",
      subtitle = paste0("n = ", case_unknown_occupation," without occupation recorded,", " n = ", case_other_occupation," where 'OTHER'")
    ) +
    theme(axis.line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          legend.position = "right",
          legend.title = element_text(size=10, face = "bold"),
          plot.title = element_text(size=12, face = "bold"),
          plot.subtitle = element_text(size=11),
          plot.caption = element_text(size = 8, face = "italic"))
  #############################################################
  ### high risk groups
  ############################################################
  cases_high_risk <- cases %>%
    mutate(
      health_worker_yes = case_when(occupation == "HEALTH_CARE_WORKER" |
                                      occupation == "HEALTH_LABORATORY_WORKER" ~ TRUE,
                                    TRUE ~ FALSE),
      health_worker_unknown = case_when(occupation == "UNKNOWN" |
                                          is.na(occupation) ~ TRUE,
                                        TRUE ~ FALSE),
      
      pregnant_yes = case_when(str_detect(pregnancy_status, "YES_") ~ TRUE,
                               TRUE ~ FALSE),
      
      pregnant_unknown = case_when(is.na(pregnancy_status) ~ TRUE,
                                   TRUE ~ FALSE),
      
      high_risk_yes = case_when(risk_level == "3_HIGH" ~ TRUE,
                                TRUE ~ FALSE),
      
      risk_unknown = case_when(is.na(risk_level) ~ TRUE,
                               TRUE ~ FALSE)
      
    ) %>%
    summarize(health_worker_yes = sum(health_worker_yes),
              health_worker_unknown = sum(health_worker_unknown),
              pregnant_yes = sum(pregnant_yes),
              pregnant_unknown = sum(pregnant_unknown),
              high_risk_yes = sum(high_risk_yes),
              risk_unknown = sum(risk_unknown),
              total = nrow(cases)) %>%
    mutate(
      health_worker_no = total - health_worker_yes - health_worker_unknown,
      pregnant_no = total - pregnant_yes - pregnant_unknown,
      high_risk_no = total - high_risk_yes - risk_unknown) %>%
    pivot_longer(-total,
                 names_to = "category", values_to = "count") %>%
    
    mutate(risk_factor = case_when(str_detect(category,"health_worker") ~ "Health Worker",
                                   str_detect(category,"pregnant") ~ "Pregnant",
                                   str_detect(category,"risk") ~ "Risk Level = `High`")
    ) %>%
    mutate(category = case_when(str_detect(category,"_yes") ~ "Yes",
                                str_detect(category,"_no") ~ "No",
                                str_detect(category,"_unknown") ~ "Unknown")
    ) %>%
    
    mutate(prop = (count/total*100)) %>%
    arrange(risk_factor, category)
  cases_high_risk$category<-factor(cases_high_risk$category, levels=c('Unknown','No','Yes'), ordered = TRUE)
  plot_cases_high_risk <- 
    ggplot(cases_high_risk, aes(x = risk_factor, y = count, fill = category)) + 
    geom_col() +
    coord_flip() +
    labs(x = " ",
         y = "# of cases",
         title = "Groups of interest among registered cases",
         subtitle = "Proportion of records with relevant fields marked YES") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 11)) +
    scale_fill_manual(
      values=c(
        "Yes" = "#ff5c33", 
        "No" = "#c2d6d6",
        "Unknown" = custom_grey0),
      guide = guide_legend(reverse = TRUE))  
  
  
  
################################################
# plot case summary table showing by region - # died, recovered, hosp, vacc, iso, etc.

  tab_case_status <- cases %>%
    group_by(admin_1_name) %>%
    summarize(
      total_cases = n(),
      cases_last7d = sum(date_of_reporting >= prev_7_date),
      cases_prev_last7d = sum(date_of_reporting >= before_prev_7_date & date_of_reporting < prev_7_date),
      died = sum(outcome == "DECEASED"),
      recovered = sum(outcome == "RECOVERED"),
      no_contacts_listed = sum(no_contacts == 0),
      without_epi_link = sum(!known_epi_link), 
      from_contact_list = sum(was_contact),
      vaccinated = sum(vaccinated),
      isolated = sum(isolated),
      hospitalized = sum(hospitalized),
      icu = sum(icu)
    ) %>%
    
    mutate(
      perc_change_7d_cases = round((cases_last7d - cases_prev_last7d)/cases_prev_last7d*100,digits=1)) %>%
    arrange(admin_1_name) %>%
    select(
      `Admin 1` = admin_1_name,
      `Total cases` = total_cases,
      `Last 7d` = cases_last7d,
      `% Weekly Change` = perc_change_7d_cases,
      `Without epi link` = without_epi_link,
      `Zero contacts listed` = no_contacts_listed,
      `Isolated` = isolated,
      `Hospitalized` = hospitalized,
      `Died` = died,
      `Recovered` = recovered,
      `Vaccinated` = vaccinated
    ) 
  
  formattable_tab_case_status <-tab_case_status %>%
    
    mutate(
      `Admin 1` = formatter("span", style = ~ formattable::style(
        color = ifelse(`Admin 1` == "None listed", "red", "grey"),
        font.weight = "bold",font.style = "italic"))(`Admin 1`),
      `Last 7d` = color_bar("#355C7D")(`Last 7d`),
      `Zero contacts listed`= color_tile("white", "red")(`Zero contacts listed`)
    ) %>%
    
    kable("html", escape = F, align =c("l","c","c","c","c","c","c","c","c","c","c")) %>%
    kable_styling("hover", full_width = FALSE) %>%
    add_header_above(c(" " = 4, 
                       "Case investigation" = 2,
                       "Hospitalization and outcome" = 5))
  
  formattable_tab_case_status
  
  
  ### plot contacts per case
  
  plot_contacts_per_case <- cases %>%
    ggplot(aes(x = no_contacts)) +
    geom_histogram(fill = contact_green) +
    scale_y_continuous(expand = c(0,0))+   # remove excess space below 0 on y-axis
    
  labs(                               
      subtitle = stringr::str_glue(                            
        "Median = {median_contacts_per_case}",
        median_contacts_per_case = median(cases$no_contacts)
      ),
      title = "Number of contacts listed per case",
      y = "Frequency",
      x = "Contacts per case"
        ) +
    theme_classic() +
    theme(
      legend.title = element_text(size=10, face = "bold"),
      plot.title = element_text(size=14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size=11, face = "italic"),
      panel.grid = element_blank(),
      axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
      panel.grid.major.y = element_line(size = 0.2, colour = "grey")) +
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
  
