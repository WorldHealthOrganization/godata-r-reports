# Plot contacts and demographics
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

contacts <- rio::import(here::here("data", "clean", "contacts_clean.rds")) %>%
  as_tibble()

contacts_of_contacts <- rio::import(here::here("data", "clean", "contacts_of_contacts_clean.rds")) %>%
  as_tibble()

relationships <- rio::import(here::here("data", "clean", "relationships_clean.rds")) %>%
  as_tibble()

followups <- rio::import(here::here("data", "clean", "followups_clean.rds")) %>%
  as_tibble()



# format

custom_grey0 = "#ADADAD"
custom_grey = "#818181"
contact_green = "#02956D"

# counts
contact_unknown_age <- sum(contacts$age == "unknown")
contact_unknown_sex <- sum(is.na(contacts$gender))
contact_with_age_and_sex <- sum(!is.na(contacts$gender) & contacts$age_class != "unknown") 

total_contacts_reg <- nrow(contacts)


# registration of contacts, by date of reporting

# create the incidence object
contacts_by_day <- incidence(       # create incidence object
  x = contacts,             # dataset
  date_index = date_of_reporting,  # date column
  groups = follow_up_status,
  interval = "day"          # date grouping interval
)

plot_contacts_by_day <-
  plot(contacts_by_day,                       # box/bar color
       fill = follow_up_status,
       legend = "top",                       # legend on top
       title = "Contacts Listed",  # title
       xlab = "Day of reporting",               # x-axis label 
       ylab = " ",
       alpha = 0.7,                          # transparency 
       n.breaks = 10,
       border = "grey",                      # box border
       angle = 45,                           # angle of date labels
       centre_dates = FALSE,                 # date labels at edge of bar
       date_format = "%d %b %Y") + # adjust how dates are displayed 
  scale_y_continuous(expand = c(0,0)) +      # y-axis
  
  
  labs(                               
    subtitle = stringr::str_glue(                            
      "n = {total_contact_count}",
      total_contact_count = nrow(contacts))) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size=11, face = "italic"),
    panel.grid = element_blank(),
    axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
    panel.grid.major.y = element_line(size = 0.2, colour = "grey")) +
theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))




# create the incidence object, contacts by week
contacts_by_week <- incidence(       # create incidence object
  x = contacts,             # dataset
  date_index = date_of_reporting,  # date column
  groups = follow_up_status,
  interval = "week"          # date grouping interval
)

plot_contacts_by_week <-
  plot(contacts_by_week,
       # fill = follow_up_status,             # box/bar color,
       fill = contact_green,
       legend = "top",                       # legend on top
       title = "Contacts Listed",  # title
       xlab = "Week of reporting",               # x-axis label 
       ylab = "",
       alpha = 0.7,                          # transparency 
       n.breaks = 10,
       border = "grey",                      # box border
       angle = 45,                           # angle of date labels
       centre_dates = FALSE,                 # date labels at edge of bar
       date_format = "%d %b %Y") + # adjust how dates are displayed 
  scale_y_continuous(expand = c(0,0)) +      # y-axis
  
  labs(                               
    subtitle = stringr::str_glue(                            
      "n = {total_contact_count}",
      total_contact_count = nrow(contacts))) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size=11, face = "italic"),
    panel.grid = element_blank(),
    axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
    panel.grid.major.y = element_line(size = 0.2, colour = "grey")) 
  # theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
 


# age breakdown of contacts over time

contacts_per_week <- contacts %>%
  filter(age_class != "unknown") %>%
  mutate(iso_week = isoweek(date_of_reporting)) %>%
  group_by(iso_week) %>%
  summarise(weekly_total = n()) 

# contacts$age_class_v2 <- fct_collapse(contacts$age_class, 
#                                       "0-4" = c("0-4"), 
#                                       "5-14" = c("5-9", "10-14"), 
#                                       "15-39" = c("15-19", "20-29","30-39"), 
#                                       "40-64" = c("40-49", "50-59","60-64"), 
#                                       "65-79" = c("65-69", "70-74","75-79"),
#                                       "80+" = "80+")

contact_age_breakdown_over_time <- contacts %>%
  mutate(iso_week = isoweek(date_of_reporting)) %>%
  filter(age_class != "unknown") %>%
  mutate(week_of_reporting = as.Date(cut(date_of_reporting, breaks = "week"))) %>%
  group_by(week_of_reporting, iso_week, age_class) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  inner_join(contacts_per_week, by = "iso_week") %>%
  mutate(prop = count/weekly_total*100)

contact_age_breakdown_over_time_plot <- 
  ggplot(contact_age_breakdown_over_time, aes(x = week_of_reporting, y = count, fill = age_class )) +
  geom_col() +
  scale_fill_viridis(discrete = T, option = "magma") +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d",
               limits = c(min(contact_age_breakdown_over_time$week_of_reporting), max(contact_age_breakdown_over_time$week_of_reporting))) +
  ylim(0,NA) +
  theme_minimal() +
  labs(x = "",
       y = "",
       fill = "Age group",
       title = "Age breakdown of COVID-19 contacts by reporting week",
       subtitle = stringr::str_glue(                            
         "n = {total_contact_count} 
        {missing_age} contacts are missing age and not shown",
         total_contact_count = nrow(contacts),
         missing_age = nrow(contacts %>% filter(age_class == "unknown")))) +
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

contact_age_sex_breakdown <- contacts %>%
  filter(age_class != "unknown") %>%
  filter(!is.na(gender)) %>%
  group_by(gender, age_class) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(prop = num / sum(num) * 100)
drop_levels <- levels(droplevels(contact_age_sex_breakdown$age_class))

age_pyramid_contact <- 
  ggplot(data = contact_age_sex_breakdown, aes(x = age_class, fill = gender)) + 
  geom_bar(data = subset(contact_age_sex_breakdown, gender == "FEMALE"), aes(y = prop),
           stat = "identity",width=1,  alpha = 0.6, col = "white") +
  geom_bar(data = subset(contact_age_sex_breakdown, gender == "MALE"),
           stat = "identity",width=1,  aes(y = prop*(-1)), alpha = 0.6, col = "white") +
  coord_flip() + 
  # annotate(
  #         geom = 'text', 
  #         label = paste0(round(sum(contact_age_sex_breakdown$prop[contact_age_sex_breakdown$gender == "MALE"])
  #                               ,digits=1),"% Male\n",
  #                        round(sum(contact_age_sex_breakdown$prop[contact_age_sex_breakdown$gender == "FEMALE"])
  #                                ,digits=1),"% Female"
  #                        ), 
  #          size = 5,
  #          x = -Inf, 
  #          y = Inf, 
  #         ) +
  scale_fill_manual(
    values=c(
      "MALE" = "#ffac81", 
      "FEMALE" = "#e26d5c"),
    guide = guide_legend(reverse = TRUE)
  ) + 

  theme_classic() +
  
  labs(y = "Percent of total",
       x = "Age group",
       fill = "Sex",
       title = "Age/sex pyramid of COVID-19 contacts",
       subtitle = paste0(round(sum(contact_age_sex_breakdown$prop[contact_age_sex_breakdown$gender == "MALE"]),digits=1),"% Male, ",
                         round(sum(contact_age_sex_breakdown$prop[contact_age_sex_breakdown$gender == "FEMALE"]),digits=1)," % Female\n",
                         "(n = ", contact_with_age_and_sex, " with both age and sex recorded)"
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

contacts$occupation <- as.factor(contacts$occupation)
contact_unknown_occupation <- sum(is.na(contacts$occupation) | contacts$occupation == "UNKNOWN")
contact_other_occupation <- sum(contacts$occupation == "OTHER")   ### why is this not working??
contact_occupation_breakdown <- contacts %>%
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
color_count = length(unique(contacts$occupation))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
contact_occupation_pie_chart <-
  ggplot(subset(contact_occupation_breakdown, !is.na(occupation) & !(occupation == "OTHER")), aes(x = "", y = n, fill = occupation)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  scale_fill_manual(values = getPalette(color_count)) +
  labs(
    fill = "Occupation",
    title = "Occupational breakdown of COVID-19 contacts",
    subtitle = paste0("n = ", contact_unknown_occupation," without occupation recorded,", " n = ", contact_other_occupation," where 'OTHER'")
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
contacts_high_risk <- contacts %>%
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
            total = nrow(contacts)) %>%
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
  
  mutate(prop = (count/total*100))
contacts_high_risk$category<-factor(contacts_high_risk$category, levels=c('Unknown','No','Yes'), ordered = TRUE)
plot_contacts_high_risk <- 
  ggplot(contacts_high_risk, aes(x = risk_factor, y = count, fill = category)) + 
  geom_col() +
  coord_flip() +
  labs(x = " ",
       y = "# of contacts",
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




### plot daily status
plot_daily_followup_status <-
  followups %>%
  ggplot() +
  geom_histogram(mapping = aes(x = date, fill = followup_status)) +
  scale_fill_discrete(drop = FALSE) +   # show all factor levels (followup_status) in the legend, even those not used
  theme_classic() +
  labs(
    subtitle = paste0("n = ",nrow(followups)," Follow-ups generated"),
    x = "",
    y = "Number of follow-ups ",
    title = "Daily Contact Follow-up Status",
    fill = "Follow-up Status") +
scale_y_continuous(expand = c(0,0)) +      # y-axis
  theme(
    legend.position = "top",
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size=11, face = "italic"),
    panel.grid = element_blank(),
    axis.ticks.y.left = element_line(size = 0.5, colour = "grey"),
    panel.grid.major.y = element_line(size = 0.2, colour = "grey")) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))







### 
plot_by_region <- 
  followups %>%                                        # begin with follow-up dataset
  count(admin_1_name, followup_status) %>%   # get counts by unique region-status (creates column 'n' with counts)
  
  # begin ggplot()
  ggplot(                                         # begin ggplot
    mapping = aes(x = reorder(admin_1_name, n),     # reorder admin factor levels by the numeric values in column 'n'
                  y = n,                            # heights of bar from column 'n'
                  fill = followup_status,           # color stacked bars by their status
                  label = n))+                      # to pass to geom_label()              
  geom_col()+                                     # stacked bars, mapping inherited from above 
  geom_text(                                      # add text, mapping inherited from above
    size = 3,                                         
    position = position_stack(vjust = 0.5), 
    color = "white",           
    check_overlap = TRUE,
    fontface = "bold")+
  coord_flip()+
  labs(
    x = "",
    y = "Number of contacts",
    title = "Contact Followup Status, by Region",
    fill = "Followup Status",
    subtitle = str_glue("Data as of {max(followups$date, na.rm=T)}")) +
  theme_classic()+                                                                      # Simplify background
  facet_wrap(~admin_1_name, strip.position = "right", scales = "free_y", ncol = 1)      # introduce facets 

plot_by_region



### plot contact tracing FU status, by region using contact follow up start and end dates

# x-axis is date, y-axis is total # of contact on that day that are "under follow-up"
# get list of contacts and their FU start dates and FU end dates, for each one of these dates this counts at 1.
# not sure how to do this.

### plot contact tracing FU status, by region using generated FUs.

fu_by_day_region <-
  followups %>%
  group_by(date, admin_1_name) %>%
  tally()
