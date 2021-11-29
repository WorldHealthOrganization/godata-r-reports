# Plot events
######################################################################################################


library(pacman)
library(incidence2)

# load data


events <- rio::import(here::here("data", "clean", "events_clean.rds")) %>%
  as_tibble()


# plots events

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
