# Plot labs
######################################################################################################


library(pacman)
library(incidence2)

# load data


lab_results <- rio::import(here::here("data", "clean", "lab_results_clean.rds")) %>% as_tibble() %>%
  as_tibble()

# format

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



# plot sample collection, by date of reporting


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
