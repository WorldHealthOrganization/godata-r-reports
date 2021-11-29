# plot transmission chains using epicontacts

# only need to run once
# remotes::install_github("reconhub/epicontacts@timeline")

# required packages

pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # Data management + ggplot2 graphics
  remotes,    # Package installation from github
  epicontacts
)



# load data

cases <- rio::import(here::here("data", "clean", "cases_clean.rds")) %>%
  as_tibble()

relationships <- rio::import(here::here("data", "clean", "relationships_clean.rds")) %>%
  as_tibble()

relationships_sub <- relationships %>%
 drop_na(source_person_visual_id) %>%
drop_na(target_person_visual_id) 

# cases_sub <- cases 
  # drop_na(date_of_onset) %>%
  # drop_na(date_of_outcome)

## set visual parameters

## define shapes
shapes <- c(
  FEMALE = "female",
  MALE = "male",
  "NA" = "question-circle"
)

## define colours
colours <- c(
  DECEASED = "firebrick",
  RECOVERED = "green",
  ALIVE = "orange"
)

## generate epicontacts object

epic <- make_epicontacts(
  linelist = cases,
  contacts = relationships_sub,
  id = "visual_id",
  from = "source_person_visual_id",
  to = "target_person_visual_id",
  directed = TRUE
)


## generate timeline object

# timeline <- cases_sub %>%
#   transmute(
#     id = visual_id,
#     start = date_of_onset,
#     end = date_of_outcome,
#     outcome = outcome
#   )

## plot epicontacts object

plot_bubble <-

  plot(
    epic,
    width = 700,
    height = 700,
    node_size = 50,
    node_shape = "gender",
    shapes = shapes, # specify shape parameters
    #node_color = "outcome",
    thin = FALSE,
    label = FALSE,
    #col_pal = colours # specify colour parameters above
  )


# plot temporal

## make plot
# plot_temporal <- 
#   
# plot(
#   epic,
#   ## max x coordinate to date of onset
#   x_axis = "date_of_onset",
#   ## use rectangular network shape
#   network_shape = "rectangle",
#   ## mape case node shapes to gender column
#   node_shape = "gender",
#   ## we don't want to map node colour to any columns - this is important as the
#   ## default value is to map to node id, which will mess up the colour scheme
#   node_color = NULL,
#   ## set case node size to 30 (as this is not a character, node_size is not
#   ## mapped to a column but instead interpreted as the actual node size)
#   node_size = 30,
#   ## set transmission link width to 4 (as this is not a character, edge_width is
#   ## not mapped to a column but instead interpreted as the actual edge width)
#   edge_width = 4,
#   ## provide the timeline object
#   timeline = timeline,
#   ## map the shape of the end node to the outcome column in the timeline object
#   tl_end_node_shape = "outcome",
#   ## set the size of the end node to 15 (as this is not a character, this
#   ## argument is not mapped to a column but instead interpreted as the actual
#   ## node size)
#   tl_end_node_size = 15,
#   ## map the colour of the timeline edge to the hospital column
#   # tl_edge_color = "hospital",
#   ## set the width of the timeline edge to 2 (as this is not a character, this
#   ## argument is not mapped to a column but instead interpreted as the actual
#   ## edge width)
#   tl_edge_width = 2,
#   ## map edge labels to the hospital variable
#   # tl_edge_label = "hospital",
#   ## specify the shape for everyone node attribute (defined above)
#   shapes = shapes,
#   ## specify the colour palette (defined above)
#   col_pal = colours,
#   ## set the size of the arrow to 0.5
#   arrow_size = 0.5,
#   ## use two columns in the legend
#   legend_ncol = 2,
#   ## set font size
#   font_size = 15,
#   ## define formatting for dates
#   date_labels = c("%d %b %Y"),
#   ## don't plot the ID labels below nodes
#   label = FALSE,
#   ## specify height
#   height = 1000,
#   ## specify width
#   width = 1200,
#   ## ensure each case node has a unique y-coordinate - this is very important
#   ## when using timelines, otherwise you will have overlapping timelines from
#   ## different cases
#   position_dodge = TRUE
# )
