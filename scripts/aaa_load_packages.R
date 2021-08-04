##########################################
# List of useful epidemiology R packages #
##########################################

# This script uses the p_load() function from pacman R package, 
# which installs if package is absent, and loads for use if already installed


# Ensures the package "pacman" is installed
if (!require("pacman")) install.packages("pacman")


# Packages available from CRAN
##############################
pacman::p_load(
  
  # project and file management
  #############################
  #here,     # file paths relative to R project root folder
  #rio,      # import/export of many types of data
  #openxlsx, # import/export of multi-sheet Excel workbooks 
  
  # package install and management
  ################################
  pacman,   # package install/load
  renv,     # managing versions of packages when working in collaborative groups
  remotes,  # install from github
  
  # General data management
  #########################
  tidyverse,    # includes many packages for tidy data wrangling and presentation
  #dplyr,      # data management
  #tidyr,      # data management
  #ggplot2,    # data visualization
  #stringr,    # work with strings and characters
  #forcats,    # work with factors 
  #lubridate,  # work with dates
  #purrr       # iteration and working with lists
  linelist,     # cleaning linelists
  naniar,       # assessing missing data
  
  # statistics  
  ############
  janitor,      # tables and data cleaning
  #gtsummary,    # making descriptive and statistical tables
  #rstatix,      # quickly run statistical tests and summaries
  #broom,        # tidy up results from regressions
  #lmtest,       # likelihood-ratio tests
  #easystats,
  # parameters, # alternative to tidy up results from regressions
  # see,        # alternative to visualise forest plots 
  
  # I COMMENTED THIS OUT. CAUSING PROBLEMS ON MY MACHINE AND DON'T SEEM NECESSARY
  # # epidemic modeling 
  # ###################
  # epicontacts,  # Analysing transmission networks
  # EpiNow2,      # Rt estimation
  # EpiEstim,     # Rt estimation
  # projections,  # Incidence projections
  # incidence2,   # Make epicurves and handle incidence data
  # i2extras,     # Extra functions for the incidence2 package
  # epitrix,      # Useful epi functions
  # distcrete,    # Discrete delay distributions
  
  
  # plots - general
  #################
  #ggplot2,         # included in tidyverse
  #cowplot,          # combining plots  
  # patchwork,      # combining plots (alternative)     
  # RColorBrewer,     # color scales
  # ggnewscale,       # to add additional layers of color schemes
  
  
  # plots - specific types
  ########################
  # DiagrammeR,       # diagrams using DOT language
  # incidence2,       # epidemic curves
  # gghighlight,      # highlight a subset
  # ggrepel,          # smart labels
  # plotly,           # interactive graphics
  # gganimate,        # animated graphics 
  
  
  # gis
  ######
  # sf,               # to manage spatial data using a Simple Feature format
  # tmap,             # to produce simple maps, works for both interactive and static maps
  # OpenStreetMap,    # to add OSM basemap in ggplot map
  # spdep,            # spatial statistics 
  
  # routine reports
  #################
  # rmarkdown,        # produce PDFs, Word Documents, Powerpoints, and HTML files
  # reportfactory,    # auto-organization of R Markdown outputs
  # officer,          # powerpoints
  
  # dashboards
  ############
  # flexdashboard,    # convert an R Markdown script into a dashboard
  # shiny,            # interactive web apps
  
  # tables for presentation
  #########################
  # knitr,            # R Markdown report generation and html tables
  # flextable,        # HTML tables
  #DT,              # HTML tables (alternative)
  #gt,              # HTML tables (alternative)
  #huxtable,        # HTML tables (alternative) 
  
  # phylogenetics
  ###############
  # ggtree,           # visualization and annotation of trees
  # ape,              # analysis of phylogenetics and evolution
  # treeio,            # to visualize phylogenetic files
  
  # api
  ###############
  httr,            # handles HTTP requests in R
  jsonlite         # to parse .json 
  
)