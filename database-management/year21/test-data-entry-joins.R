###project: MAP
###author(s): Mack White
###goal(s): testing joins of site and catch data from new data entry process
###date(s): december 2024
###note(s):

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, writexl, scales, ggplot2)

site <- read_csv("data/other/site-test-2.csv") |> 
      mutate(site = as.character(site),
             bout = as.character(bout))
glimpse(site)
catch <- read_csv("data/other/catch-test-2.csv") |> 
      mutate(site = as.character(site),
             bout = as.character(bout))
glimpse(catch)

full <- left_join(catch, site, by = c('date', 'drainage', "site", "bout"))
