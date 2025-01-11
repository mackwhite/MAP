###project: MAP
###author(s): Mack White
###goal(s): append year 20 of MAP dataset through April for USACE-SRS
###date(s): April 2024
###note(s):

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, writexl, scales, ggplot2)

### read in dataset from teams - just copy and paste most previous year
### should be 52 variables ---
year20 <- readxl::read_xlsx("data/electrofishing/year20/map_years1thru20.xlsx") |> 
      janitor::clean_names() |> 
      # rename(lts = lts_finclip) |> 
      select(sample, hydroyear, year, date, month, season, drainage, site, bout, distance,
             starttime, endtime, pedaltime, bankside, voltagesetting, per_cvoltagerange, 
             ampoutput_low, ampoutput_high, ampoutput_mean, banktype, bottomtype, habitatstructure, wind, visibility, turbidity,
             flowdirection, flowheight, flowstrength, tribwidth, tripdepth, temp_c, domgl, doperc,
             speccond, salinity, sampleid, speciescode, catchnumber, catchcode, tl, sl, weight,
             pittag, recapture, acoustictag, fate, lts, stomachcontent, fishcomments, boutcomments,
             sitecomments, acoustic_tag_comments) |> 
      mutate(speciescode = as.factor(speciescode))

all_species <- read_csv("data/for-joins/map_species_code_table.csv") |> 
      mutate(speciescode = as.character(speciescode))

all_with_species_info <- left_join(year20, all_species, by = "speciescode")
# writexl::write_xlsx(all_with_species_info, "data/electrofishing/year20/map_years1thru20.xlsx")
# write_csv(all_with_species_info, "data/electrofishing/year20/map_years1thru20.csv")
