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

### read data

old <- read_csv("data/map_years1thru19.csv")

year20 <- read_csv("data/map_year20thruApril2024.csv") |> 
      janitor::clean_names() |> 
      rename(lts = lts_finclip) |> 
      select(sample, hydroyear, year, date, month, season, drainage, site, bout, distance,
             starttime, endtime, pedaltime, bankside, voltagesetting, per_cvoltagerange, 
             ampoutput, banktype, bottomtype, habitatstructure, wind, visibility, turbidity,
             flowdirection, flowheight, flowstrength, tribwidth, tripdepth, temp_c, domgl, doperc,
             speccond, salinity, sampleid, speciescode, catchnumber, catchcode, tl, sl, weight,
             pittag, recapture, acoustictag, fate, lts, stomachcontent, fishcomments, boutcomments,
             sitecomments, acoustic_tag_comments)

all <- rbind(old, year20)
# writexl::write_xlsx(all, "data/map_years1thru20_April.xlsx")
# write_csv(all, "data/map_years1thru20_April.csv")

species <- read_csv("data/MAPmaster_yrs1thru19_speciesnames_CLEAN.csv") |> 
      select(species, latin_name, common_name, status) |> 
      rename(speciescode = species) |> 
      distinct()
# writexl::write_xlsx(species, "data/map_species_code_table.xlsx")

all_species <- read_csv("data/map_species_code_table.csv") |> 
      mutate(speciescode = as.character(speciescode))

all_with_species_info <- left_join(all, all_species, by = "speciescode")
# writexl::write_xlsx(all_with_species_info, "data/map_years1thru20_April.xlsx")
# write_csv(all_with_species_info, "data/map_years1thru20_April.csv")
