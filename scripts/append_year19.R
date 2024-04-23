###project: MAP
###author(s): Mack White
###goal(s): recreate period of record MAP dataset for USACE-SRS
###date(s): April 2024
###note(s):

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, writexl, scales, ggplot2)

### read data

old <- read_csv("data/map_years1thru17.csv") |> 
      janitor::clean_names() |> 
      select(sample, hydroyear, year, date, month, season, drainage, site, bout, distance,
             starttime, endtime, pedaltime, bankside, voltagesetting, per_cvoltagerange, 
             ampoutput, banktype, bottomtype, habitatstructure, wind, visibility, turbidity,
             flowdirection, flowheight, flowstrength, tribwidth, tripdepth, temp_c, domgl, doperc,
             speccond, salinity, sampleid, speciescode, catchnumber, catchcode, tl, sl, weight,
             pittag, recapture, acoustictag, fate, lts, stomachcontent, fishcomments, boutcomments,
             sitecomments, acoustic_tag_comments)

year18 <- read_csv("data/map_year18.csv") |> 
      janitor::clean_names() |> 
      rename(lts = lts_finclip) |> 
      select(sample, hydroyear, year, date, month, season, drainage, site, bout, distance,
             starttime, endtime, pedaltime, bankside, voltagesetting, per_cvoltagerange, 
             ampoutput, banktype, bottomtype, habitatstructure, wind, visibility, turbidity,
             flowdirection, flowheight, flowstrength, tribwidth, tripdepth, temp_c, domgl, doperc,
             speccond, salinity, sampleid, speciescode, catchnumber, catchcode, tl, sl, weight,
             pittag, recapture, acoustictag, fate, lts, stomachcontent, fishcomments, boutcomments,
             sitecomments, acoustic_tag_comments)

year19 <- read_csv("data/map_year19.csv") |> 
      janitor::clean_names() |> 
      rename(lts = lts_finclip) |> 
      select(sample, hydroyear, year, date, month, season, drainage, site, bout, distance,
             starttime, endtime, pedaltime, bankside, voltagesetting, per_cvoltagerange, 
             ampoutput, banktype, bottomtype, habitatstructure, wind, visibility, turbidity,
             flowdirection, flowheight, flowstrength, tribwidth, tripdepth, temp_c, domgl, doperc,
             speccond, salinity, sampleid, speciescode, catchnumber, catchcode, tl, sl, weight,
             pittag, recapture, acoustictag, fate, lts, stomachcontent, fishcomments, boutcomments,
             sitecomments, acoustic_tag_comments)

all <- rbind(old, year18, year19)
# writexl::write_xlsx(all, "data/map_years1thru19.xlsx")
# write_csv(all, "data/map_years1thru19.csv")

