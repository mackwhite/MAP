###project: MAP
###author(s): Mack White
###goal(s): zero-filling of MAP Year 20 (i.e., 2024) through April
###date(s): April 2024
###note(s):

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, writexl, scales, ggplot2)

# read in QAQC'ed data ----------------------------------------------------

dat <- read_csv("data/map_years1thru20_clean.csv") |> 
      rename(hydrologic_year = hydroyear,
             project_year = year,
             site = full_site,
             do_mg_l = domgl,
             species_code = speciescode,
             catch_number = catchnumber,
             catch_code = catchcode,
             total_length = tl,
             standard_length = sl,
             stomach_contents = stomachcontent) |> 
      mutate(date_test = mdy(date),
             calendar_year = year(date),
             month = month(date),
             day = day(date)) |> 
      select(date, hydrologic_year, project_year, calendar_year, month, day,
             drainage, site, bout, 
             distance, temp_c, salinity, do_mg_l, 
             common_name, latin_name, genus, species_code, status,
             catch_number, catch_code,
             standard_length, total_length, weight_g,
             stomach_contents)
      
glimpse(dat)

na_count_per_column <- sapply(dat, function(x) sum(is.na(x)))
print(na_count_per_column)

test <- dat |> filter(is.na(calendar_year))

# preserve "No fishes collected" before NA fill ---------------------------
### ensures we are not creating unreal zeros for sites where fish were indeed collected

dat_NFC <- dat |> filter(common_name == "No fishes collected")
dat_1 <- dat |> filter(common_name != "No fishes collected")

# separate out sites based on # of years sampled --------------------------
### ensures we are no creating unreal zeros

### sites missing 0 years of data

years_all <- dat_1 |> 
      filter(site %in% c("RB7", "RB8", "RB9", "RB10", "RB11", "RB12"))

years_all_zero <- years_all |> 
      complete(nesting(common_name, latin_name, genus, species_code, status),
               nesting(date, hydrologic_year, project_year, calendar_year, month, day, drainage, site, bout, distance, temp_c, salinity, do_mg_l),
               fill = list(catch_number=0, catch_code= "ZERO", standard_length = 0, total_length = 0, weight_g = 0))

### sites missing 1 year of data

years_all_minusone <- dat_1 |> 
      filter(site %in% c("RB13", "RB14", "RB15", "RB16"))

years_all_minusone_zero <- years_all_minusone |> 
      complete(nesting(common_name, latin_name, genus, species_code, status),
               nesting(date, hydrologic_year, project_year, calendar_year, month, day, drainage, site, bout, distance, temp_c, salinity, do_mg_l),
               fill = list(catch_number=0, catch_code= "ZERO", standard_length = 0, total_length = 0, weight_g = 0))

### sites sites missing 4 years of data

years_all_minusfour <- dat_1 |> 
      filter(site %in% c("TB1", "TB2", "TB3", "TB4")) 

years_all_minusfour_zero <- years_all_minusfour |> 
      complete(nesting(common_name, latin_name, genus, species_code, status),
               nesting(date, hydrologic_year, project_year, calendar_year, month, day, drainage, site, bout, distance, temp_c, salinity, do_mg_l),
               fill = list(catch_number=0, catch_code= "ZERO", standard_length = 0, total_length = 0, weight_g = 0))

### sites missing 8 years of data

years_all_minuseight <- dat_1 |> 
      filter(site %in% c("RB18"))

years_all_minuseight_zero <- years_all_minuseight |> 
complete(nesting(common_name, latin_name, genus, species_code, status),
         nesting(date, hydrologic_year, project_year, calendar_year, month, day, drainage, site, bout, distance, temp_c, salinity, do_mg_l),
         fill = list(catch_number=0, catch_code= "ZERO", standard_length = 0, total_length = 0, weight_g = 0))

zero <- rbind(years_all_zero, years_all_minusone_zero,
              years_all_minusfour_zero, years_all_minuseight_zero)

zero_arranged <- zero |> 
      select(date, hydrologic_year, project_year, calendar_year, month, day,
             drainage, site, bout, 
             distance, temp_c, salinity, do_mg_l, 
             common_name, latin_name, genus, species_code, status,
             catch_number, catch_code,
             standard_length, total_length, weight_g,
             stomach_contents)

zero_arranged_all <- rbind(zero_arranged, dat_NFC)

### clean environment ---
keep <- c("zero_arranged_all")
rm(list = setdiff(ls(), keep))

write_csv(zero_arranged_all, "data/map_years1thru20_clean_zerofilled.csv")
