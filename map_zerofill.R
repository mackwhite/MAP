###project: MAP
###author(s): Mack White
###goal(s): zero-filling of MAP Year 20 through April
###date(s): April 2024
###note(s):

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, writexl, scales, ggplot2)

raw <- read_csv("data/map_years1thru20_April.csv") |> 
      mutate(full_site = paste(drainage, site, sep = "-"))
glimpse(raw)

unique_years <- raw |> 
      select(full_site, year) |> 
      distinct()

raw_clean <- raw |> 
      filter(!full_site %in% c("RB-17", "RB-19", "TB-5", "RB-NA"))#770 observation

years_all <- raw_clean |> 
      filter(full_site %in% c("RB-7", "RB-8", "RB-9", "RB-10", "RB-11",
                              "RB-12")) |> 
      select(hydroyear, year, date, month, season, drainage, full_site, bout,
             distance, temp_c, salinity, domgl, common_name, latin_name, genus, 
             speciescode, status, catchnumber, catchcode,
             tl, sl, weight, stomachcontent)

years_all_zero <- years_all |> 
      complete(nesting(common_name, latin_name, genus, speciescode, status),
               nesting(hydroyear, year, date, month, season, drainage, full_site, bout, distance, temp_c, salinity, domgl),
               fill = list(catchnumber=0, catchcode= "ZERO", weight=0, sl = 0, tl = 0))

years_all_minusone <- raw_clean |> 
      filter(full_site %in% c("RB-13", "RB-14", "RB-15", "RB-16")) |> 
      select(hydroyear, year, date, month, season, drainage, full_site, bout,
             distance, temp_c, salinity, domgl, common_name, latin_name, genus, 
             speciescode, status, catchnumber, catchcode,
             tl, sl, weight, stomachcontent)

years_all_minusone_zero <- years_all_minusone |> 
      complete(nesting(common_name, latin_name, genus, speciescode, status),
               nesting(hydroyear, year, date, month, season, drainage, full_site, bout, distance, temp_c, salinity, domgl),
               fill = list(catchnumber=0, catchcode= "ZERO", weight=0, sl = 0, tl = 0))

years_all_minusfour <- raw_clean |> 
      filter(full_site %in% c("TB-1", "TB-2", "TB-3", "TB-4")) |> 
      select(hydroyear, year, date, month, season, drainage, full_site, bout,
             distance, temp_c, salinity, domgl, common_name, latin_name, genus, 
             speciescode, status, catchnumber, catchcode,
             tl, sl, weight, stomachcontent)

years_all_minusfour_zero <- years_all_minusfour |> 
      complete(nesting(common_name, latin_name, genus, speciescode, status),
               nesting(hydroyear, year, date, month, season, drainage, full_site, bout, distance, temp_c, salinity, domgl),
               fill = list(catchnumber=0, catchcode= "ZERO", weight=0, sl = 0, tl = 0))

years_all_minuseight <- raw_clean |> 
      filter(full_site %in% c("RB-18")) |> 
      select(hydroyear, year, date, month, season, drainage, full_site, bout,
             distance, temp_c, salinity, domgl, common_name, latin_name, genus, 
             speciescode, status, catchnumber, catchcode,
             tl, sl, weight, stomachcontent)

years_all_minuseight_zero <- years_all_minuseight |> 
      complete(nesting(common_name, latin_name, genus, speciescode, status),
               nesting(hydroyear, year, date, month, season, drainage, full_site, bout, distance, temp_c, salinity, domgl),
               fill = list(catchnumber=0, catchcode= "ZERO", weight=0, sl = 0, tl = 0))

map_all_thru_april2024_zerofilled <- rbind(years_all_zero, years_all_minusone_zero,
                                     years_all_minusfour_zero, years_all_minuseight_zero)
# write_csv(map_all_thru_april2024_zerofilled, "data/map_years1thru20_April.csv")

dt <- map_all_thru_april2024_zerofilled
glimpse(dt)

dt_tl <- dt |> 
      filter(common_name %in% c("Florida gar", "American eel", "Bowfin",
                                "Asian Swamp Eel", "Peacock eel"))

tl_valid_data <- dt_tl |> 
      filter(catchnumber > 0, !is.na(tl), !is.na(weight))
tl_models <- tl_valid_data |> 
      group_by(common_name) |> 
      do(model = lm(weight ~ tl, data = .))
tl_model_coefs <- tl_models |> 
      summarise(intercept = coef(model)[1], slope = coef(model)[2])

dt_sl <- dt |> 
      filter(!common_name %in% c("Florida gar", "American eel", "Bowfin",
                                "Asian Swamp Eel", "Peacock Eel"))
