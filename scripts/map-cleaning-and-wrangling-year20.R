###project: MAP
###author(s): Mack White
###goal(s): cleaning of MAP Year 20 (i.e., 2024) through April
###date(s): April 2024
###note(s):

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, writexl, scales, ggplot2)

# step zero: read in appended dataset -------------------------------------

step0_dat <- read_csv("data/map_years1thru20_April.csv") |> 
      ### generate full_site column for zero-filling and "true" site identification
      mutate(full_site = paste(drainage, site, sep = ""))
glimpse(step0_dat)

# step one: filter out sites that are not real ----------------------------

step1_dat <- step0_dat |> 
      filter(!full_site %in% c("RB17", "RB19", "TB5", "RBNA"))

# step2: prepare data for outlier cleaning and NA-filling -----------------

### step 2a: separate data in to species we take TL and SL measurements for

dt_tl <- step1_dat |> 
      ### select species we take total length measurements for, not SL
      filter(common_name %in% c("Florida gar", "American eel", "Bowfin",
                                "Asian Swamp Eel", "Peacock eel")) |> 
      ### remove extraneous information in period of record file
      select(hydroyear, year, date, month, season, drainage, full_site, bout,
             distance, temp_c, salinity, domgl, common_name, latin_name, genus, 
             speciescode, status, catchnumber, catchcode,
             tl, sl, weight, stomachcontent)

### check to see where there are NAs - looks pretty good - nothing crazy (e.g., common name) with NA
na_count_per_column <- sapply(dt_tl, function(x) sum(is.na(x)))
print(na_count_per_column)

dt_sl <- step1_dat |> 
      ### select species we take total length measurements for, not SL
      filter(!common_name %in% c("Florida gar", "American eel", "Bowfin",
                                "Asian Swamp Eel", "Peacock eel")) |> 
      ### remove extraneous information in period of record file
      select(hydroyear, year, date, month, season, drainage, full_site, bout,
             distance, temp_c, salinity, domgl, common_name, latin_name, genus, 
             speciescode, status, catchnumber, catchcode,
             tl, sl, weight, stomachcontent)

### check to see where there are NAs - looks pretty good - besides 148 missing "common_name"
na_count_per_column <- sapply(dt_sl, function(x) sum(is.na(x)))
print(na_count_per_column)
### check out what is going on with missing common_name
test_common_name_na <- dt_sl |> filter(is.na(common_name))

### all species codes are either "NA" or not #'s associated with species table - need to remove
dt_sl <- dt_sl |> filter(!is.na(common_name))

### step 2b-TL: calculate length-weight relationships for all species within TL dataframe

stats_tl <- dt_tl |> 
      group_by(common_name) |> 
      summarize(mean_tl = mean(tl, na.rm = TRUE),
                sd_tl = sd(tl, na.rm = TRUE),
                mean_weight = mean(weight, na.rm = TRUE),
                sd_weight = sd(weight, na.rm = TRUE))

### add new information back to og dataframe for identifying outliers and setting as "NA" or imputation
dt_tl_outlier_NA <- dt_tl |> left_join(stats_tl, by = "common_name") |> 
      ### changing extreme outliers (i.e., human error) to NAs
      mutate(tl = ifelse(tl < (mean_tl - 5 * sd_tl) | tl > (mean_tl + 5 * sd_tl), NA, tl),
             weight = ifelse(weight < (mean_weight - 5 * sd_weight) | weight > (mean_weight + 5 * sd_weight), NA, weight)) |> 
      ### remove mean and sd columns
      select(-starts_with("mean"), -starts_with("sd"))

### impute missing TL and weight data in hierarchical manner (i.e., fine - coarse grouping)
dt_tl_NA_fill <- dt_tl_outlier_NA |> 
      ### NA fill - impute step one
      group_by(common_name, year, month, drainage) |> 
      mutate(tl = ifelse(is.na(tl), mean(tl, na.rm = TRUE), tl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### NA fill - impute step two
      group_by(common_name, year, month) |> 
      mutate(tl = ifelse(is.na(tl), mean(tl, na.rm = TRUE), tl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### NA fill - impute step three
      group_by(common_name, year) |> 
      mutate(tl = ifelse(is.na(tl), mean(tl, na.rm = TRUE), tl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### NA fill - impute step four
      group_by(common_name) |> 
      mutate(tl = ifelse(is.na(tl), mean(tl, na.rm = TRUE), tl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      # small # of observations with missing distance, so made 100 - standard distance
      mutate(distance = ifelse(is.na(distance), 100, distance)) |> 
      # useless column... so many NAs, so remove "season"
      select(-season)

na_count_per_column <- sapply(dt_tl_NA_fill, function(x) sum(is.na(x)))
print(na_count_per_column) #looks good
### assign as "step_2_tl" so we know we are done... 
step_2_tl <- dt_tl_NA_fill

### step 2b-SL: calculate length-weight relationships for all species within TL dataframe

stats_sl <- dt_sl |> 
      group_by(common_name) |> 
      summarize(mean_sl = mean(sl, na.rm = TRUE),
                sd_sl = sd(sl, na.rm = TRUE),
                mean_weight = mean(weight, na.rm = TRUE),
                sd_weight = sd(weight, na.rm = TRUE))

### add new information back to og dataframe for identifying outliers and setting as "NA" or imputation
dt_sl_outlier_NA <- dt_sl |> left_join(stats_sl, by = "common_name") |> 
      ### changing extreme outliers (i.e., human error) to NAs
      mutate(sl = ifelse(sl < (mean_sl - 5 * sd_sl) | sl > (mean_sl + 5 * sd_sl), NA, sl),
             weight = ifelse(weight < (mean_weight - 5 * sd_weight) | weight > (mean_weight + 5 * sd_weight), NA, weight)) |> 
      ### remove mean and sd columns
      select(-starts_with("mean"), -starts_with("sd"))

### filter out snook and bass, so we can do more refined LW regression at later point 
snook_bass_preserved <- dt_sl_outlier_NA |> 
      filter(common_name %in% c("Snook", "Largemouth bass")) |> 
      select(-season)

### impute missing TL and weight data in hierarchical manner (i.e., fine - coarse grouping)
dt_sl_NA_fill <- dt_sl_outlier_NA |> 
      ### remove snook and bass for LW regression
      filter(!common_name %in% c("Snook", "Largemouth bass")) |> 
      ### NA fill - impute step one
      group_by(common_name, year, month, drainage) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### NA fill - impute step two
      group_by(common_name, year, month) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### NA fill - impute step three
      group_by(common_name, year) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### NA fill - impute step four
      group_by(common_name) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### small # of observations with missing distance, so made 100 - standard distance
      mutate(distance = ifelse(is.na(distance), 100, distance)) |> 
      ### useless column... so many NAs, so remove "season"
      select(-season) |> 
      ### remove small # of NA for SL bc associated with fishes with very few obs or "No fishes collected"
      filter(!(is.na(sl)& common_name != "No fishes collected")) |> 
      filter(!(is.na(weight) & common_name != "No fishes collected" & common_name != "Sunfishes"))

na_count_per_column <- sapply(dt_sl_NA_fill, function(x) sum(is.na(x)))
print(na_count_per_column)
#missing sl and weights are coming from sunfishes (i.e., just saw) and no fishes collected

### assign as "step_2_sl" so we know we are done... 
step_2_sl_minusSnookAndBass <- dt_sl_NA_fill
step_2_sl <- rbind(step_2_sl_minusSnookAndBass, snook_bass_preserved) |> 
      ### small # of observations with missing distance, so made 100 - standard distance
      mutate(distance = ifelse(is.na(distance), 100, distance))

na_count_per_column <- sapply(step_2_sl, function(x) sum(is.na(x)))
print(na_count_per_column)

test_sl_na <- step_2_sl |> 
      filter(is.na(sl))
unique(test_sl_na$common_name)
# (1) NFC, (2) Snook, (3) Largemouth Bass

test_weight_na <- step_2_sl |> 
      filter(is.na(weight))
unique(test_weight_na$common_name)
# (1) Sunfishes, (2) NFC, (3) Snook, (4) Largemouth Bass

# NA fill for "Sunfishes" -------------------------------------------------
step_2_sl_NOsunfishes <- step_2_sl |> filter(genus != "Lepomis")
step_2_sl_sunfishes <- step_2_sl |> filter(genus == "Lepomis")

step_2_sl_sunfishesNAfill  <- step_2_sl_sunfishes |> 
      ### NA fill - impute step one
      group_by(year, month, drainage) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### NA fill - impute step two
      group_by(year, month) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### NA fill - impute step three
      group_by(year) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup() |> 
      ### NA fill - impute step four
      group_by(genus) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)) |> 
      ungroup()

step_2_sl_join1 <- rbind(step_2_sl_NOsunfishes, step_2_sl_sunfishesNAfill)
test_weight_na <- step_2_sl_join1 |> 
      filter(is.na(weight))
unique(test_weight_na$common_name) #sunfishes taken care of

na_count_per_column <- sapply(step_2_sl_join1, function(x) sum(is.na(x)))
print(na_count_per_column)

# use length-weight regressions to NA fill for Snook and Bass -------------
step_2_sl_NOsnook_bass <- step_2_sl_join1 |> filter(!common_name %in% c("Snook", "Largemouth bass"))
step_2_sl_snook_bass <- step_2_sl_join1 |> filter(common_name %in% c("Snook", "Largemouth bass"))

step_2_sl_snook_NAfill <- step_2_sl_snook_bass |> 
      filter(common_name=='Snook') |>  
      mutate(weight_g = ifelse(!is.na(weight), 
                                   weight*1000, 
                                   0.009504406*sl^3.078241))

step_2_sl_bass_NAfill <- step_2_sl_snook_bass |> 
      filter(common_name=='Largemouth bass') |>  
      mutate(weight_g = ifelse(!is.na(weight), 
                                   weight*1000,
                                   0.01218249*sl^3.200356))

step_2_sl_snook_bass_NAfill <- rbind(step_2_sl_snook_NAfill, step_2_sl_bass_NAfill)

step_2_sl_NOsnook_bass_grams <- step_2_sl_NOsnook_bass |> 
      mutate(weight_g = weight*100)

step_2_sl_join2 <- rbind(step_2_sl_snook_bass_NAfill, step_2_sl_NOsnook_bass_grams)

test_weight_na <- step_2_sl_join2 |> 
      filter(is.na(weight_g))
unique(test_weight_na$common_name) #still have some snook and bass without weights

na_count_per_column <- sapply(step_2_sl_join2, function(x) sum(is.na(x)))
print(na_count_per_column) #took care of a little over 1000 missing weights from snook and bass

# NA fill for "Snook" and "Largemouth bass" -------------------------------------------------
step_2_sl_NFC_preserved <- step_2_sl_join2 |> 
      filter(common_name == "No fishes collected")
      #hold off on this join until after zero fill - 70 instances

step_2_sl_NO_NFC <- step_2_sl_join2 |> 
      filter(common_name != "No fishes collected")

step_2_sl_NO_NFC_FINAL_NA_FILL <- step_2_sl_NO_NFC |> 
      ### NA fill - impute step one
      group_by(common_name, year, month, drainage) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight_g = ifelse(is.na(weight_g), mean(weight_g, na.rm = TRUE), weight_g)) |> 
      ungroup() |> 
      ### NA fill - impute step two
      group_by(common_name, year, month) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight_g = ifelse(is.na(weight_g), mean(weight_g, na.rm = TRUE), weight_g)) |>
      ungroup() |> 
      ### NA fill - impute step three
      group_by(common_name, year) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight_g = ifelse(is.na(weight_g), mean(weight_g, na.rm = TRUE), weight_g)) |>
      ungroup() |> 
      ### NA fill - impute step four
      group_by(common_name) |> 
      mutate(sl = ifelse(is.na(sl), mean(sl, na.rm = TRUE), sl),
             weight_g = ifelse(is.na(weight_g), mean(weight_g, na.rm = TRUE), weight_g)) |>
      ungroup()

na_count_per_column <- sapply(step_2_sl_NO_NFC_FINAL_NA_FILL, function(x) sum(is.na(x)))
print(na_count_per_column) #no more NAs

step_2_sl_join3 <- rbind(step_2_sl_NO_NFC_FINAL_NA_FILL, step_2_sl_NFC_preserved)


# prepare data and join sl and tl data ------------------------------------

### total length dataset
step_2_tl_final <- step_2_tl |> 
      mutate(weight_g = weight*1000) |> 
      rename(weight_kg = weight)

na_count_per_column <- sapply(step_2_tl_final, function(x) sum(is.na(x)))
print(na_count_per_column) #NA fixed for TL and weight_g
glimpse(step_2_tl_final)

### standard length dataset
step_2_sl_final <- step_2_sl_join3 |> 
      rename(weight_kg = weight)

na_count_per_column <- sapply(step_2_sl_final, function(x) sum(is.na(x)))
print(na_count_per_column) #NA fixed for SL and weight_g (minus "No fishes collected"; 70 instances)

glimpse(step_2_sl_final)

### join the two datasets
map_all_thru_042024 <- rbind(step_2_sl_final, step_2_tl_final)
# writexl::write_xlsx(map_all_thru_042024, "data/map_all_thru_042024.xlsx")
# write_csv(map_all_thru_042024, "data/map_all_thru_042024.csv")
