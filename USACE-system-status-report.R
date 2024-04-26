###project: MAP
###author(s): Mack White
###goal(s): SSR Plots
###date(s): April 2024
###note(s): I have been assigned the following tasks:
# (1) Bass:
### (a) CPUE, (b) Biomass, (c) Condition
# (2) Snook:
### (a) CPUE, (b) Biomass, (c) Condition
# (3) Sunfish:
### (a) CPUE, (b) Biomass, (c) Richness - calculated monthly and averaged
# (4) Nonnatives:
### (a) CPUE, (b) Biomass, (c) Richness - calculated monthly and averaged

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, writexl, scales, ggplot2)

# read in QAQC'ed data ----------------------------------------------------
### data is zero filled as well
dat <- read_csv("data/map_thru_042024_zerofilled.csv") |> 
      ### remove extraneous hydrologic years
      filter(!hydrologic_year %in% c("2021-2019", "2021-2020", "2021-2021"))

### read out hydrologic year and code according to SSR requirements
# hydrologic_year <- dat |> 
#       select(hydrologic_year) |> 
#       distinct()
# write_csv(hydrologic_year, "data/hydrologic_year.csv")

### left join with updated water year
water_year_ssr <- read_csv("data/hydrologic_year.csv")
dt <- left_join(dat, water_year_ssr, by = "hydrologic_year")

dt <- dt |> 
      select(-hydrologic_year) |> 
      rename(water_year = water_year_ssr)
glimpse(dt)

###########################################################################
# Task 1: Bass CPUE & Biomass ---------------------------------------------
###########################################################################

bass_stats <- dt |> 
      filter(common_name == "Largemouth bass") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      group_by(common_name, water_year, month, site, bout) |> 
      summarize(count = sum(catch_number, na.rm = TRUE),
                count_m = count/distance,
                count_100m = count_m*100,
                biomass = sum(weight_g*catch_number),
                biomass_m = biomass/distance,
                biomass_100m = biomass_m*100) |> 
      ungroup() |> 
      mutate(period = if_else(water_year >= 2018, "reporting", "baseline")) |> 
      group_by(common_name, period, water_year) |> 
      summarize(mean_wy_biomass = mean(biomass_100m),
                mean_wy_count = mean(count_100m)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(mean_rp_biomass = mean(mean_wy_biomass),
             mean_rp_period_count = mean(mean_wy_count))

bass_stats_1 <- bass_stats |> 
      mutate(baseline_biomass = 7262.707,
             baseline_count = 12.936896)

bass_stats_2 <- bass_stats_1 |> 
      group_by(water_year) |> 
      mutate(count_prop = mean_wy_count/baseline_count,
             biomass_prop = mean_wy_biomass/baseline_biomass, 
             count_light = case_when(
                   count_prop > 0.501 ~ "green",
                   count_prop >= 0.301 & count_prop <= 0.50 ~ "yellow",
                   count_prop <= 0.30 ~ "red"),
             biomass_light = case_when(
                   biomass_prop > 0.501 ~ "green",
                   biomass_prop >= 0.301 & biomass_prop <= 0.50 ~ "yellow",
                   biomass_prop <= 0.30 ~ "red"))

###########################################################################
# Task 2: Snook CPUE & Biomass --------------------------------------------
###########################################################################

snook_stats <- dt |> 
      filter(common_name == "Snook") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      group_by(common_name, water_year, month, site, bout) |> 
      summarize(count = sum(catch_number, na.rm = TRUE),
                count_m = count/distance,
                count_100m = count_m*100,
                biomass = sum(weight_g*catch_number),
                biomass_m = biomass/distance,
                biomass_100m = biomass_m*100) |> 
      ungroup() |> 
      mutate(period = if_else(water_year >= 2018, "reporting", "baseline")) |> 
      group_by(common_name, period, water_year) |> 
      summarize(mean_wy_biomass = mean(biomass_100m),
                mean_wy_count = mean(count_100m)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(mean_rp_biomass = mean(mean_wy_biomass),
             mean_rp_period_count = mean(mean_wy_count))

snook_stats_1 <- snook_stats |> 
      mutate(baseline_biomass = 11236.623,
             baseline_count = 11.997353)

snook_stats_2 <- snook_stats_1 |> 
      group_by(water_year) |> 
      mutate(count_prop = mean_wy_count/baseline_count,
             biomass_prop = mean_wy_biomass/baseline_biomass, 
             count_light = case_when(
                   count_prop > 0.501 ~ "green",
                   count_prop >= 0.301 & count_prop <= 0.50 ~ "yellow",
                   count_prop <= 0.30 ~ "red"),
             biomass_light = case_when(
                   biomass_prop > 0.501 ~ "green",
                   biomass_prop >= 0.301 & biomass_prop <= 0.50 ~ "yellow",
                   biomass_prop <= 0.30 ~ "red"))

###########################################################################
# Task 3: Sunfish CPUE & Biomass ------------------------------------------
###########################################################################

sunfish_stats <- dt |> 
      filter(genus == "Lepomis") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      group_by(genus, water_year, month, site, bout) |> 
      summarize(count = sum(catch_number, na.rm = TRUE),
                count_m = count/distance,
                count_100m = count_m*100,
                biomass = sum(weight_g*catch_number),
                biomass_m = biomass/distance,
                biomass_100m = biomass_m*100) |> 
      ungroup() |> 
      mutate(period = if_else(water_year >= 2018, "reporting", "baseline")) |> 
      group_by(genus, period, water_year) |> 
      summarize(mean_wy_biomass = mean(biomass_100m),
                mean_wy_count = mean(count_100m)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(mean_rp_biomass = mean(mean_wy_biomass),
             mean_rp_period_count = mean(mean_wy_count))

sunfish_stats_1 <- sunfish_stats |> 
      mutate(baseline_biomass = 172.73433,
             baseline_count = 43.334478) #important to note - this includes 10 species in genus lepomis

sunfish_stats_2 <- sunfish_stats_1 |> 
      group_by(water_year) |> 
      mutate(count_prop = mean_wy_count/baseline_count,
             biomass_prop = mean_wy_biomass/baseline_biomass, 
             count_light = case_when(
                   count_prop > 0.501 ~ "green",
                   count_prop >= 0.301 & count_prop <= 0.50 ~ "yellow",
                   count_prop <= 0.30 ~ "red"),
             biomass_light = case_when(
                   biomass_prop > 0.501 ~ "green",
                   biomass_prop >= 0.301 & biomass_prop <= 0.50 ~ "yellow",
                   biomass_prop <= 0.30 ~ "red"))

###########################################################################
# Task 4: Invasive CPUE & Biomass -----------------------------------------
###########################################################################

invasive_stats <- dt |> 
      filter(status == "invasive") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      group_by(status, water_year, month, site, bout) |> 
      summarize(count = sum(catch_number, na.rm = TRUE),
                count_m = count/distance,
                count_100m = count_m*100,
                biomass = sum(weight_g*catch_number),
                biomass_m = biomass/distance,
                biomass_100m = biomass_m*100) |> 
      ungroup() |> 
      mutate(period = if_else(water_year >= 2018, "reporting", "baseline")) |> 
      group_by(status, period, water_year) |> 
      summarize(mean_wy_biomass = mean(biomass_100m),
                mean_wy_count = mean(count_100m)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(mean_rp_biomass = mean(mean_wy_biomass),
             mean_rp_period_count = mean(mean_wy_count))

invasive_stats_1 <- invasive_stats |> 
      mutate(baseline_biomass = 686.1634,
             baseline_count = 5.188148)

invasive_stats_2 <- invasive_stats_1 |> 
      group_by(water_year) |> 
      mutate(count_prop = mean_wy_count/baseline_count,
             biomass_prop = mean_wy_biomass/baseline_biomass, 
             count_light = case_when(
                   count_prop < 0.501 ~ "green",
                   count_prop <= 0.699 & count_prop >= 0.50 ~ "yellow",
                   count_prop >= 0.70 ~ "red"),
             biomass_light = case_when(
                   biomass_prop < 0.501 ~ "green",
                   biomass_prop <= 0.699 & biomass_prop >= 0.50 ~ "yellow",
                   biomass_prop >= 0.70 ~ "red"))

# join all datasets for cpue and biomass and read out ---------------------
ssr_cpue_biomass <- rbind(bass_stats_2, snook_stats_2, sunfish_stats_2, invasive_stats_2)
# writexl::write_xlsx(ssr_cpue_biomass, "data/ssr_cpue_biomass.xlsx")

###########################################################################
# Task 5: Largemouth Bass Condition ---------------------------------------
###########################################################################

bass_cond <- dt |> 
      filter(common_name == "Largemouth bass") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      filter(weight_g != 0 & standard_length != 0) |> 
      mutate(Ws = 0.01218249*standard_length^3.200356,
             Kn = weight_g/Ws) |> 
      filter(Kn <= 2 & Kn >= 0.2) |> 
      mutate(mean_Kn = mean(Kn),
             sd_Kn = sd(Kn),
             lower_bound = mean_Kn - 1.5*sd_Kn,
             upper_bound = mean_Kn + 1.5*sd_Kn) |> 
      filter(Kn >= lower_bound & Kn <= upper_bound) |> 
      group_by(common_name, water_year, month) |> 
      summarize(mean_kn = mean(Kn)) |> 
      ungroup() |> 
      mutate(period = if_else(water_year >= 2018, "reporting", "baseline")) |> 
      group_by(common_name, period, water_year) |> 
      summarize(mean_wy_kn = mean(mean_kn)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(mean_rp_kn = mean(mean_wy_kn))

bass_cond_1 <- bass_cond |> 
      mutate(baseline_kn = 1.027971)

bass_cond_2 <- bass_cond_1 |> 
      group_by(water_year) |> 
      mutate(kn_prop = (1 + mean_wy_kn)/(1 + baseline_kn),
             kn_light = case_when(
                   kn_prop > 0.501 ~ "green",
                   kn_prop >= 0.301 & kn_prop <= 0.50 ~ "yellow",
                   kn_prop <= 0.30 ~ "red"))
      
###########################################################################
# Task 6: Common Snook Condition ------------------------------------------
###########################################################################

snook_cond <- dt |> 
      filter(common_name == "Snook") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      filter(weight_g != 0 & standard_length != 0) |> 
      mutate(Ws = 0.009504406*standard_length^3.078241,
             Kn = weight_g/Ws) |> 
      filter(Kn <= 2 & Kn >= 0.2) |> 
      mutate(mean_Kn = mean(Kn),
             sd_Kn = sd(Kn),
             lower_bound = mean_Kn - 1.5*sd_Kn,
             upper_bound = mean_Kn + 1.5*sd_Kn) |> 
      filter(Kn >= lower_bound & Kn <= upper_bound) |> 
      group_by(common_name, water_year, month) |> 
      summarize(mean_kn = mean(Kn)) |> 
      ungroup() |> 
      mutate(period = if_else(water_year >= 2018, "reporting", "baseline")) |> 
      group_by(common_name, period, water_year) |> 
      summarize(mean_wy_kn = mean(mean_kn)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(mean_rp_kn = mean(mean_wy_kn))

snook_cond_1 <- snook_cond |> 
      mutate(baseline_kn = 1.025358)

snook_cond_2 <- snook_cond_1 |> 
      group_by(water_year) |> 
      mutate(kn_prop = (1 + mean_wy_kn)/(1 + baseline_kn),
             kn_light = case_when(
                   kn_prop > 0.501 ~ "green",
                   kn_prop >= 0.301 & kn_prop <= 0.50 ~ "yellow",
                   kn_prop <= 0.30 ~ "red"))

# join all datasets for condition and read out ------------------------
ssr_condition <- rbind(bass_cond_2, snook_cond_2)
# writexl::write_xlsx(ssr_condition, "data/ssr_condition.xlsx")

###########################################################################
# Task 7: Sunfish Species Richness ----------------------------------------
###########################################################################

sunfish_richness <- dt |> 
      filter(genus == "Lepomis") |> 
      filter(catch_code != "ZERO") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      group_by(water_year, month) |> 
      summarize(n_spp = n_distinct(common_name)) |> 
      ungroup() |> 
      mutate(period = if_else(water_year >= 2018, "reporting", "baseline")) |> 
      group_by(period, water_year) |> 
      summarize(mean_wy_richness = mean(n_spp)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(mean_rp_richness = mean(mean_wy_richness))

sunfish_richness_1 <- sunfish_richness |> 
      mutate(baseline_richness = 4.265263)

sunfish_richness_2 <- sunfish_richness_1 |> 
      group_by(water_year) |> 
      mutate(richness_prop = mean_wy_richness/baseline_richness,
             richness_light = case_when(
                   richness_prop > 0.501 ~ "green",
                   richness_prop >= 0.301 & richness_prop <= 0.50 ~ "yellow",
                   richness_prop <= 0.30 ~ "red"))

###########################################################################
# Task 8: Invasive Species Richness ---------------------------------------
###########################################################################

invasive_richness <- dt |> 
      filter(status == "invasive") |> 
      filter(catch_code != "ZERO") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      group_by(water_year, month) |> 
      summarize(n_spp = n_distinct(common_name)) |> 
      ungroup() |> 
      mutate(period = if_else(water_year >= 2018, "reporting", "baseline")) |> 
      group_by(period, water_year) |> 
      summarize(mean_wy_richness = mean(n_spp)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(mean_rp_richness = mean(mean_wy_richness))

invasive_richness_1 <- invasive_richness |> 
      mutate(baseline_richness = 2.411538)

invasive_richness_2 <- invasive_richness_1 |> 
      group_by(water_year) |> 
      mutate(richness_prop = mean_wy_richness/baseline_richness,
             richness_light = case_when(
                   richness_prop < 0.501 ~ "green",
                   richness_prop <= 0.699 & richness_prop >= 0.50 ~ "yellow",
                   richness_prop >= 0.70 ~ "red"))

# join all datasets for richness and read out ------------------------
ssr_richness <- rbind(sunfish_richness_2, invasive_richness_2)
# writexl::write_xlsx(ssr_richness, "data/ssr_richness.xlsx")
