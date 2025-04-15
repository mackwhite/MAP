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
librarian::shelf(tidyverse, readr, writexl, scales, ggplot2, ggpubr)

# read in QAQC'ed data ----------------------------------------------------
### data is zero filled as well
dat <- read_csv("data/electrofishing/year20/map_years1thru20_clean_zerofilled.csv") |> 
      ### remove extraneous hydrologic years
      filter(!hydrologic_year %in% c("2021-2019", "2021-2020", "2021-2021"))

### read out hydrologic year and code according to SSR requirements
# hydrologic_year <- dat |>
#       select(hydrologic_year) |>
#       distinct()
# write_csv(hydrologic_year, "data/hydrologic_year.csv")

### left join with updated water year
water_year_ssr <- read_csv("data/hydrologic_year.csv")
dt0 <- left_join(dat, water_year_ssr, by = "hydrologic_year")
glimpse(dt0)

dt <- dt0 |> 
      select(-hydrologic_year) |> 
      rename(water_year = water_year_ssr) |> 
      ### fix some of the wonky stuff the period of record cleaning didn't catch
      mutate(weight_g = case_when(
            common_name == "Snook" & weight_g >= 64000.000 ~ 6410.000,
            genus == "Lepomis" & weight_g >= 599 ~ 3.6,
            TRUE ~ weight_g
      ))
glimpse(dt)

write_csv(dt, 'data/map-year1thru20-zerofilled.csv')

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
writexl::write_xlsx(ssr_cpue_biomass, "data/mapannualreport2025_cpue_biomass.xlsx")

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
writexl::write_xlsx(ssr_condition, "data/mapannualreport2025_condition.xlsx")

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
writexl::write_xlsx(ssr_richness, "data/mapannualreport2025_richness.xlsx")

###########################################################################
# task 9 mesoconsumer cpue/biomass ts plots -------------------------------
###########################################################################

### generate cpue estimates ----
mc_ts <- dt |> 
      filter(common_name %in% c("Snook", "Largemouth bass")) |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      group_by(common_name, calendar_year, month, site, bout) |> 
      summarize(count = sum(catch_number, na.rm = TRUE),
                count_m = count/distance,
                count_100m = count_m*100,
                biomass = sum(weight_g*catch_number),
                biomass_m = biomass/distance,
                biomass_100m = biomass_m*100) |> 
      ungroup() |> 
      group_by(common_name, calendar_year, month, site) |> 
      summarize(cpue_abund = mean(count_100m),
                cpue_bm_g = mean(biomass_100m),
                cpue_bm_kg = cpue_bm_g/1000) |> 
      ungroup() |> 
      group_by(common_name, calendar_year, month) |> 
      summarize(cpue_abund = mean(cpue_abund),
                cpue_bm_g = mean(cpue_bm_g),
                cpue_bm_kg = mean(cpue_bm_kg)) |> 
      ungroup()

### plot cpue for abundance ----
a <- mc_ts |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      filter(common_name == "Snook") |> 
      ggplot(aes(x = s_date, y = cpue_abund, 
                 group = common_name, color = common_name)) +
      geom_line(size = 1.3) +  # Add line plot
      labs(x = "Year-Month",
           y = "Mean Monthly CPUE (#/100 m)",
           fill = "Species",
           color = "Species",
           title = "Snook") +
      theme_classic() +
      scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
      scale_color_manual(values = c("Snook" = "#003153", "Largemouth bass" = "#49796B")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      theme(axis.text = element_text(size = 8, face = "bold", colour = "black"),
            # axis.title = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_blank(),
            plot.title = element_text(size = 10, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold', size = 14),
            # legend.title = element_text(face = 'bold'),
            legend.title = element_blank())

ggsave('plots/usace-annual-report-2025/snook-abund-cpue-usace-2025-annual-report.png',
       dpi = 600, units= 'in', height = 4, width = 8)

b <- mc_ts |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      filter(common_name == "Largemouth bass") |> 
      ggplot(aes(x = s_date, y = cpue_abund, 
                 group = common_name, color = common_name)) +
      geom_line(size = 1.3) +  # Add line plot
      labs(x = "Year-Month",
           y = "Mean Monthly CPUE (#/100 m)",
           fill = "Species",
           color = "Species",
           title = "Bass") +
      theme_classic() +
      scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35), limits = c(0,36)) +
      scale_color_manual(values = c("Snook" = "#003153", "Largemouth bass" = "#49796B")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      theme(axis.text = element_text(size = 8, face = "bold", colour = "black"),
            # axis.title = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_blank(),
            plot.title = element_text(size = 10, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold', size = 14),
            # legend.title = element_text(face = 'bold'),
            legend.title = element_blank())

ggsave('plots/usace-annual-report-2025/bass-abund-cpue-usace-2025-annual-report.png',
       dpi = 600, units= 'in', height = 4, width = 8)

### plot cpue for biomass ----
e <- mc_ts |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      filter(common_name == "Snook") |> 
      ggplot(aes(x = s_date, y = cpue_bm_kg, 
                 group = common_name, color = common_name)) +
      geom_line(size = 1.3) +  # Add line plot
      labs(x = "Year-Month",
           y = "Mean Monthly CPUE (kg/100 m)",
           fill = "Species",
           color = "Species",
           title = "Snook") +
      theme_classic() +
      scale_y_continuous(breaks = c(0,5,10,15,20,25), limits = c(0,26)) +
      scale_color_manual(values = c("Snook" = "#003153", "Largemouth bass" = "#49796B")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      theme(axis.text = element_text(size = 8, face = "bold", colour = "black"),
            # axis.title = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_blank(),
            plot.title = element_text(size = 10, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold', size = 14),
            # legend.title = element_text(face = 'bold'),
            legend.title = element_blank())

ggsave('plots/usace-annual-report-2025/snook-biomass-cpue-usace-2025-annual-report.png',
       dpi = 600, units= 'in', height = 4, width = 8)

f <- mc_ts |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      filter(common_name == "Largemouth bass") |> 
      ggplot(aes(x = s_date, y = cpue_bm_kg, 
                 group = common_name, color = common_name)) +
      geom_line(size = 1.3) +  # Add line plot
      labs(x = "Year-Month",
           y = "Mean Monthly CPUE (kg/100 m)",
           fill = "Species",
           color = "Species",
           title = "Bass") +
      theme_classic() +
      scale_y_continuous(breaks = c(0,5,10,15,20), limits = c(0,20)) +
      scale_color_manual(values = c("Snook" = "#003153", "Largemouth bass" = "#49796B")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      theme(axis.text = element_text(size = 8, face = "bold", colour = "black"),
            # axis.title = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_blank(),
            plot.title = element_text(size = 10, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold', size = 14),
            # legend.title = element_text(face = 'bold'),
            legend.title = element_blank())

ggsave('plots/usace-annual-report-2025/bass-biomass-cpue-usace-2025-annual-report.png',
       dpi = 600, units= 'in', height = 4, width = 8)
###########################################################################
# task 10 prey-invasive cpue/biomass ts plots -----------------------------
###########################################################################
### generate cpue estimates ----
sf_ts <- dt |> 
      filter(genus == "Lepomis") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      group_by(genus, calendar_year, month, site, bout) |> 
      summarize(count = sum(catch_number, na.rm = TRUE),
                count_m = count/distance,
                count_100m = count_m*100,
                biomass = sum(weight_g*catch_number),
                biomass_m = biomass/distance,
                biomass_100m = biomass_m*100) |> 
      ungroup() |> 
      group_by(genus, calendar_year, month, site) |> 
      summarize(cpue_abund = mean(count_100m),
                cpue_bm_g = mean(biomass_100m),
                cpue_bm_kg = cpue_bm_g/1000) |> 
      ungroup() |> 
      group_by(genus, calendar_year, month) |> 
      summarize(cpue_abund = mean(cpue_abund),
                cpue_bm_g = mean(cpue_bm_g),
                cpue_bm_kg = mean(cpue_bm_kg)) |> 
      ungroup() |> 
      mutate(prey = genus) |> 
      select(-genus)

inv_ts <- dt |> 
      filter(status == "invasive") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      group_by(status, calendar_year, month, site, bout) |> 
      summarize(count = sum(catch_number, na.rm = TRUE),
                count_m = count/distance,
                count_100m = count_m*100,
                biomass = sum(weight_g*catch_number),
                biomass_m = biomass/distance,
                biomass_100m = biomass_m*100) |> 
      ungroup() |> 
      group_by(status, calendar_year, month, site) |> 
      summarize(cpue_abund = mean(count_100m),
                cpue_bm_g = mean(biomass_100m),
                cpue_bm_kg = cpue_bm_g/1000) |> 
      ungroup() |> 
      group_by(status, calendar_year, month) |> 
      summarize(cpue_abund = mean(cpue_abund),
                cpue_bm_g = mean(cpue_bm_g),
                cpue_bm_kg = mean(cpue_bm_kg)) |> 
      ungroup() |> 
      mutate(prey = status) |> 
      select(-status)

### join sunfish and nonnative cpue estimates ----
glimpse(sf_ts)
glimpse(inv_ts)
sf_inv_ts <- rbind(sf_ts, inv_ts)
glimpse(sf_inv_ts)

### generate abundance time series ----
c <- sf_inv_ts |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      filter(prey == "Lepomis") |> 
      mutate(common_name = case_when(
            prey == "Lepomis" ~ "Sunfishes"
      )) |> 
      ggplot(aes(x = s_date, y = cpue_abund, 
                 group = common_name, color = common_name)) +
      geom_line(size = 1.3) +  # Add line plot
      labs(x = "Year-Month",
           y = "Mean Monthly CPUE (#/100 m)",
           fill = "Species",
           color = "Species",
           title = "Sunfishes") +
      theme_classic() +
      scale_y_continuous(breaks = c(0,25,50,75,100,125), limits = c(0,133)) +
      scale_color_manual(values = c("Sunfishes" = "#6C5B7B", "Non-natives" = "#A44A3F")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      theme(axis.text = element_text(size = 8, face = "bold", colour = "black"),
            # axis.title = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_blank(),
            plot.title = element_text(size = 10, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold', size = 14),
            # legend.title = element_text(face = 'bold'),
            legend.title = element_blank())

ggsave('plots/usace-annual-report-2025/sunfish-abund-cpue-usace-2025-annual-report.png',
       dpi = 600, units= 'in', height = 4, width = 8)

d <- sf_inv_ts |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      filter(prey == "invasive") |> 
      mutate(common_name = case_when(
            prey == "invasive" ~ "Non-natives"
      )) |> 
      ggplot(aes(x = s_date, y = cpue_abund, 
                 group = common_name, color = common_name)) +
      geom_line(size = 1.3) +  # Add line plot
      labs(x = "Year-Month",
           y = "Mean Monthly CPUE (#/100 m)",
           fill = "Species",
           color = "Species",
           title = "Non-natives") +
      theme_classic() +
      scale_y_continuous(breaks = c(0,15,30,45,60,75), limits = c(0,77)) +
      scale_color_manual(values = c("Sunfishes" = "#6C5B7B", "Non-natives" = "#A44A3F")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      theme(axis.text = element_text(size = 8, face = "bold", colour = "black"),
            # axis.title = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_blank(),
            plot.title = element_text(size = 10, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold', size = 14),
            # legend.title = element_text(face = 'bold'),
            legend.title = element_blank())

ggsave('plots/usace-annual-report-2025/nonnative-abund-cpue-usace-2025-annual-report.png',
       dpi = 600, units= 'in', height = 4, width = 8)

### generate biomass timeseries ----
g <- sf_inv_ts |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      filter(prey == "Lepomis") |> 
      mutate(common_name = case_when(
            prey == "Lepomis" ~ "Sunfishes"
      )) |> 
      ggplot(aes(x = s_date, y = cpue_bm_kg, 
                 group = common_name, color = common_name)) +
      geom_line(size = 1.3) +  # Add line plot
      labs(x = "Year-Month",
           y = "Mean Monthly CPUE (kg/100 m)",
           fill = "Species",
           color = "Species",
           title = "Sunfishes") +
      theme_classic() +
      # scale_y_continuous(breaks = c(0,25,50,75,100,125), limits = c(0,133)) +
      scale_color_manual(values = c("Sunfishes" = "#6C5B7B", "Non-natives" = "#A44A3F")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      theme(axis.text = element_text(size = 8, face = "bold", colour = "black"),
            # axis.title = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_blank(),
            plot.title = element_text(size = 10, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold', size = 14),
            # legend.title = element_text(face = 'bold'),
            legend.title = element_blank())

ggsave('plots/usace-annual-report-2025/sunfish-biomass-cpue-usace-2025-annual-report.png',
       dpi = 600, units= 'in', height = 4, width = 8)

h <- sf_inv_ts |> 
      mutate(s_date = make_date(year = calendar_year, month = month, day = 1)) |> 
      filter(prey == "invasive") |> 
      mutate(common_name = case_when(
            prey == "invasive" ~ "Non-natives"
      )) |> 
      ggplot(aes(x = s_date, y = cpue_bm_kg, 
                 group = common_name, color = common_name)) +
      geom_line(size = 1.3) +  # Add line plot
      labs(x = "Year-Month",
           y = "Mean Monthly CPUE (kg/100 m)",
           fill = "Species",
           color = "Species",
           title = "Non-natives") +
      theme_classic() +
      scale_y_continuous(breaks = c(0,2,4,6,8,10,12), limits = c(0,12.5)) +
      scale_color_manual(values = c("Sunfishes" = "#6C5B7B", "Non-natives" = "#A44A3F")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      theme(axis.text = element_text(size = 8, face = "bold", colour = "black"),
            # axis.title = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_blank(),
            plot.title = element_text(size = 10, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold', size = 14),
            # legend.title = element_text(face = 'bold'),
            legend.title = element_blank())
h

ggsave('plots/usace-annual-report-2025/nonnative-biomass-cpue-usace-2025-annual-report.png',
       dpi = 600, units= 'in', height = 4, width = 8)

keep <- c("a", "b", "c", "d", "e", "f", "g", "h", "sf_inv_ts", "mc_ts")
rm(list = setdiff(ls(), keep))

abund_arranged <- ggarrange(a,b,c,d, nrow = 4, align = "v", 
                           labels = c("a", "b", "c", "d"),
                           label.y = 1.025)
abund_arranged

abund_labeled <- annotate_figure(
      abund_arranged,
      left = text_grob("Mean Monthly Abundance CPUE (#/100m)", rot = 90, vjust = 1, size = 14, face = "bold"),
      bottom = text_grob("Year-Month", size = 14, face = "bold")
)
abund_labeled

ggsave('plots/usace-annual-report-2025/abundance-four-panel-usace-annual-2025.png',
       dpi = 600, units= 'in', height = 8, width = 4)

bm_arranged <- ggarrange(e,f,g,h, nrow = 4, align = "v", 
                            labels = c("a", "b", "c", "d"),
                            label.y = 1.025)
bm_arranged

bm_labeled <- annotate_figure(
      bm_arranged,
      left = text_grob("Mean Monthly Abundance CPUE (kg/100m)", rot = 90, vjust = 1, size = 14, face = "bold"),
      bottom = text_grob("Year-Month", size = 14, face = "bold")
)
bm_labeled

ggsave('plots/usace-annual-report-2025/biomass-four-panel-usace-annual-2025.png',
       dpi = 600, units= 'in', height = 8, width = 4)
