###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): calculate summary stats needed for analysis/figures
###date(s): January - March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, e1071)

### set google drive path
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) |> 
  dplyr::filter(name %in% c("harmonized_consumer_excretion_CLEAN.csv"))

strata_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1CEgNtAnk4DuPNpR3lJN9IqpjWq0cM8F4")) %>%
  dplyr::filter(name %in% c("strata_class.xlsx"))

# Combine file IDs
harmonized_ids <- rbind(exc_ids, strata_ids)

# For each raw data file, download it into the consumer folder
for(k in 1:nrow(harmonized_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = harmonized_ids[k, ]$id, overwrite = T,
                                path = file.path("tier2", harmonized_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(harmonized_ids))
}

rm(list = ls()) #cleans env


### read in clean excretion and strata data from google drive
dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion_CLEAN.csv"),stringsAsFactors = F,na.strings =".") |> 
  janitor::clean_names()
glimpse(dt)

strata_list <- readxl::read_excel(path = file.path("tier2", "strata_class.xlsx"),na=".") |> 
  ### remove decimals from numbered sites
  mutate(site = str_remove(site, "\\.0$"),
         subsite_level1 = str_remove(subsite_level1, "\\.0$"),
         subsite_level2 = str_remove(subsite_level2, "\\.0$"),
         subsite_level3 = str_remove(subsite_level3, "\\.0$"))

# set up data for summary statistics --------------------------------------

### replace NAs in subsite_level2 and subsite_level3 columns with "Not Available"
### to allow group_by function to go as far in sequence as it can for 
### each project without throwing NAs

dt <- dt %>%
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

### check to see NA fixes incorporated
na_count_per_column <- sapply(dt, function(x) sum(is.na(x)))
print(na_count_per_column)


fce <- dt |> 
  filter(project == "FCE") |> 
      mutate(wyear = if_else(project == "FCE" & month < 10, year - 1, year))

strata_list <- strata_list %>%
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

fce_dt <- left_join(fce, strata_list, 
                             by = c("project", "habitat", "site",
                                    "subsite_level1", "subsite_level2",
                                    "subsite_level3"))

spp <- (unique(fce$common_name))
# write_csv(spp, "../../../fce_spp.csv")
spp_fce <- read_csv("../../../fce_spp.csv")
dt <- left_join(fce_dt, spp_fce)

# split data into groups of interest --------------------------------------

snook <- dt |> 
      filter(common_name == "Snook") |> 
      unite(site, site, subsite_level1, sep = "-") |> 
      rename(bout = subsite_level2) |> 
      dplyr::select(-subsite_level3) |> 
      filter(site %in% c("RB-8", "RB-9", "RB-10", "RB-11", "RB-13")) |> 
      group_by(common_name, wyear, month, site, bout) |> 
      summarize(total_bm_m = sum(dmperind_g_ind*density_num_m, na.rm = TRUE),
                total_bm_100m = total_bm_m*100,
                count_m = density_num_m,
                count_100m = count_m*100) |> 
      ungroup() |> 
      mutate(period = if_else(wyear >= 2017, "reporting", "baseline")) |> 
      group_by(common_name, period, wyear) |> 
      summarize(bm = mean(total_bm_100m),
                count = mean(count_100m)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(bm_avg = mean(bm),
             count_avg = mean(count))

snook1 <- snook |> 
      mutate(bl_bm = 2681.612,
             bl_count = 0.9067856)

snook2 <- snook1 |> 
      group_by(wyear) |> 
      mutate(count_prop = count/bl_count,
             bm_prop = bm/bl_bm, 
             count_light = case_when(
                   count_prop > 0.501 ~ "green",
                   count_prop >= 0.301 & count_prop <= 0.50 ~ "yellow",
                   count_prop <= 0.30 ~ "red"),
             biomass_light = case_when(
                   bm_prop > 0.501 ~ "green",
                   bm_prop >= 0.301 & bm_prop <= 0.50 ~ "yellow",
                   bm_prop <= 0.30 ~ "red"))

# snook_ss <- dt |> 
#       filter(common_name == "Snook",
#              !(dmperind_g_ind == 0)) |> 
#       mutate(period = if_else(wyear >= 2017, "reporting", "baseline")) |> 
#       group_by(common_name, period, wyear) |> 
#       summarize(ss = mean(skewness(dmperind_g_ind))) |> 
#       ungroup() |> 
#       group_by(common_name, period) |> 
#       mutate(ss_avg = mean(ss, na.rm = TRUE))
# 
# snook_ss1 <- snook_ss |> 
#       mutate(bl_ss = 2.485220)
# 
# snook_ss2 <- snook_ss1 |> 
#       group_by(wyear) |> 
#       mutate(ss_prop = ss/bl_ss,
#              count_light = case_when(
#                    ss_prop <= 1.10 & ss_prop >= 0.90 ~ "green",
#                    ss_prop >= 1.101 & ss_prop <= 1.20 ~ "yellow",
#                    ss_prop <= 0.899 & ss_prop >= 0.80 ~ "yellow",
#                    ss_prop <= 0.799 ~ "red", 
#                    ss_prop >= 1.21 ~ "red"))

# bass calcs --------------------------------------------------------------

bass <- dt |> 
      filter(common_name == "Largemouth bass") |> 
      unite(site, site, subsite_level1, sep = "-") |> 
      rename(bout = subsite_level2) |> 
      dplyr::select(-subsite_level3) |> 
      filter(site %in% c("RB-8", "RB-9", "RB-10", "RB-11", "RB-13")) |> 
      group_by(common_name, wyear, month, site, bout) |> 
      summarize(total_bm_m = sum(dmperind_g_ind*density_num_m, na.rm = TRUE),
                total_bm_100m = total_bm_m*100,
                count_m = density_num_m,
                count_100m = count_m*100) |> 
      ungroup() |> 
      mutate(period = if_else(wyear >= 2017, "reporting", "baseline")) |> 
      group_by(common_name, period, wyear) |> 
      summarize(bm = mean(total_bm_100m),
                count = mean(count_100m)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(bm_avg = mean(bm),
             count_avg = mean(count))

bass1 <- bass |> 
      mutate(bl_bm = 1603.8623,
             bl_count = 1.093978)

bass2 <- bass1 |> 
      group_by(wyear) |> 
      mutate(count_prop = count/bl_count,
             bm_prop = bm/bl_bm, 
             count_light = case_when(
                   count_prop > 0.501 ~ "green",
                   count_prop >= 0.301 & count_prop <= 0.50 ~ "yellow",
                   count_prop <= 0.30 ~ "red"),
             biomass_light = case_when(
                   bm_prop > 0.501 ~ "green",
                   bm_prop >= 0.301 & bm_prop <= 0.50 ~ "yellow",
                   bm_prop <= 0.30 ~ "red"))

# bass_ss <- dt |> 
#       filter(common_name == "Largemouth bass",
#              !(dmperind_g_ind == 0)) |> 
#       mutate(period = if_else(wyear >= 2017, "reporting", "baseline")) |> 
#       group_by(common_name, period, wyear) |> 
#       summarize(ss = mean(skewness(dmperind_g_ind))) |> 
#       ungroup() |> 
#       group_by(common_name, period) |> 
#       mutate(ss_avg = mean(ss, na.rm = TRUE))
# 
# bass_ss1 <- bass_ss |> 
#       mutate(bl_ss = 2.375704)
# 
# bass_ss2 <- bass_ss1 |> 
#       group_by(wyear) |> 
#       mutate(ss_prop = ss/bl_ss,
#              count_light = case_when(
#                    ss_prop <= 1.10 & ss_prop >= 0.90 ~ "green",
#                    ss_prop >= 1.101 & ss_prop <= 1.20 ~ "yellow",
#                    ss_prop <= 0.899 & ss_prop >= 0.80 ~ "yellow",
#                    ss_prop <= 0.799 ~ "red", 
#                    ss_prop >= 1.21 ~ "red"))

# invasive calcs ----------------------------------------------------------

invasive <- dt |> 
      filter(origin == "invasive") |> 
      unite(site, site, subsite_level1, sep = "-") |> 
      rename(bout = subsite_level2) |> 
      dplyr::select(-subsite_level3, -common_name) |> 
      rename(common_name = origin) |> 
      filter(site %in% c("RB-8", "RB-9", "RB-10", "RB-11", "RB-13")) |> 
      group_by(common_name, wyear, month, site, bout) |> 
      summarize(total_bm_m = sum(dmperind_g_ind*density_num_m, na.rm = TRUE),
                total_bm_100m = total_bm_m*100,
                count_m = density_num_m,
                count_100m = count_m*100) |> 
      ungroup() |> 
      mutate(period = if_else(wyear >= 2017, "reporting", "baseline")) |> 
      group_by(common_name, period, wyear) |> 
      summarize(bm = mean(total_bm_100m),
                count = mean(count_100m)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(bm_avg = mean(bm),
             count_avg = mean(count))

invasive1 <- invasive |> 
      mutate(bl_bm = 369.4822,
             bl_count = 0.1616202)

invasive2 <- invasive1 |> 
      group_by(wyear) |> 
      mutate(count_prop = count/bl_count,
             bm_prop = bm/bl_bm, 
             count_light = case_when(
                   count_prop < 0.501 ~ "green",
                   count_prop <= 0.699 & count_prop >= 0.50 ~ "yellow",
                   count_prop >= 0.70 ~ "red"),
             biomass_light = case_when(
                   bm_prop < 0.501 ~ "green",
                   bm_prop <= 0.699 & bm_prop >= 0.50 ~ "yellow",
                   bm_prop >= 0.70 ~ "red"))


df <- rbind(snook2, bass2, invasive2) |>
      arrange(wyear)

### Snook Test

bm_heatmap_data <- snook2 %>%
      select(wyear, biomass_light, count_light) %>%
      pivot_longer(cols = c(biomass_light, count_light), 
                   names_to = "category", values_to = "light")
      # pivot_wider(names_from = wyear, values_from = light)

ggplot(bm_heatmap_data, aes(x = wyear, y = category)) +
      geom_tile(aes(fill = light), color = "white") +
      scale_fill_manual(values = c("green" = "#00806b", 
                                   "yellow" = "#c2a829", 
                                   "red" = "#850d0d")) +
      theme_minimal() +
      labs(fill = "Light Category", x = "Year", y = "Metric") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

### All Test

bm_heatmap_data_all <- df %>%
      select(common_name, wyear, biomass_light, count_light) |> 
      pivot_longer(cols = c(biomass_light, count_light), names_to = "category", values_to = "light") %>%
      # pivot_wider(names_from = wyear, values_from = light, names_prefix = "Year_") %>%
      arrange(common_name, category)  # Make sure common_name is not repeated


bm_heatmap_data_all <- bm_heatmap_data_all %>%
      mutate(common_name = fct_recode(common_name,
                                      `Invasive Spp.` = "invasive",
                                      `Florida Largemouth Bass` = "Largemouth bass",
                                      `Common Snook` = "Snook")) |> 
      rename(Abundace = count_light,
             Biomass = biomass_light)

# Generate the heatmap for biomass_light
ggplot(bm_heatmap_data_all, aes(x = as.factor(wyear), y = category)) +
      geom_tile(aes(fill = light), color = "white") +
      scale_fill_manual(values = c("green" = "#00806b", 
                                   "yellow" = "#c2a829", 
                                   "red" = "#850d0d")) +
      theme_minimal() +
      facet_wrap(~common_name) +
      labs(fill = "Stoplight Rank", x = "Hydrologic Year: Dry Season", y = "Metric") +
      theme(axis.text = element_text(size = 14, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, size = 14, face = "bold"),
            plot.title = element_text(size = 14, face = "bold"),
            strip.text = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 14, face = "bold"),
            legend.title = element_text(size = 14, face = "bold"))

ggsave("plots/heatmap_plot_example.tiff",
       width = 21, height = 7, dpi = 300,
       bg = "white")



