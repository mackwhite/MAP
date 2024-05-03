###project: MAP
###author(s): Mack White
###goal(s): SSR plots based on conversation with JR early May 2024
###date(s): May 2024
###note(s): I have been assigned the following tasks verbatim:
#### Explore relationship between distance and cpue to see if things are shifting downstream
#### X: River km/ month in x axis​
#### Y: cpue/biomass for our groups (marine prey, moj hogs mullet, bluecrab, snapper)​
#### Ribbons for baseline, years 1-7 of reporting period​
#### Snook, bass, NN, sunfishes, marine prey [i.e., marine prey, mojarra, hogchockers, mullet, blue crab, snapper]


### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, writexl, scales, ggplot2)

dat <- read_csv("data/map_thru_042024_zerofilled.csv") |> 
      ### remove extraneous hydrologic years
      filter(!hydrologic_year %in% c("2021-2019", "2021-2020", "2021-2021"))

### left join with updated water year
water_year_ssr <- read_csv("data/hydrologic_year.csv")
dt <- left_join(dat, water_year_ssr, by = "hydrologic_year")

dt <- dt |> 
      select(-hydrologic_year) |> 
      rename(water_year = water_year_ssr)
glimpse(dt)

site_w_ds_distance <- read_csv("data/sites_with_coordinates_ds_distance.csv")
data <- left_join(dt, site_w_ds_distance)

spp_info <- read_csv("data/map_species_code_table.csv") |> 
      select(common_name, marine_prey)

data <- left_join(data, spp_info, by = "common_name")

data <- data |> 
      mutate(month_chr = factor(month, levels = c("1", "2", "3", "4", "5", "6", 
                                           "7", "8", "9", "10", "11", "12"),
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                    "Aug", "Sep", "Oct", "Nov", "Dec")),
             season = factor(month_chr, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                         labels = c("dry", "dry", "dry", "dry", "wet", "wet", "wet", "wet", "wet", 
                           "wet", "dry", "dry")))

###########################################################################
# snook homogenization figures --------------------------------------------
###########################################################################

snook_homo <- data |> 
      filter(common_name == "Snook") |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |> 
      group_by(common_name, water_year, month_chr, season, site, bout) |> 
      summarize(count = sum(catch_number, na.rm = TRUE),
                count_m = count/distance,
                count_100m = count_m*100,
                biomass = sum(weight_g*catch_number),
                biomass_m = biomass/distance,
                biomass_100m = biomass_m*100) |> 
      ungroup() |> 
      mutate(period = if_else(water_year >= 2018, "reporting", "baseline")) |> 
      group_by(common_name, period, water_year, month_chr, season, site) |> 
      summarize(mean_wym_biomass = mean(biomass_100m/1000),
                mean_wym_count = mean(count_100m)) |> 
      ungroup() |> 
      group_by(period) |> 
      mutate(mean_rp_biomass = mean(mean_wym_biomass),
             mean_rp_period_count = mean(mean_wym_count))

# write_csv(snook_homo, "../../snook_test.csv")

months_to_include <- c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

snook_homo_filter <- snook_homo |> 
      filter(month_chr %in% months_to_include)

baseline_data <- snook_homo_filter %>% 
      filter(period == "baseline") %>%
      group_by(month_chr) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n())) |> 
      mutate(water_year = "2005-2017")

# Calculate statistics for individual years 2018 to 2024
reporting_data <- snook_homo_filter %>%
      filter(period == 'reporting') %>%
      group_by(water_year, month_chr) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n()))

# Combine baseline and years data
baseline_data$group <- 'Baseline'
reporting_data$group <- as.character(reporting_data$water_year)
all_data <- rbind(baseline_data, reporting_data)

# Convert month to factor to ensure correct order in the plot
all_data$month_chr <- factor(all_data$month_chr, levels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))

##### example 1
ggplot(all_data, aes(x = month_chr, y = (mean_bm), color = group, group = group)) +
      # geom_line() +
      # geom_point() +
      # geom_errorbar(aes(ymin = mean_bm - se, ymax = mean_bm + se), width = 0.2) +
      geom_line(data = filter(all_data, group != "Baseline"), aes(color = group), size = 1.5) +
      # geom_point(data = filter(all_data, group != "Baseline"), aes(color = group)) +
      # geom_line(data = filter(all_data, group == "Baseline"), color = "black", size = 1.2) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), color = "black", alpha = 0.5) +
      scale_fill_manual(values = c("Baseline" = "grey80", "2018" = "blue", "2019" = "green", "2020" = "red", "2021" = "purple", "2022" = "orange", "2023" = "brown", "2024" = "yellow")) +
      labs(title = "Monthly Mean Snook Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Month",
           y = "Mean Biomass (kg/100m)",
           color = "Legend") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            # legend.position = c(0.2,0.7),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black")) 

ggsave(
      filename = "snook_bm_ribbon_1.tiff",
      path = "plots/",
      width = 10, height = 10
)

##### example 2
ggplot(all_data, aes(x = month_chr, y = (mean_bm), color = group, group = group)) +
      # geom_line() +
      # geom_point() +
      # geom_errorbar(aes(ymin = mean_bm - se, ymax = mean_bm + se), width = 0.2) +
      # geom_line(data = filter(all_data, group != "Baseline"), aes(color = group), size = 1.5) +
      # # geom_point(data = filter(all_data, group != "Baseline"), aes(color = group)) +
      # # geom_line(data = filter(all_data, group == "Baseline"), color = "black", size = 1.2) +
      geom_ribbon(aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = group), alpha = 0.5) +
      # scale_fill_manual(values = c("Baseline" = "grey80", "2018" = "blue", "2019" = "green", "2020" = "red", "2021" = "purple", "2022" = "orange", "2023" = "brown", "2024" = "yellow")) +
      labs(title = "Monthly Mean Snook Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Month",
           y = "Mean Biomass (kg/100m)",
           color = "Legend") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            # legend.position = c(0.2,0.7),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black")) 

ggsave(
      filename = "snook_bm_ribbon_2.tiff",
      path = "plots/",
      width = 10, height = 10
)

##### example 3
ggplot(all_data, aes(x = month_chr, y = (mean_bm), color = group, group = group)) +
      # geom_line() +
      # geom_point() +
      geom_errorbar(data = filter(all_data, group != "Baseline"), aes(ymin = mean_bm - se, ymax = mean_bm + se), width = 0.2) +
      geom_line(data = filter(all_data, group != "Baseline"), aes(color = group), size = 1.5) +
      # geom_point(data = filter(all_data, group != "Baseline"), aes(color = group)) +
      # geom_line(data = filter(all_data, group == "Baseline"), color = "black", size = 1.2) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), color = "black", alpha = 0.5) +
      scale_fill_manual(values = c("Baseline" = "grey80", "2018" = "blue", "2019" = "green", "2020" = "red", "2021" = "purple", "2022" = "orange", "2023" = "brown", "2024" = "yellow")) +
      labs(title = "Monthly Mean Snook Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Month",
           y = "Mean Biomass (kg/100m)",
           color = "Legend") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            # legend.position = c(0.2,0.7),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black")) 

ggsave(
      filename = "snook_bm_ribbon_3.tiff",
      path = "plots/",
      width = 10, height = 10
)

###### plot example four

baseline_data <- snook_homo_filter %>% 
      filter(period == "baseline") %>%
      group_by(month_chr) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n())) |> 
      mutate(water_year = "2005-2017")

reporting_data <- snook_homo_filter %>% 
      filter(period == "reporting") %>%
      group_by(month_chr) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n())) |> 
      mutate(water_year = "2018-2024")

# Combine baseline and years data
baseline_data$group <- 'Baseline'
reporting_data$group <- 'Reporting'
all_data <- rbind(baseline_data, reporting_data)

# Convert month to factor to ensure correct order in the plot
all_data$month_chr <- factor(all_data$month_chr, levels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))

ggplot(all_data, aes(x = month_chr, y = (mean_bm), color = group, group = group)) +
      # geom_line() +
      # geom_point() +
      # geom_errorbar(aes(ymin = mean_bm - se, ymax = mean_bm + se), width = 0.2) +
      # geom_line(data = filter(all_data, group != "Baseline"), aes(color = group), size = 1.5) +
      # geom_point(data = filter(all_data, group != "Baseline"), aes(color = group)) +
      # geom_line(data = filter(all_data, group == "Baseline"), color = "black", size = 1.2) +
      geom_ribbon(data = filter(all_data, group == "Baseline"),aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = group), color = "black", alpha = 0.5) +
      geom_ribbon(data = filter(all_data, group != "Baseline"),aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = group), color = "black", alpha = 0.5) +
      scale_fill_manual(values = c("Baseline" = "grey80", "Reporting" = "darkblue")) +
      labs(title = "Monthly Mean Snook Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Month",
           y = "Mean Biomass (kg/100m)",
           color = "Legend") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            # legend.position = c(0.2,0.7),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black")) 

ggsave(
      filename = "snook_bm_ribbon_4.tiff",
      path = "plots/",
      width = 10, height = 10
)
