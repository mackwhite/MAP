### notes
# run everything in script 'USACE-ssr-space-homog.R'
# then, just search and replace for each species/taxon group


baseline_data1 <- nonnative_homog_filter %>% 
      filter(period == "baseline") %>%
      group_by(distance) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n())) |> 
      mutate(water_year = "2005-2017")

# Calculate statistics for individual years 2018 to 2024
reporting_data1 <- nonnative_homog_filter %>% 
      filter(period == "reporting") %>%
      group_by(distance) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n())) |> 
      mutate(water_year = "2018-2024")

current_data1 <- nonnative_homog_filter %>%
      filter(water_year == 2024) %>%
      group_by(water_year, distance) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n()))

# Combine baseline and years data
baseline_data1$group <- 'Baseline'
reporting_data1$group <- 'Reporting'
current_data1$group <- '2024'
all_data1 <- rbind(baseline_data1, reporting_data1, current_data1)

ggplot(all_data1, aes(x = distance, y = mean_bm, color = group, group = group)) +
      # Add a grey ribbon for the Baseline group
      geom_ribbon(data = filter(all_data1, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      # Add a solid black line for all reporting years except 2024
      geom_line(data = filter(all_data1, group == "Reporting"), 
                color = "black", size = 1.5) +
      # Add a dashed black line for the year 2024
      geom_line(data = filter(all_data1, group == "2024"), 
                color = "black", linetype = "dashed", size = 1.5) +
      # Adjust color and fill scale for different groups
      scale_fill_manual(values = c("Baseline" = "grey80", "Reporting" = "black")) +
      labs(x = "Distance Downstream (km)",
           y = "Mean Biomass (kg/100m)",
           color = "Legend") +
      theme_classic() + 
      theme(axis.title = element_text(hjust = 0.5, face = "bold", size = 30),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 30),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 30),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 30),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))
ggsave(
      filename = "nonnative-distance.tiff",
      path = "plots/ssr2024_revisions/",
      width = 10, height = 10
)

# mean monthly biomass ----------------------------------------------------

baseline_data2 <- nonnative_homog_filter %>% 
      filter(period == "baseline") %>%
      group_by(month_chr) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n())) |> 
      mutate(water_year = "2005-2017")

# Calculate statistics for individual years 2018 to 2024
reporting_data2 <- nonnative_homog_filter %>% 
      filter(period == "reporting") %>%
      group_by(month_chr) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n())) |> 
      mutate(water_year = "2018-2024")

# Calculate statistics for water year 2024
current_data2 <- nonnative_homog_filter %>%
      filter(water_year == 2024) %>%
      group_by(water_year, month_chr) %>%
      summarise(mean_bm = mean(mean_wym_biomass, na.rm = TRUE),
                se = sd(mean_wym_biomass, na.rm = TRUE) / sqrt(n()))

# Combine baseline and years data
baseline_data2$group <- 'Baseline'
reporting_data2$group <- 'Reporting'
current_data2$group <- '2024'
all_data2 <- rbind(baseline_data2, reporting_data2, current_data2)

# Convert month to factor to ensure correct order in the plot
all_data2$month_chr <- factor(all_data2$month_chr, levels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))

ggplot(all_data2, aes(x = month_chr, y = mean_bm, color = group, group = group)) +
      # Add a grey ribbon for the Baseline group
      geom_ribbon(data = filter(all_data2, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      # Add a solid black line for all reporting years except 2024
      geom_line(data = filter(all_data2, group == "Reporting"), 
                color = "black", size = 1.5) +
      # Add a dashed black line for the year 2024
      geom_line(data = filter(all_data2, group == "2024"), 
                color = "black", linetype = "dashed", size = 1.5) +
      # Adjust color and fill scale for different groups
      scale_fill_manual(values = c("Baseline" = "grey80", "Reporting" = "black")) +
      labs(x = "Month",
           y = "Mean Biomass (kg/100m)",
           color = "Legend") +
      theme_classic() + 
      theme(axis.title = element_text(hjust = 0.5, face = "bold", size = 30),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 30),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 30),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 30),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))

ggsave(
      filename = "nonnative-month.tiff",
      path = "plots/ssr2024_revisions/",
      width = 10, height = 10
)
